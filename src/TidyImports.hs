module TidyImports where

import Control.Monad
import Data.Char
import Data.List
import System.Exit
import System.IO
import System.Process
import Text.Printf


version :: String
version = "0.1.0.1" -- 2017/02/13 (leave Debug.Trace)
-- version = "0.1.0.0" -- 2016/12/02

usage =
  "Tim's Haskell Source Tidier (v" ++ version ++ ")\n" ++
  "This program deletes unused imports from a Haskell project.\n" ++
  "\n" ++
  "usage: thstidy OPTS MODULE\n" ++
  "where OPTS = \n" ++
  "  -c    --cabal         calls cabal repl --ghc-options=\"-Wunused-imports\" to generate\n" ++
  "                        the unused import list (do not pass a module)\n" ++
  "  -d    --dry-run       does not actual modify the files, just renders the changes\n" ++
  "  -v    --verbose       verbose/quiet output\n" ++
  "  -q    --quiet\n" ++
  "\n" ++
  "EXAMPLES:\n" ++
  " % thstidy Main\n" ++
  "     * Removes unused imports from all modules reachable from Main\n" ++
  " % thstidy Main -d\n" ++
  "     * As above, but doesn't actually commit the changes\n" ++
  " % thstidy Foo/Bar/Baz/Module.hs\n" ++
  "     * Removes unused imports from all modules reachable from Foo/Bar/Baz/Module.hs\n" ++
  " % thstidy -c\n" ++
  "     * Uses cabal repl ... to generate the warnings" ++
  ""

data Opts =
  Opts {
    oModule :: !String
  , oCabal :: !Bool
  , oKeepDebugTrace :: !Bool
  , oDryRun :: !Bool
  , oVerbosity :: !Int
  } deriving Show
dftOpts =
  Opts {
    oModule = ""
  , oCabal = False
  , oKeepDebugTrace = True
  , oDryRun = False
  , oVerbosity = 0
  }

oQuiet :: Opts -> Bool
oQuiet = (<0) . oVerbosity
oVerbose :: Opts -> Bool
oVerbose = (>0) . oVerbosity

run :: [String] -> IO ()
run as = parseOpts dftOpts as >>= runWithOpts

parseOpts :: Opts -> [String] -> IO Opts
parseOpts os []
  | not (null (oModule os)) && oCabal os = fatal "may not specify module name with -c option"
  | null (oModule os) && not (oCabal os) = fatal "expected module name to load (e.g. Main) (try -h)\n"
  | otherwise = return os
parseOpts os (a:as)
  | a `elem` ["-h","--help"] = putStrLn usage >> exitSuccess
  | a `elem` ["-d","--dry-run"] = parseOpts (os{oDryRun = True}) as
  | a `elem` ["-c","--cabal"] = parseOpts (os{oCabal = True}) as
  | a `elem` ["-v","--verbose"] = parseOpts (os{oVerbosity = 1}) as
  | a `elem` ["-q","--quiet"] = parseOpts (os{oVerbosity = -1}) as
  | head a == '-' = badArg "invalid option"
  | null (oModule os) = parseOpts (os{oModule = a}) as
  | otherwise = badArg ("argument already specified as " ++ oModule os)
  where badArg msg = fatal $ a ++ ": " ++ msg ++ "\n"

fatal :: String -> IO a
fatal = die

runWithOpts :: Opts -> IO ()
runWithOpts os = do
  let (exe,args)
        | oCabal os = ("cabal",["repl","--ghc-options=-Wunused-imports"])
        | otherwise = ("ghci",[oModule os,"-Wunused-imports"])
  when (oVerbose os) $ do
    putStrLn "To collect warnings we are running:"
    putStrLn $ "% " ++ exe ++ "  " ++ intercalate "  " args
  (ec,out,err) <- readProcessWithExitCode exe args  ""
  case ec of
    ExitSuccess -> processUnusedImports os err
    _ -> do
      putStrLn "ghci Main failed"
      exitFailure


processUnusedImports :: Opts -> String -> IO ()
processUnusedImports os err_output = body
  where body = do
          when (oVerbose os) $ do
            putStrLn "************************************* GHC WARNINGS"
            when (not (null err_output)) $
              putStrLn $ err_output
          let sfs = parseSrcFixes (lines err_output)
              fs = sort $ nub (map sfFile sfs)
          when (oVerbose os) $ do
            putStrLn "************************************* PARSED WARNINGS"
            mapM_ print sfs
          mapM_ (applyToFile sfs) fs
          when (not (oQuiet os)) $ do
            putStrLn "*************************************"
            let plural n xs = show (length xs) ++ if (length xs) == 1 then n else (n ++ "s")
                pluralE n xs = show (length xs) ++ if (length xs) == 1 then n else (n ++ "es")
            putStrLn $ "applied " ++ pluralE " fix" sfs ++ " to " ++ plural " file" sfs

        applyToFile :: [SrcFix] -> FilePath -> IO ()
        applyToFile all_sfs fp = do
          let sfs = sortOn sfLine $ filter ((== fp) . sfFile) all_sfs
          putStrLn $ "************* APPLYING TO " ++ fp ++ "*************"
          fstr <- readFile fp
          length fstr `seq` return ()
        --  mapM_ print sfs
          let (fixed_file,fxs) = deleteLines os sfs (lines fstr)
          forM_ fxs $ \(lno,ln,new_ln) -> do
            when (not (oQuiet os)) $ do
              putStrLn ("<" ++ printf "%5d" lno ++ ". " ++ ln)
              when (not (null new_ln)) $
                putStrLn (">" ++ printf "%5d" lno ++ ". " ++ new_ln)
          when (not (oDryRun os)) $
            withBinaryFile fp WriteMode $ \h -> hPutStr h fixed_file
          -- exitSuccess

data SrcFix =
  SrcFix {
    sfFile   :: !FilePath
  , sfLine   :: !Int
  , sfSymbols :: ![String]
  } deriving Show
 -- The import of `*>, <$>' from module

parseSrcFixes :: [String] -> [SrcFix]
parseSrcFixes [] = []
parseSrcFixes [_] = []
parseSrcFixes (ln0:ln1:lns)
  | is_unused_import =
    -- something of the form:
    -- ln0: -- src\Intel\Gen\ISA\Parser.hs:19:1: warning: [-Wunused-imports]
    -- ln1: ...
    -- lns: ...
    case span (/=':') ln0 of
      -- :18:1: warning
      (path,(':':sfx)) ->
        case span (/=':') sfx of
          (lno_str,(':':'1':':':_)) ->
            case reads lno_str of
              [(lno,"")] -> classifyResult path lno : parseSrcFixes lns
              _ -> noMatch
          _ -> noMatch
      _ -> noMatch
  | otherwise = noMatch
  -- src\Intel\CLasm\FormatProgram.hs:18:1: warning: [-Wunused-imports]
  where noMatch = parseSrcFixes (ln1:lns)

        classifyResult path lno
          | " from module " `isInfixOf` ln1 || "from module" `isInfixOf` head lns =
            SrcFix path lno syms
          | otherwise =
            SrcFix path lno []
          -- The import of `<?>' from module `Text.Parsec' is redundant
          --
          -- The import of `*>, <$>'
          -- from module `Control.Applicative' is redundant
          where raw_syms = takeWhile (/='\'') (drop 1 (dropWhile (/='`') ln1))
                syms = words (map (\c -> if c == ',' then ' ' else c) raw_syms)

                fixSym s -- <?> -> (<?>)
                  | isAlpha (head s) = s
                  | otherwise        = "(" ++ s ++ ")"

        is_unused_import = "warning: [-Wunused-imports]"`isSuffixOf`ln0
        -- want to avoid partial imports
        -- src\Intel\Gen\ISA\Parser.hs:19:1: warning: [-Wunused-imports]
        -- The import of `<?>' from module `Text.Parsec' is redundant
        --
        -- import Text.Parsec((<|>),(<?>))
        --                     ^^^ keep this one
        explicit_token_import =
          not (null (take 1 lns)) && False
        -- The import of `padL' from module `Intel.Gen.Util.Text' is redundant
        -- "    The import of `<?>' from module `Text.Parsec' is redundant"--
        -- "    The import of `Data.Char' is redundant"

unlinesUnix :: [String] -> String
unlinesUnix = concatMap (++"\n")

toy = deleteLines dftOpts toy_fixes toy_file
toy_fixes = [SrcFix {sfFile = "ToyFile.hs", sfLine = 2, sfSymbols = ["(<$>)"]}]
toy_file = lines $
  "import Intel.Gen.Util.Text(ppChar)\n" ++
  "import Control.Applicative((<$>))\n" ++
  "\n" ++
  ""

deleteLines :: Opts -> [SrcFix] -> [String] -> (String,[(Int,String,String)])
deleteLines os rmlnos = loop [] [] rmlnos 1
  where loop :: [String] ->
                [(Int,String,String)] ->
                [SrcFix] ->
                Int ->
                [String] ->
                (String,[(Int,String,String)])
        loop rlns rfxs []       _   lns = (unlinesUnix (reverse rlns ++ lns),reverse rfxs)
        loop rlns rfxs (sf:sfs) lno (ln:lns)
          | sfLine sf < lno  = error "deleteLines: out of sync"
          | sfLine sf == lno && null (sfSymbols sf) =
            if "Debug.Trace" `isInfixOf` ln && oKeepDebugTrace os
              then loop rlns ((lno,ln,"-- " ++ ln):rfxs) sfs      (lno + 1) lns
              else -- full line removal (remove a module import)
                    -- import Data.List => drop the line
                    loop rlns ((lno,ln,""):rfxs) sfs      (lno + 1) lns
          | sfLine sf == lno =
            case tokenizeImportStatement sf ln of
              (imp_pfx,imp_args,imp_sfx)
                | length imp_args == length (sfSymbols sf) ->
                    -- explicit import where we remove all members;
                    --   import Data.List(intercalate,find) =>
                    --   import Data.List() => drop the line
                    loop rlns ((lno,ln,""):rfxs) sfs (lno + 1) lns
                | otherwise ->
                    -- import Data.List(intercalate,find) =>
                    -- import Data.List(find) => retain the line
                    loop rlns ((lno,ln,new_line):rfxs) sfs (lno + 1) lns -- remove just that symbol
                where new_args = filter (not . (`elem`sfSymbols sf)) imp_args
                      new_line = imp_pfx ++ "(" ++ intercalate ", " new_args ++ ")" ++ imp_sfx
            -- symbol removal, e.g. "(<?>)" from "import Text.Parsec((<|>),(<?>),padR)"
          | otherwise        = loop (ln:rlns) rfxs            (sf:sfs) (lno + 1) lns -- retain line
        loop _    _    _              _   [] =
          error "deleteLines: unhandled lines"

-- only called on imports that require hard tokenization
--  import Foo(***)
--             ^^^
-- "import   Foo(baz,bar) -- comment" -> ("import   Foo",["bar","baz"]," -- comment")
-- "(<|>),(<?>),padR)" -> ["(<|>)","(<?>)","padR"]
tokenizeImportStatement :: SrcFix -> String -> (String,[String], String)
tokenizeImportStatement sf imp_str
  | not ("import" `isPrefixOf` imp_str) = err "no import found"
  | otherwise =
    case span (/='(') imp_str of
      (pfx,('(':sfx)) -> nextSym [] sfx
        where -- end of the imports
          nextSym rsyms (')':sfx) = (pfx, reverse rsyms, sfx)
          -- skip the comma; on to the next symbol
          nextSym rsyms (',':sfx) = nextSym rsyms sfx
          -- skip spacing
          nextSym rsyms (c:sfx) | isSpace c = nextSym rsyms sfx
          -- parenthetical symbol e.g. "(<|>)"
          nextSym rsyms ('(':sfx) =
            case span (/=')') sfx of
              (sym,')':sfx) -> nextSym (new_sym:rsyms) (dropWhile isSpace sfx)
                where new_sym = "(" ++ sym ++ ")"
          -- identifier symbol: e.g. "foo_bar"
          nextSym rsyms (c:sfx)
            | isSpace c = nextSym rsyms sfx
            | otherwise =
              case span (\c -> not (c`elem`"),")) (c:sfx) of
                (sym,sfx) -> nextSym (sym:rsyms) sfx
          nextSym rsyms [] = err "end of line before ) found"
      _ -> err "no '(' found"
  where err msg = error $
                    sfFile sf ++ ". " ++ show (sfLine sf) ++ ". malformed import line: " ++ msg ++ "\n" ++
                    imp_str ++ "\n" ++
                    show sf ++ "\n" ++
                    ""
