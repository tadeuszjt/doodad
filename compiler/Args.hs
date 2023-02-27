module Args where

data Args = Args
    { verbose     :: Bool
    , optimise    :: Bool
    , astOnly     :: Bool
    , lexOnly     :: Bool
    , printLLIR   :: Bool
    , printCImports :: Bool
    , compileObj  :: Bool
    , printTokens :: Bool
    , printAst    :: Bool
    , printAstResolved :: Bool
    , printAstAnnotated :: Bool
    , printSymbols :: Bool
    , printCSymbols :: Bool
    , printAstFinal :: Bool
    , printIR :: Bool
    , useOldLexer :: Bool
    , modPaths    :: [String]
    }

initArgs = Args
    { verbose   = False
    , optimise  = True
    , astOnly   = False
    , lexOnly   = False
    , printLLIR = False
    , compileObj = False
    , printTokens = False
    , printCImports = False
    , printAst = False
    , printAstResolved = False
    , printAstAnnotated = False
    , printSymbols = False
    , printCSymbols = False
    , printAstFinal = False
    , printIR = False
    , useOldLexer = False
    , modPaths  = []
    }


parseArgs :: Args -> [String] -> Args
parseArgs args argStrs = case argStrs of
    []     -> args
    ["-n"] -> args { optimise  = False }
    ["-a"] -> args { astOnly   = True }
    ["-l"] -> args { lexOnly   = True }
    ["--verbose"] -> args { verbose   = True }
    ["--print-llir"] -> args { printLLIR = True }
    ["--print-tokens"] -> args { printTokens = True }
    ["--print-c"] -> args { printCImports = True }
    ["--print-c-symbols"] -> args { printCSymbols = True }
    ["--print-ast"] -> args { printAst = True }
    ["--print-ast-resolved"] -> args { printAstResolved = True }
    ["--print-ast-annotated"] -> args { printAstAnnotated = True }
    ["--print-ast-final"] -> args { printAstFinal = True }
    ["--print-symbols"] -> args { printSymbols = True }
    ["--print-ir"] -> args { printIR = True }
    ["--use-old-lexer"] -> args { useOldLexer = True }
    ["-c"] -> args { compileObj = True }
    [str]  -> args { modPaths  = (modPaths args) ++ [str] }
    (a:as) -> parseArgs (parseArgs args [a]) as

