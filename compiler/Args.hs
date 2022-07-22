module Args where

data Args = Args
    { verbose     :: Bool
    , optimise    :: Bool
    , astOnly     :: Bool
    , lexOnly     :: Bool
    , printLLIR   :: Bool
    , printCImports :: Bool
    , compileObj  :: Bool
    , printAst    :: Bool
    , printSymbols :: Bool
    , printCSymbols :: Bool
    , printFinalAst :: Bool
    , modPaths    :: [String]
    }

initArgs = Args
    { verbose   = False
    , optimise  = True
    , astOnly   = False
    , lexOnly   = False
    , printLLIR = False
    , compileObj = False
    , printCImports = False
    , printAst = False
    , printSymbols = False
    , printCSymbols = False
    , printFinalAst = False
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
    ["--print-c"] -> args { printCImports = True }
    ["--print-c-symbols"] -> args { printCSymbols = True }
    ["--print-ast"] -> args { printAst = True }
    ["--print-ast-final"] -> args { printFinalAst = True }
    ["--print-symbols"] -> args { printSymbols = True }
    ["-c"] -> args { compileObj = True }
    [str]  -> args { modPaths  = (modPaths args) ++ [str] }
    (a:as) -> parseArgs (parseArgs args [a]) as

