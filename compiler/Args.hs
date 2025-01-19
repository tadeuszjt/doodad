module Args where

data Args = Args
    { verbose     :: Bool
    , astOnly     :: Bool
    , lexOnly     :: Bool
    , optimise    :: Bool
    , printTokens :: Bool
    , printAst    :: Bool
    , printAstResolved :: Bool
    , printAstAnnotated :: Bool
    , printAstInferred :: Bool
    , printSymbols :: Bool
    , printAstFinal :: Bool
    , printIr       :: Bool
    , printC        :: Bool
    , printAssembly :: Bool
    , modPaths    :: [String]
    }

initArgs = Args
    { verbose   = False
    , astOnly   = False
    , lexOnly   = False
    , optimise  = True
    , printTokens = False
    , printAst = False
    , printAstResolved = False
    , printAstAnnotated = False
    , printAstInferred = False
    , printSymbols = False
    , printAstFinal = False
    , printIr = False
    , printC = False
    , printAssembly = False
    , modPaths  = []
    }


parseArgs :: Args -> [String] -> Args
parseArgs args argStrs = case argStrs of
    []     -> args
    ["-a"] -> args { astOnly   = True }
    ["-l"] -> args { lexOnly   = True }
    ["-n"] -> args { optimise  = False }
    ["--verbose"] -> args { verbose   = True }
    ["--print-tokens"] -> args { printTokens = True }
    ["--print-ast"] -> args { printAst = True }
    ["--print-ast-resolved"] -> args { printAstResolved = True }
    ["--print-ast-annotated"] -> args { printAstAnnotated = True }
    ["--print-ast-inferred"] -> args { printAstInferred = True }
    ["--print-ast-final"] -> args { printAstFinal = True }
    ["--print-symbols"] -> args { printSymbols = True }
    ["--print-c"] -> args { printC = True }
    ["--print-assembly"] -> args { printAssembly = True }
    ["--print-ir"] -> args { printIr = True }
    [str]  -> args { modPaths  = (modPaths args) ++ [str] }
    (a:as) -> parseArgs (parseArgs args [a]) as

