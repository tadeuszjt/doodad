module Args where

data Args = Args
    { verbose     :: Bool
    , optimise    :: Bool
    , astOnly     :: Bool
    , lexOnly     :: Bool
    , printTokens :: Bool
    , printAst    :: Bool
    , printAstResolved :: Bool
    , printAstAnnotated :: Bool
    , printSymbols :: Bool
    , printAstFinal :: Bool
    , modPaths    :: [String]
    }

initArgs = Args
    { verbose   = False
    , optimise  = True
    , astOnly   = False
    , lexOnly   = False
    , printTokens = False
    , printAst = False
    , printAstResolved = False
    , printAstAnnotated = False
    , printSymbols = False
    , printAstFinal = False
    , modPaths  = []
    }


parseArgs :: Args -> [String] -> Args
parseArgs args argStrs = case argStrs of
    []     -> args
    ["-n"] -> args { optimise  = False }
    ["-a"] -> args { astOnly   = True }
    ["-l"] -> args { lexOnly   = True }
    ["--verbose"] -> args { verbose   = True }
    ["--print-tokens"] -> args { printTokens = True }
    ["--print-ast"] -> args { printAst = True }
    ["--print-ast-resolved"] -> args { printAstResolved = True }
    ["--print-ast-annotated"] -> args { printAstAnnotated = True }
    ["--print-ast-final"] -> args { printAstFinal = True }
    ["--print-symbols"] -> args { printSymbols = True }
    [str]  -> args { modPaths  = (modPaths args) ++ [str] }
    (a:as) -> parseArgs (parseArgs args [a]) as

