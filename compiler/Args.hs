module Args where

data Args = Args
    { verbose     :: Bool
    , optimise    :: Bool
    , astOnly     :: Bool
    , lexOnly     :: Bool
    , printLLIR   :: Bool
    , modPaths    :: [String]
    }

initArgs = Args
    { verbose   = False
    , optimise  = True
    , astOnly   = False
    , lexOnly   = False
    , printLLIR = False
    , modPaths  = []
    }


parseArgs :: Args -> [String] -> Args
parseArgs args argStrs = case argStrs of
    []     -> args
    ["-n"] -> args { optimise  = False }
    ["-v"] -> args { verbose   = True }
    ["-a"] -> args { astOnly   = True }
    ["-l"] -> args { lexOnly   = True }
    ["-p"] -> args { printLLIR = True }
    [str]  -> args { modPaths  = (modPaths args) ++ [str] }
    (a:as) -> parseArgs (parseArgs args [a]) as

