module Args where

data Args = Args
    { verbose     :: Bool
    , optimise    :: Bool
    , astOnly     :: Bool
    , lexOnly     :: Bool
    , inferOnly   :: Bool
    , printLLIR   :: Bool
    , compileObj  :: Bool
    , modPaths    :: [String]
    }

initArgs = Args
    { verbose   = False
    , optimise  = True
    , astOnly   = False
    , lexOnly   = False
    , inferOnly = False
    , printLLIR = False
    , compileObj = False
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
    ["-c"] -> args { compileObj = True }
    ["-i"] -> args { inferOnly  = True }
    [str]  -> args { modPaths  = (modPaths args) ++ [str] }
    (a:as) -> parseArgs (parseArgs args [a]) as

