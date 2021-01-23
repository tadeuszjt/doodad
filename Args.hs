module Args where

data Args = Args
    { verbose     :: Bool
    , optimise    :: Bool
    , lexOnly     :: Bool
    , astOnly     :: Bool
    , modPaths    :: [String]
    }

initArgs = Args
    { verbose     = False
    , optimise    = True
    , astOnly     = False
    , lexOnly     = False
    , modPaths    = []
    }


parseArgs :: Args -> [String] -> Args
parseArgs args argStrs = case argStrs of
    []     -> args
    ["-n"] -> args { optimise    = False }
    ["-v"] -> args { verbose     = True }
    ["-a"] -> args { astOnly     = True }
    ["-l"] -> args { lexOnly     = True }
    [str]  -> args { modPaths    = (modPaths args) ++ [str] }
    (a:as) -> parseArgs (parseArgs args [a]) as

