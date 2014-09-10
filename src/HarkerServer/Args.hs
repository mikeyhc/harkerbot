module HarkerServer.Args where

import System.Console.GetOpt

data CmdFlag 
    = SetNick String
    | SetPass String
    | SetServer String
    | SetPort String
    | SetChan String
    | SetMaxLine String
    | SetPlugDir String

data OptSet = OptSet 
    { optSetNick     :: String
    , optSetPass     :: String
    , optSetServer   :: String
    , optSetPort     :: Int
    , optSetChan     :: String
    , optSetMaxLines :: Int
    , optSetPlugDir  :: String
    }

defpass      = "harkerpass"
defserver    = "segfault.net.nz"
defport      = 6667
defchan      = "#bots"
defnick      = "harker"
defmaxlines  = 7
defplugdir   = "plugins"

options = [Option "n" []  (ReqArg SetNick "NICK")
            "Set the default nick for the bot"
          ,Option "p" []  (ReqArg SetPass "PASS")
            "Set the authentication password"
          ,Option "H" []  (ReqArg SetServer "HOST")  
            "Sets the host to connect to"
          ,Option "P" []  (ReqArg SetPort "PORT")
            "Sets the port to connect on"
          ,Option "c" []  (ReqArg SetChan "CHANNEL")
            "Sets the channel to connect to"
          ,Option "m" []  (ReqArg SetMaxLine "MAXLINES") 
            "Set the maximum lines printed in a public channel"
          ,Option "d" []  (ReqArg SetPlugDir "PLUGINDIR")
            "Sets the directory to automatically load plugins from"
          ]

emptyOptSet :: OptSet
emptyOptSet = OptSet defnick defpass defserver defport defchan defmaxlines 
                     defplugdir

parseopts :: [String] -> Either String OptSet
parseopts argv = case getOpt Permute options argv of 
    (args, _, []  ) -> Right $ foldr parsearg emptyOptSet args
    (_,    _, errs) -> Left $ concat errs ++ usageInfo header options
    where header = "Usage: harker-server [OPTIONS..]"

parsearg :: CmdFlag -> OptSet -> OptSet
parsearg (SetNick x)    o = o { optSetNick     =      x }
parsearg (SetPass x)    o = o { optSetPass     =      x }
parsearg (SetServer x)  o = o { optSetServer   =      x }
parsearg (SetPort x)    o = o { optSetPort     = read x }
parsearg (SetChan x)    o = o { optSetChan     =      x }
parsearg (SetMaxLine x) o = o { optSetMaxLines = read x }
parsearg (SetPlugDir x) o = o { optSetPlugDir  =      x }
