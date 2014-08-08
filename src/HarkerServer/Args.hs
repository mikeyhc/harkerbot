module HarkerServer.Args where

import System.Console.GetOpt

data CmdFlag 
    = SetNick String
    | SetPass String
    | SetServer String
    | SetPort String
    | SetChan String
    | SetMaxLine String

type OptSet         = (String, String, String, Int, String, Int)

defpass      = "morepass"
defserver    = "segfault.net.nz"
defport      = 6667
defchan      = "#bots"
defnick      = "harker"
defmaxlines  = 7

options = [Option ['n'] []  (ReqArg SetNick "NICK")
            "Set the default nick for the bot"
          ,Option ['p'] []  (ReqArg SetPass "PASS")
            "Set the authentication password"
          ,Option ['H'] []  (ReqArg SetServer "HOST")  
            "Sets the host to connect to"
          ,Option ['P'] []  (ReqArg SetPort "PORT")
            "Sets the port to connect on"
          ,Option ['c'] []  (ReqArg SetChan "CHANNEL")
            "Sets the channel to connect to"
          ,Option ['m'] []  (ReqArg SetMaxLine "MAXLINES") 
            "Set the maximum lines printed in a public channel"
          ]

emptyOptSet :: OptSet
emptyOptSet = (defnick, defpass, defserver, defport, defchan, defmaxlines)

parseopts :: [String] -> Either String OptSet
parseopts argv = case getOpt Permute options argv of 
    (args, _, []  ) -> Right $ foldr parsearg emptyOptSet args
    (_,    _, errs) -> Left $ concat errs ++ usageInfo header options
    where header = "Usage: harker-server [OPTIONS..]"

_first  g (a, b, c, d, e, f) = (g a, b, c, d, e, f)
_second g (a, b, c, d, e, f) = (a, g b, c, d, e, f)
_third  g (a, b, c, d, e, f) = (a, b, g c, d, e, f)
_fourth g (a, b, c, d, e, f) = (a, b, c, g d, e, f)
_fifth  g (a, b, c, d, e, f) = (a, b, c, d, g e, f)
_sixth  g (a, b, c, d, e, f) = (a, b, c, d, e, g f)

parsearg :: CmdFlag -> OptSet -> OptSet
parsearg (SetNick x)    = _first  (const x)
parsearg (SetPass x)    = _second (const x)
parsearg (SetServer x)  = _third  (const x)
parsearg (SetPort x)    = _fourth (const $ read x)
parsearg (SetChan x)    = _fifth  (const x)
parsearg (SetMaxLine x) = _sixth  (const $ read x)
