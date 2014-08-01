import Control.Concurrent
import Control.Monad
import Network
import System.Directory
import System.IO

sockaddr = "/tmp/.local-echo.sock"

main = do
    s <- listenOn (UnixSocket sockaddr)
    h <- connectTo "localhost" (UnixSocket "/tmp/.harker-server.sock")
    hPutStrLn h "name: local-echo"
    hPutStrLn h "version: 0.1.0.0"
    hPutStrLn h $ "server: " ++ sockaddr
    r <- hGetLine h
    if r == "registed" 
        then loopfunc (acceptfunc s)
        else hPutStrLn stderr $ "error: " ++ r

loopfunc a = a >> loopfunc a

acceptfunc :: Socket -> IO ()
acceptfunc s = accept s >>= \(h, _, _) ->
    void $ forkFinally (loopfunc (hGetLine h >>= putStrLn)) 
                       (\_ -> hClose h >> removeFile sockaddr)
