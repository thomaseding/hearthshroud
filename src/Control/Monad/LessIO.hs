module Control.Monad.LessIO where


import System.Process
import System.Posix.IO
import Control.Exception


-- Code source
-- http://www.reddit.com/r/haskell/comments/2y2f1p/pager_in_ghci_like_less/cp5n8hb


less :: IO a -> IO a
less a = do
      stdout_copy <- dup stdOutput
      (Just pipe_handle, Nothing, Nothing, pid) <- createProcess (proc "less" []) { std_in = CreatePipe }
      closeFd stdOutput
      pipe_fd <- handleToFd pipe_handle
      _ <- dupTo pipe_fd stdOutput
      closeFd pipe_fd
      a `finally` closeFd stdOutput `finally` waitForProcess pid `finally` do
            _ <- dupTo stdout_copy stdOutput
            closeFd stdout_copy








