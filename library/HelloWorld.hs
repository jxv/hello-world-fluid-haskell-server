module HelloWorld (main) where

import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Control.Monad.IO.Class
import Fluid.Types
import Fluid.Server.Scotty
import Fluid.Server

import Api.Server
import qualified Api.Major0 as V0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance ServiceThrower App
instance V0.HelloWorld'Thrower App

instance V0.HelloWorld'Service () App where
  helloWorld'Hello () = hello
  helloWorld'Goodbye () = goodbye

hello :: Monad m => V0.Hello -> m Text
hello V0.Hello{helloWho} = return $ "Hello " `mappend` helloWho `mappend` "!"

goodbye :: Monad m => V0.Goodbye -> m Text
goodbye V0.Goodbye{goodbyeWho} = return $ "Goodbye " `mappend` goodbyeWho `mappend` "!"

main :: IO ()
main = runServer helloWorld'pull unApp routes

routes :: ScottyT TL.Text App ()
routes = do
  helloWorld'Scotty'Post helloWorld'pull (const defHooks)
  helloWorld'Scotty'Get helloWorld'pull
