module HelloWorld (main) where

import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Control.Monad.IO.Class
import Fluid.Types
import Fluid.Server.Scotty
import Fluid.Server

import Api.Server
import qualified Api.Major0 as V0
import qualified Api.Major1 as V1
import qualified Api.Major2 as V2

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance ServiceThrower App
instance V0.HelloWorld'Thrower App
instance V1.HelloWorld'Thrower App
instance V2.HelloWorld'Thrower App

instance V0.HelloWorld'Service () App where
  helloWorld'Hello () = hello
  helloWorld'Goodbye () = goodbye

instance V1.HelloWorld'Service () App where
  helloWorld'Hello () = hello'
  helloWorld'Goodbye () = goodbye

instance V2.HelloWorld'Service () App where
  helloWorld'Hello () = hello'
  helloWorld'Goodbye () = goodbye'

hello :: Monad m => V0.Hello -> m Text
hello req = return $ "Hello " `mappend` (V0.helloWho req) `mappend` "!"

hello' :: Monad m => V1.Hello -> m Text
hello' req = case V1.helloLang req of
  V1.Lang'English -> return $ "Hello " `mappend` (V1.helloWho req) `mappend` "!"
  V1.Lang'Spanish -> return $ "Hola " `mappend` (V1.helloWho req) `mappend` "!"
  V1.Lang'French -> return $ "Bonjour " `mappend` (V1.helloWho req) `mappend` "!"

goodbye :: Monad m => V0.Goodbye -> m Text
goodbye req = return $ "Goodbye " `mappend` (V0.goodbyeWho req) `mappend` "!"

goodbye' :: Monad m => V2.Goodbye -> m Text
goodbye' req = case V2.goodbyeLang req of
  V2.Lang'English -> return $ "Goodbye " `mappend` (V2.goodbyeWho req) `mappend` "!"
  V2.Lang'Spanish -> return $ "Adios " `mappend` (V2.goodbyeWho req) `mappend` "!"
  V2.Lang'French -> return $ "Adieu " `mappend` (V2.goodbyeWho req) `mappend` "!"

main :: IO ()
main = runServer helloWorld'pull unApp routes

routes :: ScottyT TL.Text App ()
routes = do
  helloWorld'Scotty'Post helloWorld'pull (const defHooks) (const defHooks) (const defHooks)
  helloWorld'Scotty'Get helloWorld'pull
