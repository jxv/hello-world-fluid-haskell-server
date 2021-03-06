-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module
module Api.Server
  ( helloWorld'handlerMap
  , helloWorld'spec
  , helloWorld'Scotty'Post
  , helloWorld'Scotty'Get
  , V2.Hello(..)
  , V2.Goodbye(..)
  , V2.Lang(..)
  , V2.HelloWorld'Service(..)
  , V2.HelloWorld'Thrower(..)
  , V2.helloWorld'pull
  ) where

import qualified Prelude as P
import qualified Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import qualified Fluid.Imports as R
import qualified Fluid.Server.Scotty as Scotty
import qualified Api.Major2 as V2
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'handler
  , helloWorld'version
  , helloWorld'pull
  , helloWorld'spec
  , Hello(..)
  , Goodbye(..)
  , Lang(..)
  )
import qualified Api.Major1 as V1
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'handler
  , helloWorld'version
  , helloWorld'pull
  , helloWorld'spec
  , Hello(..)
  , Goodbye(..)
  , Lang(..)
  )
import qualified Api.Major0 as V0
  ( HelloWorld'Service(..)
  , HelloWorld'Thrower(..)
  , helloWorld'handler
  , helloWorld'version
  , helloWorld'pull
  , helloWorld'spec
  , Hello(..)
  , Goodbye(..)
  )

helloWorld'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V2.HelloWorld'Service meta2 m
    , V1.HelloWorld'Service meta1 m
    , V0.HelloWorld'Service meta0 m
    )
  => (xtra -> C.Hooks m () meta2)
  -> (xtra -> C.Hooks m () meta1)
  -> (xtra -> C.Hooks m () meta0)
  -> xtra
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
helloWorld'handlerMap hooks2 hooks1 hooks0 xtra = R.fromList
    [ (2, (0, V2.helloWorld'handler hooks2 xtra))
    , (1, (0, V1.helloWorld'handler hooks1 xtra))
    , (0, (1, V0.helloWorld'handler hooks0 xtra))
    ]

helloWorld'spec :: R.Value
helloWorld'spec = R.toJSON
  [ V2.helloWorld'spec
  , V1.helloWorld'spec
  , V0.helloWorld'spec
  ]

helloWorld'Scotty'Post
  ::
    ( Scotty.ScottyError e
    , R.MonadIO m
    , R.MonadCatch m
    , V2.HelloWorld'Service meta2 m
    , V1.HelloWorld'Service meta1 m
    , V0.HelloWorld'Service meta0 m
    )
  => C.Pull
  -> ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m () meta2)
  -> ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m () meta1)
  -> ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m () meta0)
  -> Scotty.ScottyT e m ()
helloWorld'Scotty'Post pull hooks2 hooks1 hooks0 = Scotty.respond pull (helloWorld'handlerMap hooks2 hooks1 hooks0)

helloWorld'Scotty'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
helloWorld'Scotty'Get = Scotty.getSpec helloWorld'spec
