{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

-- Here is one of the simplest yesod apps that you can write.

import Yesod

-- Our foundation datatype. A normal haskell data type used for configuration
-- and central app state ( like a DB connection pool )
data HelloWorld = HelloWorld

-- Generates our routes and our dispatch function. 
mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

-- The yesod typeclass has lots of methods that we can override to configure 
-- yesod's behaviour. Here, we just accept the defaults.
instance Yesod HelloWorld

-- A handler is our 'controller' for GET '/'.
-- RepHtml just indicates that we're outputting HTML. Due to type inference, we
-- don't actually need this type.
-- getHomeR :: Handler RepHtml
getHomeR = defaultLayout [whamlet|Hello yesod!|]

-- Starts a warp instance listening on port 3000 that will be handled by our
-- app.
-- We don't have to run in warp in production, but this is the simplest way to
-- get a running app in a webserver.
-- main :: IO ()
main = warpDebug 3000 HelloWorld
