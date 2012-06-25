{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod

-- Some extra imports used by our generated code.
import Data.Text
import qualified Yesod.Routes.Dispatch
import qualified Data.Map
import Text.Blaze.Internal (preEscapedText)

data HelloWorld = HelloWorld

-- ( A result of mkYesod splicing )
-- This instance defines our route constructors and a method for turning each
-- one of our routes into a list of path pieces.
instance RenderRoute HelloWorld where
  data Route HelloWorld = HomeR deriving (Show, Eq, Read)
  renderRoute HomeR = ([], [])

-- ( A result of mkYesod splicing )
-- These are just shortcuts so we don't have to type the long versions in our
-- type signatures. Type errors will likely talk about GWidget and GHandler
-- though so this is really handy to know.  
type Handler = GHandler HelloWorld HelloWorld
type Widget = GWidget HelloWorld HelloWorld ()

-- ( A result of mkYesod splicing )
-- This has the job of taking our path pieces / URI and then routing it to a
-- handler ( or 404ing or 405ing if the path pieces or methods are bogus ).
instance YesodDispatch HelloWorld HelloWorld where
  yesodDispatch master sub toMaster 
    handler404 handler405 method pieces = 
    case dispatch pieces of
      Just f  -> f master sub toMaster handler404 handler405 method 
      Nothing -> handler404
    where
      dispatch = Yesod.Routes.Dispatch.toDispatch 
                 [ Yesod.Routes.Dispatch.Route [] False handleHomePieces ]
      handleHomePieces [] = Just handleHomeMethods
      homeHomePieces _ = error "Invariant violated"
      handleHomeMethods master sub toMaster app handler405 method =
        case Data.Map.lookup method methodsHomeR of 
          Just handler -> yesodRunner handler master sub (Just HomeR) toMaster
          Nothing -> handler405 HomeR
      methodsHomeR = Data.Map.fromList 
                     [ ( ( pack "GET" ) , ( fmap chooseRep getHomeR ) ) ]


instance Yesod HelloWorld

-- Note that we take the blaze html and make a widget out of it. Widgets are our
-- basic building block of building a HTML output. 
getHomeR = defaultLayout $ toWidget $ (preEscapedText . pack) "Hello yesod!"

main = warpDebug 3000 HelloWorld




