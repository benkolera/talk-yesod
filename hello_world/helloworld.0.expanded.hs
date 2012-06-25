{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Data.Text
import qualified Yesod.Routes.Dispatch
import qualified Data.Map
import Text.Blaze.Internal (preEscapedText)

data HelloWorld = HelloWorld

instance RenderRoute HelloWorld where
  data Route HelloWorld = HomeR deriving (Show, Eq, Read)
  renderRoute HomeR = ([], [])

type Handler = GHandler HelloWorld HelloWorld
type Widget = GWidget HelloWorld HelloWorld ()
  
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

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ toWidget $ (preEscapedText . pack) "Hello yesod!"

main :: IO ()
main = warpDebug 3000 HelloWorld




