{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Data.Text
import qualified Data.Map
import qualified Yesod.Routes.Dispatch
import qualified Text.Blaze.Internal

data HelloWorld = HelloWorld

instance RenderRoute HelloWorld where
  data Route HelloWorld = HomeR Text deriving (Show, Eq, Read)
  renderRoute (HomeR name) = 
    (((pack "hello") : ((toPathPiece name) : [])), 
     [])

type Handler = GHandler HelloWorld HelloWorld
type Widget = GWidget HelloWorld HelloWorld ()

instance YesodDispatch HelloWorld HelloWorld where
  yesodDispatch master sub toMaster handler404 handler405 method pieces = 
    case dispatch pieces of 
      Just f -> f master sub toMaster handler404 handler405 method
      Nothing -> handler404
    where
      dispatch = 
        Yesod.Routes.Dispatch.toDispatch
        [ Yesod.Routes.Dispatch.Route
          [ Yesod.Routes.Dispatch.Static (pack "hello"), 
            Yesod.Routes.Dispatch.Dynamic]
          False
          handleHelloPieces]
          
          
      handleHelloPieces [_, x ] = do 
        y <- fromPathPiece x
        Just $ handleHelloMethods y
      handleHelloPieces _ = error "Invariant violated"
      handleHelloMethods y master sub toMaster handler404 handler405 method = 
        case Data.Map.lookup method methodsHomeR of
          Just f -> let handler = f y
                    in yesodRunner handler master sub (Just (HomeR y)) toMaster
          Nothing -> handler405 (HomeR y)
      methodsHomeR = Data.Map.fromList
                     [(pack "GET", \ arg -> fmap chooseRep (getHomeR arg))]

instance Yesod HelloWorld

getHomeR :: Text -> Handler RepHtml
getHomeR name = defaultLayout $ do
  toWidget ((Text.Blaze.Internal.preEscapedText . pack) "Hello ")
  toWidget (toHtml name)
  toWidget ((Text.Blaze.Internal.preEscapedText . pack) "!")

main :: IO ()
main = warpDebug 3000 HelloWorld
