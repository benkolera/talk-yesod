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
  yesodDispatch
    master0_a3aG
    sub0_a3aH
    toMaster0_a3aI
    app4040_a3aJ
    handler4050_a3aK
    method0_a3aL
    pieces0_a3aM
    = 
      case dispatch_a3aN pieces0_a3aM of 
        Just f_a3b2
          -> f_a3b2
             master0_a3aG
             sub0_a3aH
             toMaster0_a3aI
             app4040_a3aJ
             handler4050_a3aK
             method0_a3aL
        Nothing -> app4040_a3aJ
      where
        dispatch_a3aN = Yesod.Routes.Dispatch.toDispatch
                        [Yesod.Routes.Dispatch.Route
                         [Yesod.Routes.Dispatch.Static (pack "hello"),
                          Yesod.Routes.Dispatch.Dynamic]
                         False
                         (\ pieces_a3aO -> case pieces_a3aO of
                             [_, x_a3aP] -> do 
                               y_a3aQ <- fromPathPiece x_a3aP
                               Just (\ master_a3aT
                                       sub_a3aU
                                       toMaster_a3aV
                                       _app404_a3aW
                                       _handler405_a3aX
                                       _method_a3aY
                                     -> case Data.Map.lookup _method_a3aY methodsHomeR of
                                       Just f_a3b0 -> let handler_a3aZ = f_a3b0 y_a3aQ
                                                      in
                                                       yesodRunner
                                                       handler_a3aZ
                                                       master_a3aT
                                                       sub_a3aU
                                                       (Just (HomeR y_a3aQ))
                                                       toMaster_a3aV
                                       Nothing -> _handler405_a3aX (HomeR y_a3aQ) )
                             _ -> error "Invariant violated" )]
        methodsHomeR = Data.Map.fromList
                       [(pack "GET", \ arg_a3b1 -> fmap chooseRep (getHomeR arg_a3b1))]

instance Yesod HelloWorld

getHomeR :: Text -> Handler RepHtml
getHomeR name = defaultLayout $ do
  toWidget ((Text.Blaze.Internal.preEscapedText . pack) "Hello ")
  toWidget (toHtml name)
  toWidget ((Text.Blaze.Internal.preEscapedText . pack) "!")

main :: IO ()
main = warpDebug 3000 HelloWorld
