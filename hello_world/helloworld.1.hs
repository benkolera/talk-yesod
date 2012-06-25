{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Data.Text

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/hello/#Text HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Text -> Handler RepHtml
getHomeR name = defaultLayout [whamlet|Hello #{name}!|]

main :: IO ()
main = warpDebug 3001 HelloWorld
