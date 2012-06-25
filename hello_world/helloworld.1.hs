{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod

-- This time we need to import Data.Text to get the Text constructor for our
-- route piece.
import Data.Text

data HelloWorld = HelloWorld

-- This time our route has a static part "hello" and a dynamic part of type Text
mkYesod "HelloWorld" [parseRoutes|
/hello/#Text HomeR GET
|]

instance Yesod HelloWorld

-- And now our handler has one parameter. It is a type error to omit this
-- parameter or to have a parameter that is not type Text.
-- This name parameter is then interpolated into the output.
-- getHomeR :: Text -> Handler RepHtml
getHomeR name = defaultLayout [whamlet|Hello #{name}!|]

main = warpDebug 3001 HelloWorld
