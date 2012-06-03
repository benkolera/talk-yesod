{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.About where

import Import

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
  setTitle "About Tomato Monad"
  $(widgetFile "about")
