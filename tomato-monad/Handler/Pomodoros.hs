{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Pomodoros where

import Import

getPomodorosR :: Handler RepHtml

getPomodorosR = defaultLayout $ do
    setTitle "Pomodoros"
    $(widgetFile "pomodoros")

postPomodorosR :: Handler RepHtml
postPomodorosR = undefined
