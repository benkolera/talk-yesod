{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Tasks where

import Import

getTasksR :: Handler RepHtml

getTasksR = defaultLayout $ do
    setTitle "Tasks"
    $(widgetFile "tasks")

postTasksR :: Handler RepHtml
postTasksR = undefined
