{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Tasks 
       (
         getTasksR
         , postTasksR
         , getTaskR
         , postTaskR
         , deleteTaskR
       ) where

import Import
import Yesod.Form.Jquery

getTasksR :: Handler RepHtml
getTasksR = do
  (widget, enctype) <- generateFormPost $ taskF Nothing
  showPageW widget enctype

postTasksR :: Handler RepHtml
postTasksR = do
  ((result, postFormW ), enctype) <- runFormPost $ taskF Nothing
  case result of
    FormSuccess task -> do
      (newFormW, enctypeNew) <- generateFormPost $ taskF Nothing 
      _ <- runDB $ insert task
      showPageW newFormW enctypeNew
    _ -> do  
      showPageW postFormW enctype

getTaskR :: TaskId -> Handler RepHtml
getTaskR id = do 
  task <- runDB $ get id 
  (formW , enctype) <- generateFormPost $ taskF task
  let taskFormW = taskCreateFormW formW enctype
  defaultLayout $ do 
    setTitle "Edit Task: " 
    $( widgetFile "task" )

postTaskR :: TaskId -> Handler RepHtml
postTaskR id = do
  task <- runDB $ get id 
  ((result, widget), enctype) <- runFormPost $ taskF task
  case result of
    FormSuccess modifiedTask -> do
      (widgetNew, enctypeNew) <- generateFormPost $ taskF Nothing 
      runDB $ replace id modifiedTask
      showPageW widgetNew enctypeNew
    _ -> do  
      showPageW widget enctype
      
deleteTaskR :: TaskId -> Handler RepHtml
deleteTaskR id = undefined      

showPageW :: Widget -> Enctype -> GHandler App App RepHtml
showPageW formW enctype = do
  dateTasks   <- runDB $ selectList [TaskDueDate !=. Nothing ] [Asc TaskDueDate]
  noDateTasks <- runDB $ selectList [TaskDueDate ==. Nothing ] [Asc TaskDueDate]
  let dueTasksW    = taskAbridgedListW "Tasks with due dates:" dateTasks
      notDueTasksW = taskAbridgedListW "Tasks without due dates:" noDateTasks
      taskFormW    = taskCreateFormW formW enctype
  defaultLayout $ do
    setTitle "Tasks"
    $(widgetFile "tasks")
   

taskAbridgedListW :: Text -> [Entity Task] -> GWidget App App ()
taskAbridgedListW title tasks = do
  $( widgetFile "task-abridged-list" )
  
--taskCreateFormW :: Widget -> Enctype -> GWidget App App ()
taskCreateFormW formW enctype = do 
  currentRoute <- getCurrentRoute
  case currentRoute of
    Nothing     -> do notFound
    Just formR  -> do
      $( widgetFile "task-create" )
  

taskF :: Maybe Task -> Html -> MForm App App (FormResult Task, Widget)
taskF task = renderDivs $ Task
  <$> areq textField "Title" ( fmap taskTitle task )
  <*> aopt dueDateField "Due Date" ( fmap taskDueDate task )  -- Not task >>= taskDueDate like I originally thought!
  <*> areq textareaField "Body" ( fmap taskBody task )
  where 
    dueDateField = jqueryDayField def { 
      jdsChangeYear = True
      , jdsYearRange = "0:+5" 
      }

