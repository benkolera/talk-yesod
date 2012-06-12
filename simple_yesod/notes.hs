{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

import Yesod
import Yesod.Form.Jquery
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Data.Text
import Control.Applicative ((<$>),(<*>))
import Control.Monad.IO.Class (liftIO)

data Notes = Notes { 
  dbConn :: Connection
}
 
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Note
  title Text
  date  Day Maybe
  body  Textarea
|]

instance YesodPersist Notes where
  type YesodPersistBackend Notes = SqlPersist
  runDB f = do
    conn <- fmap dbConn getYesod
    runSqlConn f conn      

mkYesod "Notes" [parseRoutes|
/notes/        NotesR GET POST
/notes/#NoteId NoteR  GET
|]

instance Yesod Notes

instance YesodJquery Notes
instance RenderMessage Notes FormMessage where
    renderMessage _ _ = defaultFormMessage

createNoteForm :: Day -> Html -> MForm Notes Notes ( FormResult Note , Widget )
createNoteForm today = renderDivs $ Note 
  <$> areq textField "Title" Nothing
  <*> aopt dueDateField "Date" ( Just ( Just today ) )
  <*> areq textareaField "Body" Nothing
  where 
    dueDateField = jqueryDayField def 
  
getNotesR :: Handler RepHtml
getNotesR = do
  today <- fmap utctDay $ liftIO getCurrentTime
  (widget, encType) <- generateFormPost $ createNoteForm today
  showCreateNoteForm widget encType

showCreateNoteForm widget encType = do
  notes  <- runDB $ selectList [] [Asc NoteTitle]
  defaultLayout [whamlet|
<h1>Notes
<ul>                 
  $forall Entity id note <- notes
    <li><a href="@{NoteR id}">#{noteTitle note}</a>                            
<h1>Create Note
<form method=post action=@{NotesR} enctype=#{encType}>
    ^{widget}
    <input type=submit>
|]

postNotesR :: Handler RepHtml
postNotesR = do 
  today <- fmap utctDay $ liftIO getCurrentTime
  ((result, widget), encType) <- runFormPost $ createNoteForm today
  case result of
    FormSuccess note -> postNotesRWin note
    _ -> showCreateNoteForm widget encType

postNotesRWin note = do
  noteId <- runDB $ insert note
  redirect $ NoteR noteId

getNoteR :: NoteId -> Handler RepHtml
getNoteR id = do
  note <- runDB $ get id
  case note of 
    Nothing    -> notFound
    ( Just ( Note title date body ) ) -> do 
      defaultLayout [whamlet|
<h1>#{ title }
  $maybe d <- date
    \ ( dated: #{ show d } )
<p>#{ body }
|]
  

main :: IO ()
main = withSqliteConn ":memory:" run
  where 
    run conn = do 
      runSqlConn (runMigration migrateAll) conn
      warpDebug 3000 (Notes conn )

