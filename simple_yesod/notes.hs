{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}

import Yesod

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Text

import Control.Applicative ((<$>),(<*>))

import Control.Monad.IO.Class (liftIO)

data Notes = Notes { 
  dbConn :: Connection
}

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage Notes FormMessage where
    renderMessage _ _ = defaultFormMessage

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Note
  title Text
  body  Textarea
|]

mkYesod "Notes" [parseRoutes|
/notes/        NotesR GET POST
/notes/#NoteId NoteR  GET
|]

instance Yesod Notes

createNoteForm :: Html -> MForm Notes Notes ( FormResult Note , Widget )
createNoteForm = renderDivs $ Note 
  <$> areq textField "Title" Nothing
  <*> areq textareaField "Body" Nothing
  
getNotesR :: Handler RepHtml
getNotesR = do
  (widget, encType) <- generateFormPost createNoteForm
  defaultLayout [whamlet|
<h1>Create Note
<form method=post action=@{NotesR} enctype=#{encType}>
    ^{widget}
    <input type=submit>
|]

postNotesR :: Handler RepHtml
postNotesR = do 
  ((result, widget), encType) <- runFormPost createNoteForm
  case result of
    FormSuccess note -> postNotesRWin note
    _ -> postNotesRFail widget encType 

postNotesRWin note = do
  conn   <- fmap dbConn getYesod
  noteId <- liftIO $ runSqlConn ( insert note ) conn 
  redirect $ NoteR noteId

postNotesRFail widget encType = defaultLayout [whamlet|
<p>Invalid input, let's try again.
<form method=post action=@{NotesR} enctype=#{encType}>
  ^{widget}
  <input type=submit>
|]

getNoteR :: NoteId -> Handler RepHtml
getNoteR id = do
  conn   <- fmap dbConn getYesod
  note   <- liftIO $ runSqlConn ( get id ) conn
  case note of 
    Nothing    -> notFound
    ( Just ( Note title body ) ) -> do 
      defaultLayout [whamlet|
<h1>#{ title }
<p>#{ body }
|]
  

main :: IO ()
main = withSqliteConn ":memory:" run
  where 
    run conn = do 
      runSqlConn (runMigration migrateAll) conn
      warpDebug 3000 (Notes conn )

