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
  dbConn :: Connection -- Use a connection pool in prod, plz.
}

-- This creates our schema. A note has a title, body and an optional date. 
-- This will also create a NoteId, which is an autoincrementing integer id for 
-- the note.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Note
  title Text
  date  Day Maybe
  body  Textarea
|]

-- This typeclass gives us a nice abstraction to be able to run any db function
-- in the presence of a db connection. We don't absolutely need this, but it is
-- a lot neater than pulling out the connection every time we need to do
-- something with the db.
instance YesodPersist Notes where
  type YesodPersistBackend Notes = SqlPersist
  runDB f = do
    conn <- fmap dbConn getYesod
    runSqlConn f conn      

-- Note that our NoteId has an instance of PathPiece so that we can use it as
-- part of our route.
mkYesod "Notes" [parseRoutes|
/notes/        NotesR GET POST
/notes/#NoteId NoteR  GET
|]

instance Yesod Notes

-- This is here to tell yesod where JQuery is. By default it uses the google
-- CDN but you can point it to your own CDN or copy if you want.
instance YesodJquery Notes

-- This is for 118n to be able to render form errors in different languages.
instance RenderMessage Notes FormMessage where
    renderMessage _ _ = defaultFormMessage

-- This function creates our form, taking in the current time as an argument.
-- This form will handle all of our validation ( that title and body are non
-- empty and that the date field is a valid date if it is set ) and be able to
-- give us a fully constructed Note if all of the validation passes.
createNoteForm today = renderDivs $ Note 
  <$> areq textField "Title" Nothing
  <*> aopt dateField "Date" ( Just ( Just today ) )
  <*> areq textareaField "Body" Nothing
  where 
    dateField = jqueryDayField def 

-- Shows a list of notes and also a form to add a new note.
getNotesR = do
  today <- fmap utctDay $ liftIO getCurrentTime
  (widget, encType) <- generateFormPost $ createNoteForm today
  showCreateNoteForm widget encType

-- A handler that we can compose into other handlers. It'll load the notes from
-- the DB, print the list of notes and also print the note creation form that we
-- pass in.  
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

-- This uses our form to validate the params posted to us and either print the
-- form again with the form errors or insert the note and redirect to it.
postNotesR = do 
  today <- fmap utctDay $ liftIO getCurrentTime
  ((result, widget), encType) <- runFormPost $ createNoteForm today
  case result of
    FormSuccess note -> postNotesRWin note
    _ -> showCreateNoteForm widget encType

postNotesRWin note = do
  noteId <- runDB $ insert note
  redirect $ NoteR noteId

-- This displays our note, optionally displaying the date
getNoteR id = do
  note <- runDB $ get id
  case note of 
    Nothing    -> notFound
    Just ( Note title date body ) -> do 
      defaultLayout [whamlet|
        <h1>#{ title }
        $maybe d <- date
          \ ( dated: #{ show d } )
        <p>#{ body }
      |]
  

-- This makes persistent use an in memory DB and makes it create the schema in 
-- that DB before we start warp.
main = withSqliteConn ":memory:" run
  where 
    run conn = do 
      runSqlConn (runMigration migrateAll) conn
      warpDebug 3002 (Notes conn )

