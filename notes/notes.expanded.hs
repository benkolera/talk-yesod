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
import qualified Data.Foldable
import Text.Blaze.Internal ( preEscapedText )

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
  defaultLayout $ do 
    toWidget $ ( preEscapedText . pack) "<h1>Notes</h1><ul>"
    Data.Foldable.mapM_
      (\ (Entity id note ) -> do 
          toWidget $ (preEscapedText . pack) "<li><a href=\""
          ((lift getUrlRenderParams)
           >>=
           (\ urender -> toWidget (toHtml (urender (NoteR id) [] ))))
          toWidget $ (preEscapedText . pack) "\">"
          toWidget (toHtml (noteTitle note))
          toWidget $ (preEscapedText . pack) "</a></li>" )
      notes
    toWidget $ (preEscapedText . pack) 
      "</ul><h1>Create Note</h1><form method=\"post\" action=\""
    ((lift getUrlRenderParams)
     >>=
     (\ urender -> toWidget (toHtml (urender NotesR [] ))))
    toWidget $ (preEscapedText . pack) "\" enctype=\""
    toWidget $ toHtml encType
    toWidget $ (preEscapedText . pack) "\">"
    toWidget widget
    toWidget $ (preEscapedText . pack) "<input type=\"submit\"></form>"

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
      warpDebug 3002 (Notes conn )

