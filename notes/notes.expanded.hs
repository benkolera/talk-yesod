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
import qualified Data.Map
import qualified Yesod.Routes.Dispatch

-- This is written here because Text.Hamlet.maybeH is private
import Data.Maybe (fromMaybe)
import Control.Monad (mplus)
maybeH mv f mm = fromMaybe (return ()) $ fmap f mv `mplus` mm
-- End maybeH

data Notes = Notes { 
  dbConn :: Connection
}


-- You don't really need to see this expanded. Trust me. ;) 
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

instance RenderRoute Notes where
  data Route Notes = NotesR | NoteR NoteId deriving (Show, Eq, Read)
  renderRoute NotesR = ([pack "notes"], [])
  renderRoute (NoteR id) = ([pack "notes", (toPathPiece id)],[])

type Handler = GHandler Notes Notes
type Widget = GWidget Notes Notes ()
instance YesodDispatch Notes Notes where
  yesodDispatch master sub toMaster handler404 handler405 method pieces =
    case dispatch pieces of 
      Just f  -> f master sub toMaster handler404 handler405 method
      Nothing -> handler404
    where
      dispatch = Yesod.Routes.Dispatch.toDispatch
                 [
                   Yesod.Routes.Dispatch.Route
                   [Yesod.Routes.Dispatch.Static (pack "notes")]
                   False
                   handleNotesPieces
                 , Yesod.Routes.Dispatch.Route
                   [Yesod.Routes.Dispatch.Static (pack "notes")
                    , Yesod.Routes.Dispatch.Dynamic]
                   False
                   handleNotePieces
                  ]

      handleNotesPieces [_] = Just handleNotesMethods
      handleNotesPieces _ = error "invariant violated"

      handleNotesMethods master sub toMaster handler404 handler405 method  =
        case Data.Map.lookup method methodsNotesR of 
          Just f -> let handler = f 
                    in
                     yesodRunner handler master sub (Just NotesR) toMaster
          Nothing -> handler405 NotesR 

      handleNotePieces [ _ , id ] = do
        id' <- fromPathPiece id
        Just $ handleNoteMethods id'
      handleNotePieces _ = error "Invariant violated"

      handleNoteMethods id master sub toMaster handler404 handler405 method  =
        case Data.Map.lookup method methodsNoteR of 
          Just f -> let handler = f id
                    in
                     yesodRunner handler master sub (Just (NoteR id)) toMaster
          Nothing -> handler405 (NoteR id) 

      methodsNotesR = Data.Map.fromList
                      [(pack "GET", fmap chooseRep getNotesR),
                       (pack "POST",fmap chooseRep postNotesR)]
      methodsNoteR = Data.Map.fromList
                     [(pack "GET", \ id -> fmap chooseRep (getNoteR id))]

instance Yesod Notes

instance YesodJquery Notes
instance RenderMessage Notes FormMessage where
    renderMessage _ _ = defaultFormMessage

createNoteForm today = renderDivs $ Note 
  <$> areq textField "Title" Nothing
  <*> aopt dueDateField "Date" ( Just ( Just today ) )
  <*> areq textareaField "Body" Nothing
  where 
    dueDateField = jqueryDayField def 
  
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

postNotesR = do 
  today <- fmap utctDay $ liftIO getCurrentTime
  ((result, widget), encType) <- runFormPost $ createNoteForm today
  case result of
    FormSuccess note -> postNotesRWin note
    _ -> showCreateNoteForm widget encType

postNotesRWin note = do
  noteId <- runDB $ insert note
  redirect $ NoteR noteId

getNoteR id = do
  note <- runDB $ get id
  case note of 
    Nothing    -> notFound
    Just ( Note title date body ) -> do 
      defaultLayout $ do
        toWidget ((preEscapedText . pack) "<h1>")
        toWidget (toHtml title)
        toWidget ((preEscapedText . pack) "</h1>");
        maybeH
          date
          (\ date -> do
              toWidget ((preEscapedText . pack) " ( dated: ")
              toWidget (toHtml (show date))
              toWidget ((Text.Blaze.Internal.preEscapedText . pack) " )")
          )
          Nothing
        toWidget ((preEscapedText . pack) "<p>")
        toWidget (toHtml body)
        toWidget ((preEscapedText . pack) "</p>")
  
main = withSqliteConn ":memory:" run
  where 
    run conn = do 
      runSqlConn (runMigration migrateAll) conn
      warpDebug 3002 (Notes conn )

