module Main where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array (filter)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Console as Console
import Foreign as Foreign
import Foreign.Object (Object)
import Foreign.Object as Object
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Prim.RowList (class RowToList)
import Simple.JSON as JSON
import Undefined (undefined)

import Dhall.Generic as DG

newtype Query a = Query (CSRFToken -> Aff { newToken :: CSRFToken, result :: a })

runQuery :: forall a. Query a -> CSRFToken -> Aff { newToken :: CSRFToken, result :: a }
runQuery (Query f) tok = f tok

derive instance functorQuery :: Functor Query

-- A (json) request as expected by the transmission API
type QueryRequest a =
  { method :: String
  , arguments :: a
  }

data QueryResponse a
  = QueryResponseError String
  | QueryResponseSuccess a

data WithResponseData a = WithResponseData a

data NoData = NoData

instance readForeignQueryResponseNoData :: JSON.ReadForeign (QueryResponse NoData) where
  readImpl f = do
    s <- JSON.readImpl f :: Foreign.F { result :: String }
    pure $ case s.result of
      "success" -> do
        QueryResponseSuccess NoData
      err -> QueryResponseError err

instance readForeignQueryResponse :: (RowToList fields fieldList, JSON.ReadForeignFields fieldList () fields) => JSON.ReadForeign (QueryResponse { | fields } ) where
  readImpl f = do
    s <- JSON.readImpl f :: Foreign.F { result :: String }
    case s.result of
      "success" -> do
        dat <- JSON.readImpl f :: Foreign.F { arguments :: { | fields } }
        pure $ QueryResponseSuccess dat.arguments
      err -> pure $ QueryResponseError err

newtype Response a = Response (Either String a)

newtype Id = Id Int

derive newtype instance writeForeignId :: JSON.WriteForeign Id
derive newtype instance readForeignId :: JSON.ReadForeign Id
derive newtype instance showId :: Show Id

data Which
  = AllTorrents
  | Ids (Array Id)

type TorrentGetResponse =
  { torrents :: Array Fields
    -- TODO
  }

torrentGet :: Which -> Query (QueryResponse TorrentGetResponse)
torrentGet which =
  query
    { method: "torrent-get"
    , arguments:
      { fields: requestedFields
      , ids: case which of
          AllTorrents -> Nothing
          Ids ids -> Just ids
      }
    }

data JsonVal
  = JsonBoolean Boolean
  | JsonNumber Number
  | JsonString String
  | JsonObject (Object JsonVal)
  | JsonArray (Array JsonVal)

instance readForeign :: JSON.ReadForeign JsonVal where
  readImpl f =
        JsonBoolean <$> JSON.readImpl f
    <|> JsonNumber <$> JSON.readImpl f
    <|> JsonString <$> JSON.readImpl f
    <|> JsonObject <$> JSON.readImpl f
    <|> JsonArray <$> JSON.readImpl f

instance showJsonVal :: Show JsonVal where
  show (JsonBoolean b) = show b
  show (JsonNumber n) = show n
  show (JsonString s) = show s
  show (JsonObject o) = show o
  show (JsonArray a) = show a

-- don’t forget to update requestedFields
type Fields =
  { id :: Id
  , isStalled :: Boolean
  , name :: String
  , status :: Status
  , downloadDir :: String
  , hashString :: String
  , files :: Array { bytesCompleted :: Number, length :: Number, name :: String }
  , fileStats :: Array { bytesCompleted :: Number, wanted :: Boolean{-, priority :: Priority-} }
  }

-- For now we’ll just gonna list all of the manually.
-- Late, do some row polymorphism magic to determine the
-- response type from the type-level list of fields
requestedFields :: Array String
requestedFields = [ "id", "isStalled", "name", "status", "downloadDir", "hashString", "files", "fileStats" ]


data Status
  -- torrent is stopped
  = StatusStopped
  -- queued to check files
  | StatusCheckWait
  -- checking files
  | StatusCheck
  -- queued to download
  | StatusDownloadWait
  -- downloading
  | StatusDownload
  -- queued to seed
  | StatusSeedWait
  -- seeding
  | StatusSeed
  -- unknown (check transmission.h, tr_torrent_activity
  | StatusUnknown

instance readForeignStatus :: JSON.ReadForeign Status where
  readImpl f = do
    i <- JSON.readImpl f :: Foreign.F Int
    pure case i of
      0 -> StatusStopped
      1 -> StatusCheckWait
      2 -> StatusCheck
      3 -> StatusDownloadWait
      4 -> StatusDownload
      5 -> StatusSeedWait
      6 -> StatusSeed
      _ -> StatusUnknown

instance showStatus :: Show Status where
    show StatusStopped = "StatusStopped"
    show StatusCheckWait = "StatusCheckWait"
    show StatusCheck = "StatusCheck"
    show StatusDownloadWait = "StatusDownloadWait"
    show StatusDownload = "StatusDownload"
    show StatusSeedWait = "StatusSeedWait"
    show StatusSeed = "StatusSeed"
    show StatusUnknown = "StatusUnknown"

derive instance eqStatus :: Eq Status

newtype CSRFToken = CSRFToken String
dummyToken :: CSRFToken
dummyToken = CSRFToken "dummyToken"

main :: Effect Unit
main = do
  {-
  let
    and (Left err) = log $ show err
    and (Right (QueryResponseError err)) = log $ show $ err
    and (Right (QueryResponseSuccess res)) = log $ show $ res

  Aff.runAff_ and $ do
    { result } <- do
      -- { newToken, result: QueryResponseSuccess result } <- runQuery (torrentGet AllTorrents) dummyToken
      { newToken, result } <- runQuery (torrentGet AllTorrents) dummyToken
      case result of
        QueryResponseError _ -> pure { result, newToken }
        QueryResponseSuccess res -> do
          let unfinished = filter (\t -> t.status /= StatusSeed) res.torrents
          -- liftEffect $ log $ show unfinished
          runQuery (torrentGet $ Ids $ map (\t -> t.id) unfinished) newToken
    pure result
  -}

  log $ DG.printTextual (DG.toExp { a: "hi" , b: "lea", tiglenleilelitgeniletgn: "itgenltegnleeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" } :: DG.Exp)

query :: forall a b. JSON.WriteForeign a => JSON.ReadForeign (QueryResponse b) => QueryRequest a -> Query (QueryResponse b)
query q = Query $ \tok -> do
  qRes <- query_ q tok

  -- TODO: handle HTTP errors
  --  * 405 method not allowed
  --  * …
  case qRes of
    Success a -> pure $ { result: a, newToken: tok }
    OtherHttpError resp ->
      -- TODO: throw to outside, probably important
      Aff.throwError $ Aff.error $ "transmission query: Got http error response!!\n" <> show (M.headers resp)
    BodyDecodeError errs ->
      Aff.throwError $ Aff.error $ "transmission query: unable to decode JSON body" <> show errs
    CSRFTokenInvalid { newToken } -> do
      qRes' <- query_ q newToken
      case qRes' of
        CSRFTokenInvalid _ ->
          Aff.throwError $ Aff.error "transmission query: renewed CSRFToken was also invalid!"
        OtherHttpError resp ->
          -- TODO: throw to outside, probably important
          Aff.throwError $ Aff.error $ "transmission query: Got http error response!!\n" <> show (M.headers resp)
        BodyDecodeError errs ->
          Aff.throwError $ Aff.error $ "transmission query: unable to decode JSON body" <> show errs
        Success a ->pure $ { result: a, newToken }


data QueryHttpResult a
  = Success a
  | CSRFTokenInvalid {
     newToken :: CSRFToken
  }
  | OtherHttpError M.Response
  | BodyDecodeError Foreign.MultipleErrors

-- TODO: add test for the csrf token stuff
query_ :: forall a b. JSON.WriteForeign a => JSON.ReadForeign (QueryResponse b) => QueryRequest a -> CSRFToken -> Aff (QueryHttpResult (QueryResponse b))
query_ qt tok = do
  resp <- queryRaw qt tok
  case M.statusCode resp of
    200 -> do
      body <- M.json resp
      pure $ case runExcept $ JSON.readImpl body of
                  Left errs -> BodyDecodeError errs
                  Right res -> Success res
    409 -> do
      newTok <- case Object.lookup "x-transmission-session-id" $ M.headers resp of
            Nothing -> do
              bodyText <- M.text resp
              -- Nothing we can do, throw an exception
              -- TODO: maybe return more structured value?
              Aff.throwError $ Aff.error
                $ "transmission query: no CSRFToken in this 409 reply! Headers:\n" <> show (M.headers resp)
            Just t -> pure (CSRFToken t)
      pure $ CSRFTokenInvalid { newToken: newTok }
    _ -> pure $ OtherHttpError resp

queryRaw :: forall a. JSON.WriteForeign a => QueryRequest a -> CSRFToken -> Aff M.Response
queryRaw req (CSRFToken tok) = do
  let
    opts =
      { method: M.postMethod
      , headers: M.makeHeaders
          { "X-Transmission-Session-Id": tok }
      , body: JSON.writeJSON req
      }
  -- TODO
  fetch (M.URL "http://localhost:9091/transmission/rpc") opts

fetch = M.fetch nodeFetch
