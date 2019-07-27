module Main where

import Prelude

import Prim.RowList (class RowToList)
import Control.Monad.Except (runExcept)
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
import Simple.JSON as JSON

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

instance readForeignQueryResponseNoData :: JSON.ReadForeign (QueryResponse Unit) where
  readImpl f = do
    s <- JSON.readImpl f :: Foreign.F { result :: String }
    pure $ case s.result of
      "success" -> do
        QueryResponseSuccess unit
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
  | Ids (NonEmptyArray Id)

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
          Ids ids -> Just $ NonEmpty.toArray ids
      }
    }


-- don’t forget to update fields
type Fields = {
  id :: Id
}

-- For now we’ll just gonna list all of the manually.
-- Late, do some row polymorphism magic to determine the
-- response type from the type-level list of fields
requestedFields :: Array String
requestedFields = [ "id", "files" ]

newtype CSRFToken = CSRFToken String
dummyToken :: CSRFToken
dummyToken = CSRFToken "dummyToken"

main :: Effect Unit
main = do
  let
    and (Left err) = log $ show err
    and (Right (QueryResponseError err)) = log $ show $ err
    and (Right (QueryResponseSuccess res)) = log $ show $ res

  Aff.runAff_ and $ do
    { result } <- do
      { newToken } <- runQuery (torrentGet AllTorrents) dummyToken
      runQuery (torrentGet (AllTorrents)) newToken
    pure result

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
