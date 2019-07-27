module Main where

import Prelude

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
type QueryRequest a = {
  method :: String,
  arguments :: a
}

newtype Response a = Response (Either String a)

newtype Id = Id Int

derive newtype instance writeForeignId :: JSON.WriteForeign Id

data Which
  = AllTorrents
  | Ids (NonEmptyArray Id)

torrentGet :: forall a. Which -> Query String
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
data Fields = Fields {
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
    and (Right res) = log $ show $ res

  Aff.runAff_ and $ do
    { result } <- do
      { newToken } <- runQuery (torrentGet AllTorrents) dummyToken
      runQuery (torrentGet AllTorrents) newToken
    pure result

query :: forall a. JSON.WriteForeign a => QueryRequest a -> Query String
query q = Query \tok -> do
  qRes <- query_ q tok

  -- TODO: handle HTTP errors
  --  * 405 method not allowed
  --  * …
  Tuple newTok res <- case qRes of
    Success a -> pure $ Tuple tok a
    OtherHttpError resp -> pure $ Tuple tok resp
    CSRFTokenInvalid { newToken } -> do
      qRes' <- query_ q newToken
      case qRes' of
        CSRFTokenInvalid _ ->
          Aff.throwError $ Aff.error "transmission query: renewed CSRFToken was also invalid!"
        Success a -> pure $ Tuple newToken a
        OtherHttpError resp -> pure $ Tuple newToken resp

  body <- M.text res
  let head = M.headers res
  pure $ { result: (show head <> "\n" <> body), newToken: newTok }


data QueryHttpResult a
  = Success a
  | CSRFTokenInvalid {
     newToken :: CSRFToken
  }
  | OtherHttpError M.Response

-- TODO: add test for the csrf token stuff
query_ :: forall a. JSON.WriteForeign a => QueryRequest a -> CSRFToken -> Aff (QueryHttpResult M.Response)
query_ qt tok = do
  resp <- queryRaw qt tok
  case M.statusCode resp of
    200 -> pure $ Success resp
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
