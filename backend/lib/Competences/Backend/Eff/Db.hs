module Competences.Backend.Eff.Db
  ( Db (..)
  , DbSettings (..)
  , runDb
  )
where

import Data.Int (Int64)
import Data.Pool (Pool, PoolConfig, defaultPoolConfig, newPool, withResource)
import Data.Word (Word16)
import Database.PostgreSQL.Simple
  ( ConnectInfo (..)
  , Connection
  , FromRow
  , Query
  , ToRow
  , close
  , connect
  , execute
  , query
  )
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpretWith)

-- | Database effect.
--
-- We only provide two operations: 'Select' and 'Execute'.
-- I think, I do not need transactions, as most operations should be very simple.
data Db :: Effect where
  Select :: (ToRow q, FromRow r) => Query -> q -> Db m [r]
  Execute :: (ToRow q) => Query -> q -> Db m Int64

type instance DispatchOf Db = 'Dynamic

-- | Database connection pool settings.
data DbSettings
  = DbSettings
  { dbHost :: !String
  -- ^ Hostname of the database server.
  , dbPort :: !Word16
  -- ^ Port number of the database server.
  , dbUser :: !String
  -- ^ Username for the database connection.
  , dbPassword :: !String
  -- ^ Password for the database connection.
  , dbDatabase :: !String
  -- ^ Name of the database to connect to.
  , poolRetentionTime :: !Double
  -- ^ Time in seconds to keep unused connections open. 0.5 is the smallest possible value.
  , poolMaxConnections :: !Int
  -- ^ Maximum number of connections in the pool.
  }
  deriving (Eq, Show)

-- | Runs the 'Db' effect with the given database settings.
runDb :: forall es a. (IOE :> es) => DbSettings -> Eff (Db ': es) a -> Eff es a
runDb s a = withDbConnPool $ \pool ->
  let withConn :: forall b. (Connection -> IO b) -> Eff es b
      withConn = liftIO . withResource pool
   in interpretWith a $ \_ -> \case
        Select q p -> withConn $ \conn -> query conn q p
        Execute q p -> withConn $ \conn -> execute conn q p
  where
    withDbConnPool :: (IOE :> es) => (Pool Connection -> Eff es a) -> Eff es a
    withDbConnPool a' = do
      pool <- liftIO $ newPool poolConfig
      a' pool
    poolConfig :: PoolConfig Connection
    poolConfig = defaultPoolConfig mkConnection close s.poolRetentionTime s.poolMaxConnections
    mkConnection :: IO Connection
    mkConnection = connect $ ConnectInfo s.dbHost s.dbPort s.dbUser s.dbPassword s.dbDatabase

