module Database.Operation where

newtype Operation a = Operation Query

run :: FromRow a => Postgres.Connection -> Operation a -> IO a
run conn (Operation q) = query conn q
