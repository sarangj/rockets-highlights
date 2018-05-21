{-# LANGUAGE RecordWildCards #-}
module Database.Edge where

data Edge a b = Edge 
  { edge_from :: EntityConfig a
  , edge_to :: EntityConfig b
  , edge_tableName :: Text
  }

add :: Edge a b -> EntityID a -> EntityID b -> IO ()
add Edge{..} sourceID targetID = execute
  "INSERT INTO ? (id1, id1) VALUES (?, ?)"
  (edge_tableName, source, destination)

traverse :: Edge a b -> EntityID a -> IO (EntityID b) 
traverse Edge{..} sourceID = query 
  "SELECT id2 FROM ? WHERE id1 = ?" 
  (edge_tableName, sourceID) 
