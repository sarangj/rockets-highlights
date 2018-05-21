module Database.Entity where

newtype IdOf a = IdOf Int

instance FromRow (IdOf a) where
  fromRow = EntityID <$> field

type EntityID a = IdOf (Entity a)

data Entity a = Entity 
  { entity_id :: EntityID a
  , entity_timestamp :: UTCTime
  , entity_customData :: a
  }

data EntityConfig a = EntityConfig
  { entity_tableName :: Text
  , entity_customColumns :: [Text]
  }

fetch :: EntityConfig a -> EntityID a -> Operation (Entity a)
fetch EntityConfig{..} id = Operation $ query "SELECT * FROM ? WHERE id = ?"
