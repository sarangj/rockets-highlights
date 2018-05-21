module Entities.StreamRun where

import Database.Entity

type StreamRun = Entity ()

streamRunConfig :: EntityConfig StreamRun
streamRunConfig = EntityConfig
  { entity_table = Table
    { table_name = "stream_run"
    , table_customColumns = []
    }


