CREATE TABLE stream_run
(
  id SERIAL PRIMARY KEY,
  timestamp TIMESTAMP DEFAULT NOW()
);

CREATE TABLE post
(
  id SERIAL PRIMARY KEY,
  post_id TEXT NOT NULL UNIQUE
);

CREATE TABLE assoc_stream_run_to_post
( 
  stream_run INT REFERENCES stream_run (id),
  post_id INT REFERENCES post (id),
  timestamp TIMESTAMP DEFAULT NOW()
);
