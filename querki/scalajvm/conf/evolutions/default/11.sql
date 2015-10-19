# Create the Akka Persistence table
# Note that this is taken from https://github.com/okumin/akka-persistence-sql-async

CREATE TABLE IF NOT EXISTS persist_journal (
  persistence_id VARCHAR(255) NOT NULL,
  sequence_nr BIGINT NOT NULL,
  marker VARCHAR(255) NOT NULL,
  message BLOB NOT NULL,
  created_at TIMESTAMP NOT NULL,
  PRIMARY KEY (persistence_id, sequence_nr)
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS persist_snapshot (
  persistence_id VARCHAR(255) NOT NULL,
  sequence_nr BIGINT NOT NULL,
  created_at BIGINT NOT NULL,
  snapshot BLOB NOT NULL,
  PRIMARY KEY (persistence_id, sequence_nr)
) ENGINE=InnoDB;
