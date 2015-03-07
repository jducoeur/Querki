# Change the OIDNexter to hold the Shard ID for this database. That's really correct, since the
# database *is* the shard in all important respects.
#
# IMPORTANT!!!!!: you must *manually* define the shard ID for each database!!! We might someday
# get to the point of having automated processes to roll out a new shard, but that's a *long*
# ways off.

# --- !Ups

ALTER TABLE OIDNexter
  ADD COLUMN shard int DEFAULT 1; 

# --- !Downs

ALTER TABLE OIDNexter
 DROP COLUMN shard;
