# Introduce the notion of Version ID for Spaces. This is important so that
# we can detect when a Space needs to be upgraded.

# --- !Ups

ALTER TABLE Spaces
  ADD COLUMN version int DEFAULT 1; 

# --- !Downs

ALTER TABLE Spaces
 DROP COLUMN version;
