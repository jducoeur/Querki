# This update adds the notion of "status" to Spaces, so that we can add Archiving

ALTER TABLE Spaces
  ADD COLUMN status int DEFAULT 0; 
