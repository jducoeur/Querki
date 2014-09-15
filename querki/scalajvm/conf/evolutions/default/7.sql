# This update deals with Terms of Service, and is pretty simple.

# The most recent version of the ToS that this User has agreed to.
ALTER TABLE User ADD COLUMN tosVersion int DEFAULT 0;
