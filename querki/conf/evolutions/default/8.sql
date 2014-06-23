# This update deals with Notifications. It is simple at the system level, but adds new user tables at runtime.

# This User's version; very similar to the version number on Space.
ALTER TABLE User ADD COLUMN userVersion int DEFAULT 0;
