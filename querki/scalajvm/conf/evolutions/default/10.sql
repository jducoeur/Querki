# Add the Apps table. This is a join table for the many-to-many relationship
# between Spaces and their Apps.
# 
# Columns:
#   spaceId -- the Space inheriting this App
#   appId -- the App being inherited from
#   appVersion -- the *data* version of this App. This is reserved for future use:
#     once we have History working, the Space should be pinned to a specific version
#     of the App, and should upgrade voluntarily.
#   position -- the 0-based index of this App in this Space's App List.

CREATE TABLE Apps (
  space_id bigint NOT NULL,
  app_id bigint NOT NULL,
  app_version bigint DEFAULT 0,
  position int DEFAULT 0,
  PRIMARY KEY (space_id, app_id),
  KEY spaceKey (space_id),
  KEY appKey (app_id),
  CONSTRAINT space_rel FOREIGN KEY (space_id) REFERENCES Spaces (id) ON DELETE CASCADE,
  CONSTRAINT app_rel FOREIGN KEY (app_id) REFERENCES Spaces (id) ON DELETE CASCADE
) DEFAULT CHARSET=utf8;
