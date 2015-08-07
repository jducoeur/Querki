# The focus this time is on refining Identity, and getting the relationships between
# Identity and Spaces correct.

# Originally, NULL wasn't allowed in User.name. But we're now deprecating this column: 
ALTER TABLE User MODIFY name varchar(255) DEFAULT NULL; 

# Reverse the semantics of name and handle, so they make more sense:
UPDATE Identity SET handle="systemUser", name="System User" WHERE id=97;
UPDATE Identity SET handle="mark", name="Mark Waks" WHERE id=98;
UPDATE Identity SET handle="jducoeur", name="Justin du Coeur" WHERE id=99;

# Index Spaces by owner -- otherwise, the following call (quite reasonably) fails:
CREATE INDEX spaces_by_owner ON Spaces (owner);

# Change all the Spaces to point to the Identity instead of the User:
# systemUser:
UPDATE Spaces SET owner=97 WHERE owner=9;
# mark:
UPDATE Spaces SET owner=98 WHERE owner=11;
# jducoeur:
UPDATE Spaces SET owner=99 WHERE owner=31;

# Table to keep track of which Spaces I am a Member of. Note that this is
# basically just a join table, so I haven't bothered with synthetic IDs.
CREATE TABLE SpaceMembership (
  identityId bigint NOT NULL,
  spaceId bigint NOT NULL,
  membershipState tinyint DEFAULT 0,
  lastAccess datetime DEFAULT NULL,
  nickname VARCHAR(255) DEFAULT NULL,
  PRIMARY KEY (identityId, spaceId)
) DEFAULT CHARSET=utf8;
