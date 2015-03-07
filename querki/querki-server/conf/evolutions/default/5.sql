# Note that from here on out, this folder is only for *system* Evolutions. *Space* Evolutions
# happen in code, in the package querki.evolutions.
#
# Also from here on out, we expect these to be hand-applied. So they aren't really "evolutions"
# in the Play sense. That is because of permissions -- we don't want the app to have enough
# rights to be making schema changes.
#
# This file is a bunch of enhancements aimed at getting the invitation system right.


# In practice, email is the most important key to most identities:
CREATE INDEX identity_by_email ON Identity (email);

# But we are also allowing login by name:
CREATE INDEX identity_by_name ON Identity (name);

# Note that neither of the above can be unique, sadly, because duplication of the name
# identifiers with different Identity Kinds is entirely legal, and pretty common.
# So we have to enforce uniqueness in the Identity creation code instead. Eventually,
# we might do so in stored procedures.


# Add the column for passwords:
ALTER TABLE Identity ADD COLUMN authentication varchar(255) DEFAULT NULL;


# Migrate the system logins from config to the DB. Note that you need to generate and fill in the
# passwords by hand for these "bootstrap" identities:
INSERT INTO Identity (id, name, userId, kind, email, authentication)
  VALUES (97, "systemUser", 9, 2, "systemUser@querki.net", -systemPassword-);
INSERT INTO Identity (id, name, userId, kind, email, authentication)
  VALUES (98, "mark", 11, 2, "mark@querki.net", -markPassword-);
INSERT INTO Identity (id, name, userId, kind, email, authentication)
  VALUES (99, "jducoeur", 31, 2, "justin@querki.net", -jducoeurPassword-);


# Introduce the idea of user "levels", for administration and such:
ALTER TABLE User ADD COLUMN level tinyint;
UPDATE User SET level=100 WHERE id=9;
UPDATE User SET level=10 WHERE id=11;
UPDATE User SET level=4 WHERE id=31;


# Keep track of when people joined:
ALTER TABLE User ADD COLUMN join_date datetime DEFAULT NULL;
