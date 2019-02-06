# MySQL database setup
#
# First, create your "system" database (with whatever name you like), and change to that.
#
# Copy this file, and edit the copy to fill in all of the places marked TODO appropriately.
#
# Then feed this file into MySQL, to set up the system database.

CREATE TABLE User (
    id bigint NOT NULL,
    name varchar(255) DEFAULT NULL,
    level tinyint,
    join_date datetime DEFAULT NULL,
    tosVersion int DEFAULT 0,
    userVersion int DEFAULT 0,
    lastNoteChecked int DEFAULT 0,
    PRIMARY KEY (id)
) DEFAULT CHARSET=utf8;

INSERT INTO User (id, name, level) VALUES (9, 'system', 100);
# TODO: fill in the name of your initial admin user in place of the -admin- below:
INSERT INTO User (id, name, level) VALUES (11, '-admin-', 10);

CREATE TABLE OIDNexter (
    nextId int NOT NULL,
    shard int DEFAULT 1
) DEFAULT CHARSET=utf8;

INSERT INTO OIDNexter (nextId) VALUES (0);

CREATE TABLE Spaces (
    id bigint NOT NULL,
    shard int NOT NULL,
    name varchar(255) NOT NULL,
    display varchar(255) NOT NULL,
    owner bigint NOT NULL,
    size int NOT NULL,
    version int DEFAULT 1,
    status int DEFAULT 0,
    PRIMARY KEY (id)
) DEFAULT CHARSET=utf8;
CREATE INDEX spaces_by_owner ON Spaces (owner);

CREATE TABLE Identity (
    id bigint NOT NULL,
    name varchar(255) NOT NULL,
    userId bigint DEFAULT NULL,
    kind int NOT NULL,
    provider int DEFAULT NULL,
    handle varchar(255) DEFAULT NULL,
    email varchar(255) DEFAULT NULL,
    authentication varchar(255) DEFAULT NULL,
    PRIMARY KEY (id)
) DEFAULT CHARSET=utf8;
CREATE INDEX identity_by_email ON Identity (email);
CREATE INDEX identity_by_name ON Identity (name);

# TODO: fill in the system and admin user email addresses below. These
# do *not* need to be real - they won't be used much.
# TODO: fill in the name of the admin user below.
# TODO: compute passwords for the system and admin users, and place them below.
# To compute passwords, go into the Scala REPL or the sbt console,
# import querki.security.EncryptionUtil._
# and call calcHash(-password-, -iterations-),
# where -password- is your desired password and -iterations- is the value
# of querki.security.hashIterations in application.conf (default 20000).
# That will provide the String to place below:
INSERT INTO Identity (id, handle, name, userId, kind, email, authentication)
  VALUES (97, "systemUser", "System User", 9, 2, "-systemEmail-", "-systemPassword-");
INSERT INTO Identity (id, handle, name, userId, kind, email, authentication)
  VALUES (98, "mark", "-adminDisplayName-", 11, 2, "-adminEmail-", "-adminPassword-");

CREATE TABLE SpaceMembership (
  identityId bigint NOT NULL,
  spaceId bigint NOT NULL,
  membershipState tinyint DEFAULT 0,
  lastAccess datetime DEFAULT NULL,
  nickname VARCHAR(255) DEFAULT NULL,
  PRIMARY KEY (identityId, spaceId)
) DEFAULT CHARSET=utf8;

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
