# Users schema
# Essentially trivial at this point, just so we can begin to use
# real Spaces.
# TODO: Security! Authentication! Authorization! etc...
# TODO: Spaces.owner needs to be indexed
 
# --- !Ups
 
CREATE TABLE User (
    id bigint NOT NULL,
    name varchar(255) NOT NULL,
    PRIMARY KEY (id)
) DEFAULT CHARSET=utf8;

INSERT INTO User (id, name) VALUES (9, 'system');
INSERT INTO User (id, name) VALUES (11, 'mark');
INSERT INTO User (id, name) VALUES (31, 'jducoeur');

CREATE TABLE OIDNexter (
    nextId int NOT NULL
) DEFAULT CHARSET=utf8;

INSERT INTO OIDNexter (nextId) VALUES (0);

CREATE TABLE Spaces (
    id bigint NOT NULL,
    shard int NOT NULL,
    name varchar(255) NOT NULL,
    display varchar(255) NOT NULL,
    owner bigint NOT NULL,
    size int NOT NULL,
    PRIMARY KEY (id)
) DEFAULT CHARSET=utf8;
 
# --- !Downs
 
DROP TABLE Spaces;
DROP TABLE OIDNexter;
DROP TABLE User;
DROP ALL OBJECTS;
