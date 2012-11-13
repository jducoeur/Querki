# Users schema
# Essentially trivial at this point, just so we can begin to use
# real Spaces.
# TODO: Security! Authentication! Authorization! etc...
 
# --- !Ups
 
CREATE TABLE User (
    id bigint(20) NOT NULL,
    name varchar(255) NOT NULL,
    PRIMARY KEY (id)
);

INSERT INTO User (id, name) VALUES (9, 'System');
INSERT INTO User (id, name) VALUES (11, 'Mark');
 
# --- !Downs
 
DROP TABLE User;
