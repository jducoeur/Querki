# Starting to evolve -- this time, we add the Identity table
# This table is very preliminary yet. Several fields are tentative.
# Still need to add, eg, the "noInvites" flag that gets sent when
# someone receives an invitation and asks to not get any more.

# --- !Ups

CREATE TABLE Identity (
    id bigint NOT NULL,
    name varchar(255) NOT NULL,
    userId bigint DEFAULT NULL,
    kind int NOT NULL,
    provider int DEFAULT NULL,
    handle varchar(255) DEFAULT NULL,
    email varchar(255) DEFAULT NULL,
    PRIMARY KEY (id)
) DEFAULT CHARSET=utf8;


# --- !Downs

DROP TABLE Identity;
