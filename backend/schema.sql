CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TYPE user_role AS ENUM ('teacher', 'student');

CREATE TABLE "user"
  ( "id" uuid PRIMARY KEY
  , "name" text NOT NULL
  , "role" user_role NOT NULL
  );
