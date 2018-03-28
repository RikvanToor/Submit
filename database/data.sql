INSERT INTO users VALUES ('notwouter','Not Wouter Swierstra', '9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08'), ('notalejandro', 'Not Alejandro Serrano Mena', '9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08'), ('notrik', 'Not Rik van Toor', '9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08');

INSERT INTO students VALUES ('notrik', 2014);

INSERT INTO teachers VALUES ('notwouter', 'BBG-570'), ('notalejandro', 'BBG-570');

INSERT INTO courses VALUES ('INFOAFP', 'Some stuff about Haskell and Agda. Pretty cool.', 'Advanced Functional Programming');

INSERT INTO teachers_courses VALUES ('notwouter','INFOAFP'), ('notalejandro','INFOAFP');

INSERT INTO following_courses VALUES ('notrik','INFOAFP');

INSERT INTO assignments (coursecode,name,description,deadline,nrofstudents) VALUES ('INFOAFP', 'Cool cat pictures', 'Send me some real cool cat pictures. Like at least three or something.', '2018-04-20'::timestamp, 3);
