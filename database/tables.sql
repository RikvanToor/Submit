CREATE TABLE users(
    solisid VARCHAR(100),
    name VARCHAR(200) NOT NULL,
    password VARCHAR(64) NOT NULL,
    PRIMARY KEY (solisid)
);

CREATE TABLE students(
    solisid VARCHAR(100),
    enrollmentyear SMALLINT CHECK (enrollmentyear <= date_part('year', CURRENT_DATE)) NOT NULL,
    PRIMARY KEY (solisid),
    FOREIGN KEY (solisid) REFERENCES users (solisid)
);

CREATE TABLE teachers(
    solisid VARCHAR(100),
    office VARCHAR(50),
    PRIMARY KEY (solisid),
    FOREIGN KEY (solisid) REFERENCES users (solisid)
);

CREATE TABLE courses(
    coursecode VARCHAR(10),
    description TEXT NOT NULL,
    coursename VARCHAR(100) NOT NULL,
    PRIMARY KEY (coursecode)
);

CREATE TABLE teachers_courses(
    teacherid VARCHAR(100),
    courseid VARCHAR(10),
    FOREIGN KEY (teacherid) REFERENCES teachers (solisid),
    FOREIGN KEY (courseid) REFERENCES courses (coursecode),
    PRIMARY KEY (teacherid, courseid)
);

CREATE TABLE following_courses(
    studentid VARCHAR(100),
    courseid VARCHAR(10),
    FOREIGN KEY (studentid) REFERENCES students (solisid),
    FOREIGN KEY (courseid) REFERENCES courses (coursecode),
    PRIMARY KEY (studentid, courseid)
);

CREATE TABLE assisting_courses(
    assistantid VARCHAR(100),
    courseid VARCHAR(10),
    FOREIGN KEY (assistantid) REFERENCES students (solisid),
    FOREIGN KEY (courseid) REFERENCES courses (coursecode),
    PRIMARY KEY (assistantid, courseid)
);

CREATE TABLE assignments(
    assignmentid SERIAL,
    coursecode VARCHAR(10) NOT NULL,
    name VARCHAR(100) NOT NULL,
    description TEXT NOT NULL,
    deadline TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    nrofstudents SMALLINT CHECK (nrofstudents > 0) NOT NULL,
    PRIMARY KEY (assignmentid),
    FOREIGN KEY (coursecode) REFERENCES courses (coursecode)
);

CREATE TABLE submissions(
    submissionid SERIAL,
    lastchangedtime TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    readme TEXT,
    grade REAL CHECK (grade >= 0 AND grade <= 10),
    assignmentid SERIAL NOT NULL,
    gradedby VARCHAR(100), 
    FOREIGN KEY (gradedby) REFERENCES users(solisid),
    FOREIGN KEY (assignmentid) REFERENCES assignments (assignmentid),
    PRIMARY KEY (submissionid)
);

CREATE TABLE students_submissions(
    studentsid VARCHAR(100),
    submissionid SERIAL,
    FOREIGN KEY (studentsid) REFERENCES students (solisid),
    FOREIGN KEY (submissionid) REFERENCES submissions(submissionid),
    PRIMARY KEY (studentsid, submissionid)
);

CREATE TABLE files(
    filepath VARCHAR(200),
    filesize BIGINT, -- in bytes
    filename VARCHAR(100),
    submissionid SERIAL,
    FOREIGN KEY (submissionid) REFERENCES submissions(submissionid),
    PRIMARY KEY (filepath)
);
