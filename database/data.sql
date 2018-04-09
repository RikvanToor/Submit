--
-- PostgreSQL database dump
--

-- Dumped from database version 10.3
-- Dumped by pg_dump version 10.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Data for Name: course; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.course (id, coursecode, description, coursename) VALUES (1, 'INFOAFP', 'Haskell and Agda, but _advanced_', 'Advanced Functional Programming');
INSERT INTO public.course (id, coursecode, description, coursename) VALUES (2, 'INFOMBD', 'In this course you will probably approximately learn about probably approximately correct learning.', 'Big Data');
INSERT INTO public.course (id, coursecode, description, coursename) VALUES (3, 'INFOFP', 'Haskell, but not quite as _advanced_.', 'Functioneel Programmeren');


--
-- Data for Name: assignment; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.assignment (id, coursecode, name, description, deadline, nrofstudents) VALUES (1, 1, 'Solve P=NP', 'Just do it man. Come on', '2018-04-10 00:00:00+02', 1);
INSERT INTO public.assignment (id, coursecode, name, description, deadline, nrofstudents) VALUES (2, 1, 'Cool cat pictures', 'Send me some cool cat pictures.', '2018-04-10 00:00:00+02', 1);
INSERT INTO public.assignment (id, coursecode, name, description, deadline, nrofstudents) VALUES (3, 3, 'Hello World', 'Write Hello world in Haskell', '2018-04-10 00:00:00+02', 1);


--
-- Data for Name: user; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public."user" (id, name, username, password) VALUES (1, 'Rik', '4239776', 'test');
INSERT INTO public."user" (id, name, username, password) VALUES (2, 'Wouter', 'wouter', 'test');
INSERT INTO public."user" (id, name, username, password) VALUES (3, 'Alejandro', 'alejandro', 'test');
INSERT INTO public."user" (id, name, username, password) VALUES (4, 'Arno', 'arno', 'test');
INSERT INTO public."user" (id, name, username, password) VALUES (5, 'Ad', 'ad', 'test');


--
-- Data for Name: student; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.student (id, userid, enrollmentyear) VALUES (1, 1, 2014);


--
-- Data for Name: assists; Type: TABLE DATA; Schema: public; Owner: submit
--



--
-- Data for Name: submission; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.submission (id, lastchangedtime, readme, grade, gradedby, assignmentid) VALUES (1, '2018-04-05 05:35:00+02', 'For this project I did a bit of this and a bit of that.', 7.5, 2, 1);


--
-- Data for Name: file; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.file (id, isbinary, filesize, filename, submissionid, filepath) VALUES (1, false, 31, 'Solution.md', 1, 'assets/files/servant-multipart26490-4.buf');


--
-- Data for Name: follows; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.follows (id, studentid, courseid) VALUES (2, 1, 1);
INSERT INTO public.follows (id, studentid, courseid) VALUES (1, 1, 2);


--
-- Data for Name: submits; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.submits (id, studentid, submissionid) VALUES (1, 1, 1);


--
-- Data for Name: teacher; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.teacher (id, userid, office) VALUES (1, 2, 'BBG-570');
INSERT INTO public.teacher (id, userid, office) VALUES (2, 3, 'BBG-570');
INSERT INTO public.teacher (id, userid, office) VALUES (3, 4, 'BBG-562');
INSERT INTO public.teacher (id, userid, office) VALUES (4, 5, 'BBG-563');


--
-- Data for Name: teaches; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.teaches (id, teacherid, courseid) VALUES (3, 1, 1);
INSERT INTO public.teaches (id, teacherid, courseid) VALUES (4, 2, 1);
INSERT INTO public.teaches (id, teacherid, courseid) VALUES (1, 3, 2);
INSERT INTO public.teaches (id, teacherid, courseid) VALUES (2, 4, 2);
INSERT INTO public.teaches (id, teacherid, courseid) VALUES (5, 2, 3);


--
-- Data for Name: user_password; Type: TABLE DATA; Schema: public; Owner: submit
--

INSERT INTO public.user_password (id, userid, password) VALUES (1, 1, 'test');
INSERT INTO public.user_password (id, userid, password) VALUES (2, 2, 'test');
INSERT INTO public.user_password (id, userid, password) VALUES (3, 3, 'test');


--
-- Name: assignment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.assignment_id_seq', 1, false);


--
-- Name: assists_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.assists_id_seq', 1, false);


--
-- Name: course_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.course_id_seq', 2, true);


--
-- Name: file_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.file_id_seq', 8, true);


--
-- Name: follows_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.follows_id_seq', 2, true);


--
-- Name: student_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.student_id_seq', 1, true);


--
-- Name: submission_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.submission_id_seq', 1, false);


--
-- Name: submits_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.submits_id_seq', 1, false);


--
-- Name: teacher_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.teacher_id_seq', 2, true);


--
-- Name: teaches_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.teaches_id_seq', 4, true);


--
-- Name: user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.user_id_seq', 3, true);


--
-- Name: user_password_id_seq; Type: SEQUENCE SET; Schema: public; Owner: submit
--

SELECT pg_catalog.setval('public.user_password_id_seq', 3, true);


--
-- PostgreSQL database dump complete
--

