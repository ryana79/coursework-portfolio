-- 1. Create the database schema

CREATE TABLE students (
    sid     INT PRIMARY KEY,
    name    VARCHAR(100),
    age     INT,
    gpa     FLOAT
);

CREATE TABLE courses (
    cid     VARCHAR(20) PRIMARY KEY,
    cname   VARCHAR(100),
    deptid  VARCHAR(50)
);

CREATE TABLE professors (
    ssn     INT PRIMARY KEY,
    pname   VARCHAR(100),
    address VARCHAR(200),
    phone   VARCHAR(30),
    deptid  VARCHAR(50)
);

CREATE TABLE sections (
    cid     VARCHAR(20),
    section INT,
    ssn     INT,
    PRIMARY KEY (cid, section),
    FOREIGN KEY (cid) REFERENCES courses(cid),
    FOREIGN KEY (ssn) REFERENCES professors(ssn)
);

CREATE TABLE enrollment (
    sid     INT,
    cid     VARCHAR(20),
    section INT,
    grade   CHAR(1),
    PRIMARY KEY (sid, cid, section),
    FOREIGN KEY (sid) REFERENCES students(sid),
    FOREIGN KEY (cid, section) REFERENCES sections(cid, section)
);


-- 2. Find the name of professors that work for the cs department.

SELECT pname
FROM professors
WHERE deptid = 'cs';


-- 3. Find those students (sid) enrolled in courses in the cs department.


SELECT DISTINCT e.sid
FROM enrollment e
JOIN courses c ON e.cid = c.cid
WHERE c.deptid = 'cs';


-- 4. List ssn and name of professors that work for the cs department,
--    but are not teaching any cs courses.


SELECT p.ssn, p.pname
FROM professors p
WHERE p.deptid = 'cs'
  AND p.ssn NOT IN (
      SELECT s.ssn
      FROM sections s
      JOIN courses c ON s.cid = c.cid
      WHERE c.deptid = 'cs'
  );


-- 5. List the number of courses offered by each department.
--    Just the number of courses (not sections).

SELECT deptid, COUNT(*) AS num_courses
FROM courses
GROUP BY deptid;


-- 6. List those departments that offer more than 10 courses.

SELECT deptid
FROM courses
GROUP BY deptid
HAVING COUNT(*) > 10;

-- 7. Produce a list of the names of those students whose professor’s
--    name starts with an M. No duplicates.

SELECT DISTINCT st.name
FROM students st
JOIN enrollment e ON st.sid = e.sid
JOIN sections s ON e.cid = s.cid AND e.section = s.section
JOIN professors p ON s.ssn = p.ssn
WHERE p.pname LIKE 'M%';


-- 8. For each department, count small / medium / large sections.
--    small  < 30
--    medium >=30 and <80
--    large  >=80

WITH section_sizes AS (
    SELECT c.deptid,
           s.cid,
           s.section,
           COUNT(e.sid) AS num_students
    FROM sections s
    JOIN courses c ON s.cid = c.cid
    LEFT JOIN enrollment e
      ON s.cid = e.cid AND s.section = e.section
    GROUP BY c.deptid, s.cid, s.section
)
SELECT deptid,
       SUM(CASE WHEN num_students < 30 THEN 1 ELSE 0 END) AS small,
       SUM(CASE WHEN num_students >= 30 AND num_students < 80 THEN 1 ELSE 0 END) AS medium,
       SUM(CASE WHEN num_students >= 80 THEN 1 ELSE 0 END) AS large
FROM section_sizes
GROUP BY deptid;



-- 9. List professors that work for departments with more than 20 faculty
--    members and that offer more large sections than small and medium
--    sections combined.

WITH dept_faculty AS (
    SELECT deptid, COUNT(*) AS faculty_count
    FROM professors
    GROUP BY deptid
),
section_sizes AS (
    SELECT c.deptid,
           s.cid,
           s.section,
           COUNT(e.sid) AS num_students
    FROM sections s
    JOIN courses c ON s.cid = c.cid
    LEFT JOIN enrollment e
      ON s.cid = e.cid AND s.section = e.section
    GROUP BY c.deptid, s.cid, s.section
),
dept_section_counts AS (
    SELECT deptid,
           SUM(CASE WHEN num_students < 30 THEN 1 ELSE 0 END) AS small,
           SUM(CASE WHEN num_students >= 30 AND num_students < 80 THEN 1 ELSE 0 END) AS medium,
           SUM(CASE WHEN num_students >= 80 THEN 1 ELSE 0 END) AS large
    FROM section_sizes
    GROUP BY deptid
)
SELECT p.ssn, p.pname
FROM professors p
JOIN dept_faculty df ON p.deptid = df.deptid
JOIN dept_section_counts dsc ON p.deptid = dsc.deptid
WHERE df.faculty_count > 20
  AND dsc.large > (dsc.small + dsc.medium);

-- 10. For each course section, find the percentage of students that failed
--     (D or F).

SELECT e.cid,
       e.section,
       100.0 * SUM(CASE WHEN e.grade IN ('D', 'F') THEN 1 ELSE 0 END) / COUNT(*) AS fail_percentage
FROM enrollment e
GROUP BY e.cid, e.section;


-- 11. Find the name of the professor with the maximum percentage of
--     students that failed his course.

WITH section_fail AS (
    SELECT s.ssn,
           s.cid,
           s.section,
           100.0 * SUM(CASE WHEN e.grade IN ('D', 'F') THEN 1 ELSE 0 END) / COUNT(*) AS fail_percentage
    FROM sections s
    JOIN enrollment e
      ON s.cid = e.cid AND s.section = e.section
    GROUP BY s.ssn, s.cid, s.section
)
SELECT p.pname
FROM professors p
JOIN section_fail sf ON p.ssn = sf.ssn
WHERE sf.fail_percentage = (
    SELECT MAX(fail_percentage)
    FROM section_fail
);


-- 12. On average what percentage of students fail a course?
--     (total failed / total enrolled)

SELECT 100.0 * SUM(CASE WHEN grade IN ('D', 'F') THEN 1 ELSE 0 END) / COUNT(*) AS avg_fail_percentage
FROM enrollment;


-- 13. Find course sections where the percentage of students with D or F
--     is greater than average.

WITH section_fail AS (
    SELECT cid,
           section,
           100.0 * SUM(CASE WHEN grade IN ('D', 'F') THEN 1 ELSE 0 END) / COUNT(*) AS fail_percentage
    FROM enrollment
    GROUP BY cid, section
),
overall_avg AS (
    SELECT 100.0 * SUM(CASE WHEN grade IN ('D', 'F') THEN 1 ELSE 0 END) / COUNT(*) AS avg_fail_percentage
    FROM enrollment
)
SELECT sf.cid, sf.section, sf.fail_percentage
FROM section_fail sf, overall_avg oa
WHERE sf.fail_percentage > oa.avg_fail_percentage;


-- 14. Produce:
--     deptid | SPS | %A | %B | %C | %D | %F
--     SPS = average number of students per section for the department

WITH section_sizes AS (
    SELECT c.deptid,
           s.cid,
           s.section,
           COUNT(e.sid) AS num_students
    FROM sections s
    JOIN courses c ON s.cid = c.cid
    LEFT JOIN enrollment e
      ON s.cid = e.cid AND s.section = e.section
    GROUP BY c.deptid, s.cid, s.section
),
dept_sps AS (
    SELECT deptid,
           AVG(num_students * 1.0) AS SPS
    FROM section_sizes
    GROUP BY deptid
),
dept_grades AS (
    SELECT c.deptid,
           100.0 * SUM(CASE WHEN e.grade = 'A' THEN 1 ELSE 0 END) / COUNT(*) AS pctA,
           100.0 * SUM(CASE WHEN e.grade = 'B' THEN 1 ELSE 0 END) / COUNT(*) AS pctB,
           100.0 * SUM(CASE WHEN e.grade = 'C' THEN 1 ELSE 0 END) / COUNT(*) AS pctC,
           100.0 * SUM(CASE WHEN e.grade = 'D' THEN 1 ELSE 0 END) / COUNT(*) AS pctD,
           100.0 * SUM(CASE WHEN e.grade = 'F' THEN 1 ELSE 0 END) / COUNT(*) AS pctF
    FROM enrollment e
    JOIN courses c ON e.cid = c.cid
    GROUP BY c.deptid
)
SELECT ds.deptid,
       ds.SPS,
       dg.pctA AS "%A",
       dg.pctB AS "%B",
       dg.pctC AS "%C",
       dg.pctD AS "%D",
       dg.pctF AS "%F"
FROM dept_sps ds
JOIN dept_grades dg ON ds.deptid = dg.deptid;