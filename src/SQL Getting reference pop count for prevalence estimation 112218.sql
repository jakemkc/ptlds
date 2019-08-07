/* 1.1. Pick Lyme */
	/* check */
	SELECT TOP 100 *
	FROM jc574.dbo.lyme_010818_2
	ORDER BY MemberId; /*case range: 2008-12-31 to 2013-12-31 */
	/* size: 48 596, time: 20s */ 

/* 1.2. Merge zipcode */
	/* Extract */
	SELECT DISTINCT l.HmsAetnaLineId, l.MemberId, m.EmployeeZipCode
	INTO jc574.dbo.lyme_032718_1
	FROM jc574.dbo.lyme_010818_2 AS l
	INNER JOIN dbo.Enrollment AS m
	ON l.MemberId = m.MemberId;
		/* size: 55 895 , time: 3:45 */
		/* larger size, as people can move. ~15% has this, OK for my purpose */

/* 1.3. New table with zip count */
	SELECT DISTINCT EmployeeZipCode, count(EmployeeZipCode) AS zipcount
	INTO jc574.dbo.lyme_032718_2
	FROM jc574.dbo.lyme_032718_1 AS l
	GROUP BY EmployeeZipCode
	ORDER BY EmployeeZipCode DESC;
		/* size: 7 672 , time: 0:01 */


/* 1.4. New table with zip count of ref pop */
	SELECT DISTINCT l.EmployeeZipCode, m.MemberId, m.EffectiveDate
	INTO jc574.dbo.lyme_032718_3
	FROM jc574.dbo.lyme_032718_2 AS l
	INNER JOIN dbo.Enrollment AS m
	ON l.EmployeeZipCode = m.EmployeeZipCode;
		/* size: 1 052 000 153 , time: 53:08 */


/* 1.5. create min and max date variables */
	SELECT l.*,
		MIN(EffectiveDate) OVER (PARTITION BY MemberId) AS minenrolldate, /* "group by" only get one single aggregate value; "partition by" with row number is diff */
		MAX(EffectiveDate) OVER (PARTITION BY MemberId) AS maxenrolldate
	INTO jc574.dbo.lyme_032718_4
	FROM jc574.dbo.lyme_032718_3 AS l;
		/* size: 1 052 000 153 , time:1:16:24  */



/* 1.6. Filter for the year range */
	/*case range: 2008-12-31 to 2013-12-31 */
	SELECT l.*
	INTO jc574.dbo.lyme_032718_5
	FROM jc574.dbo.lyme_032718_4 AS l
	WHERE 
	DATEDIFF(day, '2008/12/31', minenrolldate) >= 0
	AND DATEDIFF(day, '2013/12/31', maxenrolldate) >= 0;
		/* size: 407 794 622 , time:30:37  */

/* 1.7. zip count table of ref pop */
	SELECT DISTINCT EmployeeZipCode, count(DISTINCT MemberID) AS refpopcount
	INTO jc574.dbo.lyme_032718_6
	FROM jc574.dbo.lyme_032718_5 AS l
	GROUP BY EmployeeZipCode
	ORDER BY EmployeeZipCode DESC;
		/* size: 7667 , time:3:38  */


/* 2. new 112618: modify 1.6 and 1.7 to get count of total enrolled by year */
	/*case range: 2008-12-31 to 2013-12-31 */

/* 2009-2010 */
SELECT l.*
INTO jc574.dbo.lyme_112618_totcount_1
FROM jc574.dbo.lyme_032718_4 AS l
WHERE 
DATEDIFF(day, minenrolldate, '2008/12/31') >= 0
AND DATEDIFF(day, '2009/12/31', maxenrolldate) >= 0;
	/* size: 395 803 252 , time:26 m  */


SELECT DISTINCT EmployeeZipCode AS zip0910, count(DISTINCT MemberID) AS refpopcount0910
INTO jc574.dbo.lyme_112618_totcount_20092010
FROM jc574.dbo.lyme_112618_totcount_1 AS l
GROUP BY EmployeeZipCode
ORDER BY EmployeeZipCode DESC;
	/* size: 7665 , time: 4 m */


/* 2010-2011 */
SELECT l.*
INTO jc574.dbo.lyme_112618_totcount_2
FROM jc574.dbo.lyme_032718_4 AS l
WHERE 
DATEDIFF(day, minenrolldate, '2009/12/31') >= 0
AND DATEDIFF(day, '2010/12/31', maxenrolldate) >= 0;
	/* size: 451398430 , time: 37 m */


SELECT DISTINCT EmployeeZipCode AS zip1011, count(DISTINCT MemberID) AS refpopcount1011
INTO jc574.dbo.lyme_112618_totcount_20102011
FROM jc574.dbo.lyme_112618_totcount_2 AS l
GROUP BY EmployeeZipCode
ORDER BY EmployeeZipCode DESC;
	/* size: 7671 , time:  */


/* 2011-2012 */
SELECT l.*
INTO jc574.dbo.lyme_112618_totcount_3
FROM jc574.dbo.lyme_032718_4 AS l
WHERE 
DATEDIFF(day, minenrolldate, '2010/12/31') >= 0
AND DATEDIFF(day, '2011/12/31', maxenrolldate) >= 0;
	/* size: 516718397 , time: ~40m  */


SELECT DISTINCT EmployeeZipCode AS zip1112, count(DISTINCT MemberID) AS refpopcount1112
INTO jc574.dbo.lyme_112618_totcount_20112012
FROM jc574.dbo.lyme_112618_totcount_3 AS l
GROUP BY EmployeeZipCode
ORDER BY EmployeeZipCode DESC;
	/* size: 7670 , time:  */


/* 2012-2013 */
SELECT l.*
INTO jc574.dbo.lyme_112618_totcount_4
FROM jc574.dbo.lyme_032718_4 AS l
WHERE 
DATEDIFF(day, minenrolldate, '2011/12/31') >= 0
AND DATEDIFF(day, '2012/12/31', maxenrolldate) >= 0;
	/* size: 512860633 , time: ~40 m */


SELECT DISTINCT EmployeeZipCode AS zip1213, count(DISTINCT MemberID) AS refpopcount1213
INTO jc574.dbo.lyme_112618_totcount_20122013
FROM jc574.dbo.lyme_112618_totcount_4 AS l
GROUP BY EmployeeZipCode
ORDER BY EmployeeZipCode DESC;
	/* size: 7671 , time:  */


/* 2013-2014 */
SELECT l.*
INTO jc574.dbo.lyme_112618_totcount_5
FROM jc574.dbo.lyme_032718_4 AS l
WHERE 
DATEDIFF(day, minenrolldate, '2012/12/31') >= 0
AND DATEDIFF(day, '2013/12/31', maxenrolldate) >= 0;
	/* size: 478809677 , time: ~40m */


SELECT DISTINCT EmployeeZipCode AS zip1314, count(DISTINCT MemberID) AS refpopcount1314
INTO jc574.dbo.lyme_112618_totcount_20132014
FROM jc574.dbo.lyme_112618_totcount_5 AS l
GROUP BY EmployeeZipCode
ORDER BY EmployeeZipCode DESC;
	/* size: 7671 , time:  */


/* join the by year into 1 */
SELECT *
INTO jc574.dbo.PTLDS_112618_refpopbyyear
FROM jc574.dbo.lyme_112618_totcount_20092010 AS a
FULL JOIN jc574.dbo.lyme_112618_totcount_20102011 AS b ON a.zip0910 = b.zip1011
FULL JOIN jc574.dbo.lyme_112618_totcount_20112012 AS c ON a.zip0910 = c.zip1112
FULL JOIN jc574.dbo.lyme_112618_totcount_20122013 AS d ON a.zip0910 = d.zip1213
FULL JOIN jc574.dbo.lyme_112618_totcount_20132014 AS e ON a.zip0910 = e.zip1314;
	/* 7692 */

