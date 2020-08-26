
/* Obtain all clyme patient, fitted all criteria of clyme, and with inclusion ICDs */
SELECT o.*, p.Icd AS secondICDs /* m.DateServiceStarted AS clymedatefir, m.Icd as clymeICDfir */
INTO jc574.dbo.lyme_112318_t1
FROM (
	SELECT l.*, m.Icd AS firstICDs /* m.DateServiceStarted AS clymedatefir, m.Icd as clymeICDfir */
	FROM jc574.dbo.lyme_010818_4_allin1 AS l
	LEFT JOIN dbo.FactIcd AS m
	ON l.MemberId = m.MemberId
	WHERE
		m.Icd IN ('780.7', '780.71', '780.79', '331.83', '780.02', '780.1', '780.93', '780.97', '781.8', '784.3', '784.5', '784.51', '784.52', '784.59', '784.6', '784.60', '784.61', '784.69', '799.50', '799.51', '799.52', '799.53', '799.54', '799.55', '799.59', '719.4', '719.40', '719.41', '719.42', '719.43', '719.44', '719.45', '719.46', '719.47', '719.48', '719.49', '729.5', '729.1', '729.2', '357.4', '337.0', '337.00', '337.09', '337.1')
		AND DATEDIFF(day, l.DateServiceStarted, m.DateServiceStarted) BETWEEN 0 AND 180
) AS o
LEFT JOIN dbo.FactIcd AS p
ON o.MemberId = p.MemberId
WHERE
	p.Icd IN ('780.7', '780.71', '780.79', '331.83', '780.02', '780.1', '780.93', '780.97', '781.8', '784.3', '784.5', '784.51', '784.52', '784.59', '784.6', '784.60', '784.61', '784.69', '799.50', '799.51', '799.52', '799.53', '799.54', '799.55', '799.59', '719.4', '719.40', '719.41', '719.42', '719.43', '719.44', '719.45', '719.46', '719.47', '719.48', '719.49', '729.5', '729.1', '729.2', '357.4', '337.0', '337.00', '337.09', '337.1')
	AND DATEDIFF(day, o.DateServiceStarted, p.DateServiceStarted) BETWEEN 210 AND 730;
	/* 1 511 253 */

	SELECT DISTINCT MemberId
	FROM jc574.dbo.lyme_112318_t1;
	/* 4176; equal to clyme total, so this works */



/* Get memberID fits the 1st set of inclusion ICDs */
SELECT DISTINCT MemberId AS IDs1, firstICDs AS s1
INTO jc574.dbo.clyme_112318_s1
FROM jc574.dbo.lyme_112318_t1
WHERE
	firstICDs IN ('780.7', '780.71', '780.79')
	OR secondICDs IN ('780.7', '780.71', '780.79'); 
	/* 4176 */

/* Get memberID fits the 2nd set of inclusion ICDs */
SELECT DISTINCT MemberId AS IDs2, firstICDs AS s2
INTO jc574.dbo.clyme_112318_s2
FROM jc574.dbo.lyme_112318_t1
WHERE
	firstICDs IN ('331.83', '780.02', '780.1', '780.93', '780.97', '781.8', '784.3', '784.5', '784.51', '784.52', '784.59', '784.6', '784.60', '784.61', '784.69', '799.50', '799.51', '799.52', '799.53', '799.54', '799.55', '799.59')
	OR secondICDs IN ('331.83', '780.02', '780.1', '780.93', '780.97', '781.8', '784.3', '784.5', '784.51', '784.52', '784.59', '784.6', '784.60', '784.61', '784.69', '799.50', '799.51', '799.52', '799.53', '799.54', '799.55', '799.59'); 
	/* 602 */

/* Get memberID fits the 3rd set of inclusion ICDs */
SELECT DISTINCT MemberId AS IDs3, firstICDs AS s3
INTO jc574.dbo.clyme_112318_s3
FROM jc574.dbo.lyme_112318_t1
WHERE
	firstICDs IN ('719.4', '719.40', '719.41', '719.42', '719.43', '719.44', '719.45', '719.46', '719.47', '719.48', '719.49', '729.5', '729.1', '729.2', '357.4', '337.0', '337.00', '337.09', '337.1')
	OR secondICDs IN ('719.4', '719.40', '719.41', '719.42', '719.43', '719.44', '719.45', '719.46', '719.47', '719.48', '719.49', '729.5', '729.1', '729.2', '357.4', '337.0', '337.00', '337.09', '337.1'); 
	/* 5201 */

/* results */
/* join 3 of them into one */
SELECT *
INTO jc574.dbo.clyme_112318_count
FROM jc574.dbo.clyme_112318_s1 AS m
FULL JOIN jc574.dbo.clyme_112318_s2 AS n ON m.IDs1 = n.IDs2
FULL JOIN jc574.dbo.clyme_112318_s3 AS o ON m.IDs1 = o.IDs3;
	/* 10 288 */




/* Count by ICDs; not for shown in figure but discussion only*/

SELECT o.*, p.DateServiceStarted AS datesecondICDs, p.Icd AS secondICDs /* m.DateServiceStarted AS clymedatefir, m.Icd as clymeICDfir */
INTO jc574.dbo.lyme_112318_t2
FROM (
	SELECT l.*, m.DateServiceStarted AS datefirstICDs, m.Icd AS firstICDs /* m.DateServiceStarted AS clymedatefir, m.Icd as clymeICDfir */
	FROM jc574.dbo.lyme_010818_4_allin1 AS l
	LEFT JOIN dbo.FactIcd AS m
	ON l.MemberId = m.MemberId
	WHERE
		m.Icd IN ('780.7', '780.71', '780.79', '331.83', '780.02', '780.1', '780.93', '780.97', '781.8', '784.3', '784.5', '784.51', '784.52', '784.59', '784.6', '784.60', '784.61', '784.69', '799.50', '799.51', '799.52', '799.53', '799.54', '799.55', '799.59', '719.4', '719.40', '719.41', '719.42', '719.43', '719.44', '719.45', '719.46', '719.47', '719.48', '719.49', '729.5', '729.1', '729.2', '357.4', '337.0', '337.00', '337.09', '337.1')
		AND DATEDIFF(day, l.DateServiceStarted, m.DateServiceStarted) BETWEEN 0 AND 180
) AS o
LEFT JOIN dbo.FactIcd AS p
ON o.MemberId = p.MemberId
WHERE
	p.Icd IN ('780.7', '780.71', '780.79', '331.83', '780.02', '780.1', '780.93', '780.97', '781.8', '784.3', '784.5', '784.51', '784.52', '784.59', '784.6', '784.60', '784.61', '784.69', '799.50', '799.51', '799.52', '799.53', '799.54', '799.55', '799.59', '719.4', '719.40', '719.41', '719.42', '719.43', '719.44', '719.45', '719.46', '719.47', '719.48', '719.49', '729.5', '729.1', '729.2', '357.4', '337.0', '337.00', '337.09', '337.1')
	AND DATEDIFF(day, o.DateServiceStarted, p.DateServiceStarted) BETWEEN 210 AND 730;
	/* 1 511 253 */

	SELECT DISTINCT MemberId
	FROM jc574.dbo.lyme_112318_t2;
	/* 4176; equal to clyme total, so this works */


/* first ICDs */
SELECT firstICDs, count(firstICDs) AS firstICDscount
/* INTO jc574.dbo.lyme_011618_PTLDS_exc_3_count */
FROM (
	SELECT DISTINCT MemberId, datefirstICDs, firstICDs
	FROM jc574.dbo.lyme_112318_t2
) AS m
GROUP BY firstICDs
ORDER BY firstICDscount DESC;


SELECT MemberId, firstICDs, count(firstICDs) AS firstICDscount
/* INTO jc574.dbo.lyme_011618_PTLDS_exc_3_count */
FROM (
	SELECT DISTINCT MemberId, datefirstICDs, firstICDs
	FROM jc574.dbo.lyme_112318_t2
) AS m
GROUP BY MemberId, firstICDs
ORDER BY firstICDscount DESC;



/* second ICDs */
SELECT secondICDs, count(secondICDs) AS secondICDscount
/* INTO jc574.dbo.lyme_011618_PTLDS_exc_3_count */
FROM (
	SELECT DISTINCT MemberId, datesecondICDs, secondICDs
	FROM jc574.dbo.lyme_112318_t2
) AS m
GROUP BY secondICDs
ORDER BY secondICDscount DESC;


SELECT MemberId, secondICDs, count(secondICDs) AS secondICDscount
/* INTO jc574.dbo.lyme_011618_PTLDS_exc_3_count */
FROM (
	SELECT DISTINCT MemberId, datesecondICDs, secondICDs
	FROM jc574.dbo.lyme_112318_t2
) AS m
GROUP BY MemberId, secondICDs
ORDER BY secondICDscount DESC;












