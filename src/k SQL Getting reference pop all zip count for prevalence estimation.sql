

/* 1. Select ID and Year */
    SELECT DISTINCT memberID, YEAR(EffectiveDate) AS yearenroll 
    INTO jc574.dbo.lyme_112718_count_1
    FROM dbo.Enrollment;
    /* size: 181 273 275 , time: 9 m */ 



/* 2009 */
/* 2. Select within target year */
    SELECT *
    INTO jc574.dbo.lyme_112718_count_2009_1
    FROM jc574.dbo.lyme_112718_count_1
    WHERE 
    yearenroll = '2009';
    /* size: 19 904 723 , time: 9s */ 


/* 3. Join zipcode */
    SELECT m.*, n.EmployeeZipCode
    INTO jc574.dbo.lyme_112718_count_2009_2
    FROM jc574.dbo.lyme_112718_count_2009_1 AS m
    INNER JOIN(
        SELECT memberID, EmployeeZipCode
        FROM dbo.Enrollment
        WHERE
        YEAR(EffectiveDate) = '2009'
        GROUP BY memberID, EmployeeZipCode
    ) AS n
    ON m.memberID = n.memberID;
    /* size: 19 904 723 , time: 2:12 */ 

/* 4. Count by zip */
    SELECT EmployeeZipCode AS zip0910, count(DISTINCT MemberID) AS refpopcount0910
    INTO jc574.dbo.lyme_112718_count_2009_3
    FROM jc574.dbo.lyme_112718_count_2009_2
    GROUP BY EmployeeZipCode
    ORDER BY EmployeeZipCode DESC;
    /* size: 37542 , time: 9s */ 


/* 2010 */
/* 2. Select within target year */
    SELECT *
    INTO jc574.dbo.lyme_112718_count_2010_1
    FROM jc574.dbo.lyme_112718_count_1
    WHERE 
    yearenroll = '2010';
    /* size: 21 349 229 , time: 12 s */ 


/* 3. Join zipcode */
    SELECT m.*, n.EmployeeZipCode
    INTO jc574.dbo.lyme_112718_count_2010_2
    FROM jc574.dbo.lyme_112718_count_2010_1 AS m
    INNER JOIN(
        SELECT memberID, EmployeeZipCode
        FROM dbo.Enrollment
        WHERE
        YEAR(EffectiveDate) = '2010'
        GROUP BY memberID, EmployeeZipCode
    ) AS n
    ON m.memberID = n.memberID;
    /* size: 21 349 229 , time: 2:30 */ 

/* 4. Count by zip */
    SELECT EmployeeZipCode AS zip1011, count(DISTINCT MemberID) AS refpopcount1011
    INTO jc574.dbo.lyme_112718_count_2010_3
    FROM jc574.dbo.lyme_112718_count_2010_2
    GROUP BY EmployeeZipCode
    ORDER BY EmployeeZipCode DESC;
    /* size: 38 458 , time: 9 s */ 


/* 2011 */
/* 2. Select within target year */
    SELECT *
    INTO jc574.dbo.lyme_112718_count_2011_1
    FROM jc574.dbo.lyme_112718_count_1
    WHERE 
    yearenroll = '2011';
    /* size: 20 232 416 , time:  */ 


/* 3. Join zipcode */
    SELECT m.*, n.EmployeeZipCode
    INTO jc574.dbo.lyme_112718_count_2011_2
    FROM jc574.dbo.lyme_112718_count_2011_1 AS m
    INNER JOIN(
        SELECT memberID, EmployeeZipCode
        FROM dbo.Enrollment
        WHERE
        YEAR(EffectiveDate) = '2011'
        GROUP BY memberID, EmployeeZipCode
    ) AS n
    ON m.memberID = n.memberID;
    /* size:20 232 416  , time:  */ 

/* 4. Count by zip */
    SELECT EmployeeZipCode AS zip1112, count(DISTINCT MemberID) AS refpopcount1112
    INTO jc574.dbo.lyme_112718_count_2011_3
    FROM jc574.dbo.lyme_112718_count_2011_2
    GROUP BY EmployeeZipCode
    ORDER BY EmployeeZipCode DESC;
    /* size: 38125 , time:  */ 



/* 2012 */
/* 2. Select within target year */
    SELECT *
    INTO jc574.dbo.lyme_112718_count_2012_1
    FROM jc574.dbo.lyme_112718_count_1
    WHERE 
    yearenroll = '2012';
    /* size: 19 902 664  , time:  */ 


/* 3. Join zipcode */
    SELECT m.*, n.EmployeeZipCode
    INTO jc574.dbo.lyme_112718_count_2012_2
    FROM jc574.dbo.lyme_112718_count_2012_1 AS m
    INNER JOIN(
        SELECT memberID, EmployeeZipCode
        FROM dbo.Enrollment
        WHERE
        YEAR(EffectiveDate) = '2012'
        GROUP BY memberID, EmployeeZipCode
    ) AS n
    ON m.memberID = n.memberID;
    /* size: 19 902 664 , time:  */ 

/* 4. Count by zip */
    SELECT EmployeeZipCode AS zip1213, count(DISTINCT MemberID) AS refpopcount1213
    INTO jc574.dbo.lyme_112718_count_2012_3
    FROM jc574.dbo.lyme_112718_count_2012_2
    GROUP BY EmployeeZipCode
    ORDER BY EmployeeZipCode DESC;
    /* size: 38082  , time:  */ 



/* 2013 */
/* 2. Select within target year */
    SELECT *
    INTO jc574.dbo.lyme_112718_count_2013_1
    FROM jc574.dbo.lyme_112718_count_1
    WHERE 
    yearenroll = '2013';
    /* size: 20 668 465 , time:  */ 


/* 3. Join zipcode */
    SELECT m.*, n.EmployeeZipCode
    INTO jc574.dbo.lyme_112718_count_2013_2
    FROM jc574.dbo.lyme_112718_count_2013_1 AS m
    INNER JOIN(
        SELECT memberID, EmployeeZipCode
        FROM dbo.Enrollment
        WHERE
        YEAR(EffectiveDate) = '2013'
        GROUP BY memberID, EmployeeZipCode
    ) AS n
    ON m.memberID = n.memberID;
    /* size: 21 650 784 , time:  */ 


/* 4. Count by zip */
    SELECT EmployeeZipCode AS zip1314, count(DISTINCT MemberID) AS refpopcount1314
    INTO jc574.dbo.lyme_112718_count_2013_3
    FROM jc574.dbo.lyme_112718_count_2013_2
    GROUP BY EmployeeZipCode
    ORDER BY EmployeeZipCode DESC;
    /* size: 38 302 , time:  */ 




/* join the by year into 1 */
SELECT *
INTO jc574.dbo.lyme_112918_refpopallzipbyyear
FROM jc574.dbo.lyme_112718_count_2009_3 AS a
FULL JOIN jc574.dbo.lyme_112718_count_2010_3 AS b ON a.zip0910 = b.zip1011
FULL JOIN jc574.dbo.lyme_112718_count_2011_3 AS c ON a.zip0910 = c.zip1112
FULL JOIN jc574.dbo.lyme_112718_count_2012_3 AS d ON a.zip0910 = d.zip1213
FULL JOIN jc574.dbo.lyme_112718_count_2013_3 AS e ON a.zip0910 = e.zip1314;
    /* size: 44060 */
