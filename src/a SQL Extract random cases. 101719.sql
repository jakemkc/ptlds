/*
	Getting covariates and drug etc will be in other scripts
DO:
    1. Random pick lyme control from long format longitudinal FactIcd table
	2. Get first case and unique memberID
  
	Getting covariates and drug etc in different scripts

101719
*/




/* 1.1. Random cases */
	/* Get uniqueCtIcd for all members */
	SELECT MemberId, COUNT(DISTINCT Icd) AS uniqueCtIcd
    INTO jc574.dbo.random_count_101719
    FROM dbo.FactIcd
    GROUP BY MemberId;
	

     /* Range A ----------------------------------------------------------- */
	/* A1 */
	/* random pick cases with uniqueCtIcd range */
	SELECT TOP 50000 *
	INTO jc574.dbo.random_101719_a1
	FROM jc574.dbo.random_count_101719
	WHERE
		uniqueCtIcd BETWEEN 1 AND 25
	ORDER BY NEWID() /* random order rows */


	
    SELECT t2.*, t1.uniqueCtIcd
    INTO jc574.dbo.random_101719_a11
    FROM (
    	SELECT *, 
			(SELECT TOP 1 HmsAetnaLineId 
			FROM dbo.FactIcd as ai
			WHERE ai.MemberId = a.MemberId
			ORDER BY NEWID()
			) as HmsAetnaLineId
		FROM jc574.dbo.random_101719_a1 AS a 
	) AS t1 /* random pick a HmsAetnalineID from FactIcd for MemberId join */
	LEFT JOIN dbo.FactIcd AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId
    
		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_101719_a11
		


	/* A2 */
	/* random pick cases with uniqueCtIcd range */
	SELECT TOP 100000 *
	INTO jc574.dbo.random_101719_a2
	FROM jc574.dbo.random_count_101719
	WHERE
		uniqueCtIcd BETWEEN 26 AND 50
	ORDER BY NEWID() /* random order rows */
    


	
    SELECT t2.*, t1.uniqueCtIcd
    INTO jc574.dbo.random_101719_a22
    FROM (
    	SELECT *, 
			(SELECT TOP 1 HmsAetnaLineId 
			FROM dbo.FactIcd as ai
			WHERE ai.MemberId = a.MemberId
			ORDER BY NEWID()
			) as HmsAetnaLineId
		FROM jc574.dbo.random_101719_a2 AS a 
	) AS t1 
	LEFT JOIN dbo.FactIcd AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId
   
		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_101719_a22
		/* size: 100 000 ; time: */


	/* A3 */
	/* random pick cases with uniqueCtIcd range */
	SELECT TOP 100000 *
	INTO jc574.dbo.random_101719_a3
	FROM jc574.dbo.random_count_101719
	WHERE
		uniqueCtIcd BETWEEN 51 AND 75
	ORDER BY NEWID() /* random order rows */
    
    SELECT t2.*, t1.uniqueCtIcd
    INTO jc574.dbo.random_101719_a33
    FROM (
    	SELECT *, 
			(SELECT TOP 1 HmsAetnaLineId 
			FROM dbo.FactIcd as ai
			WHERE ai.MemberId = a.MemberId
			ORDER BY NEWID()
			) as HmsAetnaLineId
		FROM jc574.dbo.random_101719_a3 AS a 
	) AS t1 
	LEFT JOIN dbo.FactIcd AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId
    
		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_101719_a33
		/* size: 100 000 ; time: */


	/* A4 */
	/* random pick cases with uniqueCtIcd range */
	SELECT TOP 80000 *
	INTO jc574.dbo.random_101719_a4
	FROM jc574.dbo.random_count_101719
	WHERE
		uniqueCtIcd BETWEEN 76 AND 100
	ORDER BY NEWID() /* random order rows */
    

    SELECT t2.*, t1.uniqueCtIcd
    INTO jc574.dbo.random_101719_a44
    FROM (
    	SELECT *, 
			(SELECT TOP 1 HmsAetnaLineId 
			FROM dbo.FactIcd as ai
			WHERE ai.MemberId = a.MemberId
			ORDER BY NEWID()
			) as HmsAetnaLineId
		FROM jc574.dbo.random_101719_a4 AS a 
	) AS t1 
	LEFT JOIN dbo.FactIcd AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId
    
		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_101719_a44
		


    /* Range B ----------------------------------------------------------- */
	/* B1 */
	/* random pick cases with uniqueCtIcd range  */
	SELECT TOP 100000 *
	INTO jc574.dbo.random_101719_b1
	FROM jc574.dbo.random_count_101719
	WHERE
		uniqueCtIcd BETWEEN 101 AND 150
	ORDER BY NEWID()
    


	
    SELECT t2.*, t1.uniqueCtIcd
    INTO jc574.dbo.random_101719_b11
    FROM (
    	SELECT *, 
			(SELECT TOP 1 HmsAetnaLineId 
			FROM dbo.FactIcd as ai
			WHERE ai.MemberId = a.MemberId
			ORDER BY NEWID()
			) as HmsAetnaLineId
		FROM jc574.dbo.random_101719_b1 AS a
	) AS t1 
	LEFT JOIN dbo.FactIcd AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId
    
	
		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_101719_b11
		


	/* B1 */
	/* random pick cases with uniqueCtIcd range  */
	SELECT TOP 100000 *
	INTO jc574.dbo.random_101719_b2
	FROM jc574.dbo.random_count_101719
	WHERE
		uniqueCtIcd BETWEEN 151 AND 200
	ORDER BY NEWID()
    

    SELECT t2.*, t1.uniqueCtIcd
    INTO jc574.dbo.random_101719_b22
    FROM (
    	SELECT *, 
			(SELECT TOP 1 HmsAetnaLineId 
			FROM dbo.FactIcd as ai
			WHERE ai.MemberId = a.MemberId
			ORDER BY NEWID()
			) as HmsAetnaLineId
		FROM jc574.dbo.random_101719_b2 AS a
	) AS t1 
	LEFT JOIN dbo.FactIcd AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId
    
	
		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_101719_b22
		



    /* Range C ----------------------------------------------------------- */
	/* random pick cases with uniqueCtIcd range */
	SELECT TOP 200000 *
	INTO jc574.dbo.random_101719_c
	FROM jc574.dbo.random_count_101719
	WHERE
		uniqueCtIcd BETWEEN 201 AND 300
	ORDER BY NEWID()
   



    SELECT t2.*, t1.uniqueCtIcd
    INTO jc574.dbo.random_101719_ccc
    FROM (
    	SELECT *, 
			(SELECT TOP 1 HmsAetnaLineId 
			FROM dbo.FactIcd as ai
			WHERE ai.MemberId = a.MemberId
			ORDER BY NEWID()
			) as HmsAetnaLineId
		FROM jc574.dbo.random_101719_c AS a
	) AS t1 
	LEFT JOIN dbo.FactIcd AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId
    
		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_101719_ccc
		

    /* Range D ----------------------------------------------------------- */
	/* random pick cases with uniqueCtIcd range */
	SELECT TOP 200000 *
	INTO jc574.dbo.random_101719_d
	FROM jc574.dbo.random_count_101719
	WHERE
		uniqueCtIcd BETWEEN 301 AND 400
	ORDER BY NEWID()
    

    SELECT t2.*, t1.uniqueCtIcd
    INTO jc574.dbo.random_101719_ddd
    FROM (
    	SELECT *, 
			(SELECT TOP 1 HmsAetnaLineId 
			FROM dbo.FactIcd as ai
			WHERE ai.MemberId = a.MemberId
			ORDER BY NEWID()
			) as HmsAetnaLineId
		FROM jc574.dbo.random_101719_d AS a
	) AS t1 
	LEFT JOIN dbo.FactIcd AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId
    
		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_101719_ddd
	
					

    /* Range E ----------------------------------------------------------- */
	/* random pick cases with uniqueCtIcd range */
	SELECT TOP 200000 *
	INTO jc574.dbo.random_101719_e
	FROM jc574.dbo.random_count_101719
	WHERE
		uniqueCtIcd BETWEEN 401 AND 510
	ORDER BY NEWID()
  
    SELECT t2.*, t1.uniqueCtIcd
    INTO jc574.dbo.random_101719_eee
    FROM (
    	SELECT *, 
			(SELECT TOP 1 HmsAetnaLineId 
			FROM dbo.FactIcd as ai
			WHERE ai.MemberId = a.MemberId
			ORDER BY NEWID()
			) as HmsAetnaLineId
		FROM jc574.dbo.random_101719_e AS a
	) AS t1
	LEFT JOIN dbo.FactIcd AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId
    
		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_101719_eee



	/* 1.1.1 union all into long format */
		SELECT *
		INTO jc574.dbo.random_stra_101719_test
		FROM jc574.dbo.random_101719_a11
		UNION
		SELECT *
		FROM jc574.dbo.random_101719_a22
		UNION
		SELECT *
		FROM jc574.dbo.random_101719_a33
		UNION
		SELECT *
		FROM jc574.dbo.random_101719_a44
		UNION
		SELECT *
		FROM jc574.dbo.random_101719_b11
		UNION
		SELECT *
		FROM jc574.dbo.random_101719_b22
		UNION
		SELECT *
		FROM jc574.dbo.random_101719_ccc
		UNION
		SELECT *
		FROM jc574.dbo.random_101719_ddd
		UNION
		SELECT *
		FROM jc574.dbo.random_101719_eee;

		/* Count unique by MemberID */
		SELECT COUNT(DISTINCT MemberID) AS Count 
		FROM jc574.dbo.random_stra_101719_test


/* 1.2. Then pick first case to represent member, if repeated */
	/* Get cases represented by first date record, by window function. i.e. memberID+date = unique record for a patient */
		SELECT *
		INTO jc574.dbo.random_stra_101719_test_1
		FROM (
		   SELECT *,
		          ROW_NUMBER() OVER (PARTITION BY MemberId ORDER BY DateServiceStarted) as row_num /*Create column */
		   FROM jc574.dbo.random_stra_101719_test
		   ) AS trows
		WHERE row_num = 1
		ORDER BY MemberId;
	

			/* check */
			SELECT TOP 100 *
			FROM jc574.dbo.random_stra_101719_test_1
			ORDER BY memberid, HmsAetnaLineId, DateServiceStarted;


/* 1.2.1 "t1.uniqueCtIcd" in the extract above (range A - E) was added to check error */
	

	SELECT HmsAetnaLineId, MemberId, DateServiceStarted, Icd, row_num
	INTO jc574.dbo.random_stra_101719_1
	FROM jc574.dbo.random_stra_101719_test_1;


/* 1.3. Merge enrollment information */
	/* a. Pick lyme memberID from Enrollment table and use then extract
	- JOIN first can min the row expansion afterward*/
		SELECT l.*, m.EffectiveDate
		INTO jc574.dbo.random_stra_101719_2
		FROM jc574.dbo.random_stra_101719_1 AS l
		INNER JOIN dbo.Enrollment AS m
		ON l.MemberId = m.MemberId
			

			/* check */
			SELECT TOP 100 *
			FROM jc574.dbo.random_stra_101719_2
			ORDER BY memberid;

		SELECT DISTINCT *
		INTO jc574.dbo.random_stra_101719_3
		FROM (   
			SELECT HmsAetnaLineId, MemberId, DateServiceStarted, Icd,
				MIN(EffectiveDate) OVER (PARTITION BY MemberId) AS minenrolldate, 
				MAX(EffectiveDate) OVER (PARTITION BY MemberId) AS maxenrolldate
			FROM jc574.dbo.random_stra_101719_2
			) AS l  /*  unique */
		WHERE
			DATEDIFF(day, minenrolldate, DateServiceStarted) >= 365 /*105 148 unique*/ /* personal enrollment period criteria */
			AND DATEDIFF(day, DateServiceStarted, maxenrolldate) >= 730 /* 48 903 unique */ /* personal enrollment period criteria */
			AND DATEDIFF(day, DateServiceStarted, '2015/12/31') >= 730 /* 48 596 unique*/ /* aetna overall data availablity */
			AND DATEDIFF(day, '2008/01/01', DateServiceStarted) >= 365; /* aetna overall data availablity */
			

			/* check */
			SELECT TOP 100 *
			FROM jc574.dbo.random_stra_101719_3
			ORDER BY MemberId; 





/* 2.1. count unique ICDs per member */
	SELECT t1.MemberId,  
		t2.Icd
	INTO jc574.dbo.random_unique_101719_1
	FROM jc574.dbo.random_stra_101719_3 AS t1 /* size: 48 596, lyme unique from last year extraction*/ 
	LEFT JOIN
		dbo.FactIcd AS t2
	ON t1.MemberId = t2.MemberId;
	

    SELECT MemberId, COUNT(DISTINCT Icd) AS uniqueCtIcd
    INTO jc574.dbo.random_unique_101719_2
    FROM jc574.dbo.random_unique_101719_1
    GROUP BY MemberId;
	

	SELECT TOP 48596 *
	FROM jc574.dbo.random_unique_101719_2
	ORDER BY uniqueCtIcd DESC
	




/* 2.2. count unique ICDs per member for Lyme */

	SELECT t1.MemberId,  
		t2.Icd
	INTO jc574.dbo.lyme_unique_101719_1
	FROM jc574.dbo.lyme_010818_2 AS t1 
	LEFT JOIN
		dbo.FactIcd AS t2
	ON t1.MemberId = t2.MemberId;
	

    SELECT MemberId, COUNT(DISTINCT Icd) AS uniqueCtIcd
    INTO jc574.dbo.lyme_unique_101719_2
    FROM jc574.dbo.lyme_unique_101719_1
    GROUP BY MemberId;
	

	SELECT TOP 48596 *
	FROM jc574.dbo.lyme_unique_101719_2
	ORDER BY uniqueCtIcd DESC
	

