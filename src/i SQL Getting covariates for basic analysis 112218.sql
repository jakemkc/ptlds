
/* 1. Pick lyme */
	/* check */
	SELECT TOP 100 *
	FROM jc574.dbo.lyme_010818_2
	ORDER BY MemberId; /*case range: 2008-12-31 to 2013-12-31 */
	/* size: 48 596, time: 20s */ 

/* 2. Join with covariates from MedicalClaims table*/
	SELECT t1.HmsAetnaLineId, t1.MemberId, t1.DateServiceStarted, 
		t2.DateServiceStopped, t2.MemberBirthYear, t2.MemberGender, t2.MemberRelationshiptoEmployee, t2.EmployeeZipCode
	INTO jc574.dbo.lyme_032818_1
	FROM jc574.dbo.lyme_010818_2 AS t1
	LEFT JOIN
		dbo.MedicalClaims AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId;
	/* size: 48 596, time: 3 m */ 

/* 2.1 Join with covariates from Members table*/
	SELECT t1.*, t2.SubscriberId
	INTO jc574.dbo.lyme_032818_2
	FROM jc574.dbo.lyme_032818_1 AS t1
	LEFT JOIN
		dbo.Members AS t2
	ON t1.MemberId = t2.MemberId;
	/* size: 48 596, time: 19s */ 


/* 1. Pick clyme */
	/* check */
	SELECT TOP 100 *
	FROM jc574.dbo.lyme_010818_4_allin1_final
	ORDER BY MemberId; /*case range: 2008-12-31 to 2013-12-31 */
	/* size: 4176 */ 

/* 2. Join with covariates from MedicalClaims table*/
	SELECT t1.HmsAetnaLineId, t1.MemberId, t1.DateServiceStarted, 
		t2.DateServiceStopped, t2.MemberBirthYear, t2.MemberGender, t2.MemberRelationshiptoEmployee, t2.EmployeeZipCode
	INTO jc574.dbo.lyme_032818_3
	FROM jc574.dbo.lyme_010818_4_allin1_final AS t1
	LEFT JOIN
		dbo.MedicalClaims AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId;
	/* size: 4176 , time:  0s */ 

/* 2.1 Join with covariates from Members table*/
	SELECT t1.*, t2.SubscriberId
	INTO jc574.dbo.lyme_032818_4
	FROM jc574.dbo.lyme_032818_3 AS t1
	LEFT JOIN
		dbo.Members AS t2
	ON t1.MemberId = t2.MemberId;
	/* size: 4176 , time:  0s */ 


/* 1. Pick PTLDS */
	/* check */
	SELECT TOP 100 *
	FROM jc574.dbo.lyme_010818_5_final_all_2
	ORDER BY MemberId; /*case range: 2008-12-31 to 2013-12-31 */
	/* size: 1518 */

/* 2. Join with covariates from MedicalClaims table*/
	SELECT t1.HmsAetnaLineId, t1.MemberId, t1.DateServiceStarted, 
		t2.DateServiceStopped, t2.MemberBirthYear, t2.MemberGender, t2.MemberRelationshiptoEmployee, t2.EmployeeZipCode
	INTO jc574.dbo.lyme_032818_5_1
	FROM jc574.dbo.lyme_010818_5_final_all_2 AS t1
	LEFT JOIN
		dbo.MedicalClaims AS t2
	ON t1.HmsAetnaLineId = t2.HmsAetnaLineId;
	/* size: 1518 , time:  0s */ 

/* 2.1 Join with covariates from Members table*/
	SELECT t1.*, t2.SubscriberId
	INTO jc574.dbo.lyme_032818_6_1
	FROM jc574.dbo.lyme_032818_5_1 AS t1
	LEFT JOIN
		dbo.Members AS t2
	ON t1.MemberId = t2.MemberId;
	/* size: 1518 , time:  0s */ 