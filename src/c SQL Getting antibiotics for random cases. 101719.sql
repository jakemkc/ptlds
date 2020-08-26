/*
	Getting drugs and antibiotics info for patients
DO:
    1. Pick random table
	2. Leftjoin by memberID and for all drugs within 5 days after "dateservicestarted" in medicalclaims
		- in long format by memberID
	3. Filter 3 antibiotics (generic and trade names)

101719
*/

/* random table */
/* 1. Left join drug info */

/* left join lyme patients with drug */
SELECT l.*,
	d.PrescribingProviderId AS d_PresProvirId, 
	d.PrescribingProviderSpecialty AS d_PresProvSpec, 
	d.PharmacyId AS d_PharmacyId , 
	d.PharmacyZipCode AS d_PharmacyZip, 
	d.DispenseDate AS d_DispenseDate, 
	d.NationalDrugCode AS d_NationalDrug, 
	d.NdcDescription AS d_NdcDesc, 
	d.BrandGenericIndicator AS d_BrandGenInd, 
	d.SingleMultiSource AS d_SingMultiSour, 
	d.NewRefill AS d_NewRefill, 
	d.Quantity AS d_Quantity, 
	d.DaysSupply AS d_DaysSupply, 
	d.MaintenanceDrugIndicator AS d_MaintDrugInd
INTO jc574.dbo.random_drug_101719_1
FROM jc574.dbo.random_cov_101719_1 AS l
LEFT JOIN dbo.PharmacyClaims AS d
ON l.MemberId = d.MemberId
	AND (d.DispenseDate <= DATEADD(Day, +5, l.DateServiceStarted) AND d.DispenseDate >= l.DateServiceStarted);
	
	
	/* check */
	SELECT TOP 500 *
	FROM jc574.dbo.random_drug_101719_1
	ORDER BY memberid, HmsAetnaLineId, DateServiceStarted;


/* 2. Filter drugs with in the Drugdescription fields for 3 antibiotics */

/* random-drug table */
SELECT *
INTO jc574.dbo.random_drug_101719_2
FROM jc574.dbo.random_drug_101719_1
WHERE 
	d_NdcDesc LIKE '%Amoxicillin%' OR
	d_NdcDesc LIKE '%Augmentin%' OR
	d_NdcDesc LIKE '%Amoxil%' OR
	d_NdcDesc LIKE '%Biomox%' OR
	d_NdcDesc LIKE '%Trimox%' OR
	d_NdcDesc LIKE '%Moxatag%' OR
	d_NdcDesc LIKE '%Clavamox%' OR
	d_NdcDesc LIKE '%Amoxi Drop%' OR
	d_NdcDesc LIKE '%Amoxi-tabs%' OR
	d_NdcDesc LIKE '%Doxycycline%' OR
	d_NdcDesc LIKE '%Ocudox%' OR
	d_NdcDesc LIKE '%Morgidox%' OR
	d_NdcDesc LIKE '%Vibramycin%' OR
	d_NdcDesc LIKE '%Doryx%' OR
	d_NdcDesc LIKE '%Monodox%' OR
	d_NdcDesc LIKE '%Periostat%' OR
	d_NdcDesc LIKE '%Atridox%' OR
	d_NdcDesc LIKE '%Adoxa%' OR
	d_NdcDesc LIKE '%Doxy%' OR
	d_NdcDesc LIKE '%Oracea%' OR
	d_NdcDesc LIKE '%Alodox%' OR
	d_NdcDesc LIKE '%Oraxyl%' OR
	d_NdcDesc LIKE '%Avidoxy%' OR
	d_NdcDesc LIKE '%Acticlate%' OR
	d_NdcDesc LIKE '%Mondoxyne%' OR
	d_NdcDesc LIKE '%Targadox%' OR
	d_NdcDesc LIKE '%Cefuroxime%' OR
	d_NdcDesc LIKE '%Ceftin%' OR
	d_NdcDesc LIKE '%Zinacef%';
	

	/* check */
	SELECT TOP 500 *
	FROM jc574.dbo.random_drug_101719_2
	ORDER BY memberid, HmsAetnaLineId, DateServiceStarted;





