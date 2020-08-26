# Epidemiological Study of Post-Treatment Lyme Disease Syndrome with Large Medical Claims Data

<!--
*Submitted for peer review, Fall 2017*
-->

<!--
## Authors
- Ming Kei (Jake) Chung
  - github: [\@jakemkc](http://github.com/jakemkc)
  - twitter: [\@jakekei](http://twitter.com/jakekei)
  - email: jake_chung[at]hms[dot]harvard[dot]edu
- Germaine M. Buck Louis
  - email: glouis[at]gmu[dot]edu
- Kurunthachalam Kannan
  - email: kurunthachalam[dot]kannan[at]health[dot]ny[dot]gov
- Chirag J. Patel
  - github: [\@chiragjp](http://github.com/chiragjp)
  - web: [www.chiragjpgroup.org](http://www.chiragjpgroup.org)
--> 


## Figure 1A
 ![F1](methods/Figure_1_3072.png)

 

Figure 1A) Case definitions of Lyme and PTLDS in current study. We used three progressive procedures to extract the cases. For Lyme disease cases, we started from identifying the first Lyme disease record for each patient in the database (ICD = 088.81; light blue shading). Then, we selected the individuals who had consecutively enrolled in Aetna’s insurance plan for at least 1 year prior to and 2 years following the first records and used them as the baseline for all the subsequent analyses. For PTLDS, cases were defined successively by adding new inclusion (light yellow shading) and exclusion criteria (light green shading) on top of Lyme disease patients. We filtered for Lyme patients who 1) did not complain for any one of the core PTLDS symptoms  -- chronic fatigue, cognitive impairment, and musculoskeletal pain -- 1 year prior to the baseline; 2) did not have another Lyme record from 1 month following the baseline, and 3) complained for any one of the core PTLDS symptoms within 6 months and after the 7th month from the baseline. Finally, we defined PTLDS patients as a subset of the core symptom Lyme disease patients not matching any one of the exclusion (i.e., potentially confounding) symptoms such as autoimmune disease, liver disease, and cancer that are proposed by the Infectious Diseases Society of America. 


## Figure 1B
 ![F2](methods/Figure_2_3072.png)

 
Figure 1B) Overview of the data extraction and analytical procedures. We first extracted Lyme disease cases and controls from the claims data. We required Lyme disease patients to have a longer enrollment criterion because of the longitudinal comorbidity analysis in Aim 2. For Aim 1, our main analysis includes a phenome wide association study (PheWAS) of three prolonged symptoms of Lyme disease (chronic fatigue, cognitive impairment, musculoskeletal pain) with logistic regression and used generalized estimating equations (GEE) to handle correlation between ZIP codes. We ran two sensitivity analyses to confirm the robustness of our findings -- A) to adjust for seven additional chronic conditions that were associated with Lyme disease and B) to refine the definition of Lyme disease patients and only analyzed subjects with the drug plan. For Aim 2, we compared chronic conditions between Lyme disease and PTLDS. We selected a subset of Lyme disease patients as “cured Lyme” for more meaningful comparisons. To identify the differences between cured Lyme and PTLDS, we ran a PheWAS of 19 chronic conditions.




 
 
 
