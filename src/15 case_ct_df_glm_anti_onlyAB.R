## Nov 28 2019
## Goal: Lyme and control data run regression, with Drug as cov

rm(list=ls()) # clear workspace
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000
options(warn=1) # default = 0


## Load data
load("./LID_data_012721/case_control_wAB_030920_onlyAB.Rdata")
lymeAllcomo <- readRDS("./data/SQL/lymecomo102819.rds") # new with all selected como
ctAllcomo <- readRDS("./data/SQL/controlComo101819.rds") # take some time to load

## load varset
source("./src/prep/VariableSetsLyme.R") # functons and long vector names


# ******** -----
# A. PREP var -----------------------------------------------------------------
library(tidyverse); library(lubridate)


lymecomo <- lymeAllcomo[which(lymeAllcomo$MemberID %in% lymeWab$MemberId), ] #same rows
ctcomo <- ctAllcomo[which(ctAllcomo$MemberID %in% ctWab$MemberId), ] # less rows


## *************************************
## prepare chronic disease class
chronicDisease <- mget(c("chronicFatigue","cognitiveImp","pain","coInfection","obese","sleepApnea","narcolepsy","autoimmune","uncontrolEndo1","uncontrolCardio1","cancer","bipolar","schizophrenia","delusion","dementia","anorexia","bulimia","depressive","drug1","alcohol1","Hypothyroidism","Acute_Myocardial_Infarction","Anemia","Asthma","Atrial_Fibrillation","Prostatic_Hyperplasia","Cataract","Chron_Kidney_Disease","Pulmonary_Disease","Diabetes","Glaucoma","Heart_Failure","Pelvic_Fracture","Hyperlipidemia","Hypertension","Ischemic_Heart_Disease","Osteoporosis","Rheumatoid_Arthritis","Stroke"))


rm("chronicFatigue","cognitiveImp","pain","coInfection","obese","sleepApnea","narcolepsy","autoimmune","uncontrolEndo1","uncontrolCardio1","cancer","bipolar","schizophrenia","delusion","dementia","anorexia","bulimia","depressive","drug1","alcohol1","Hypothyroidism","Acute_Myocardial_Infarction","Anemia","Asthma","Atrial_Fibrillation","Prostatic_Hyperplasia","Cataract","Chron_Kidney_Disease","Pulmonary_Disease","Diabetes","Glaucoma","Heart_Failure","Pelvic_Fracture","Hyperlipidemia","Hypertension","Ischemic_Heart_Disease","Osteoporosis","Rheumatoid_Arthritis","Stroke")

# build function
chronicDiseasePrep <- function(lymecomo) {
  
  lymecomo$chronicClass <- "n____"
  
  for(i in 1:length(chronicDisease)) {
    mtchC <- which(lymecomo$coICD %in% chronicDisease[[i]])
    lymecomo$chronicClass[mtchC] <- names(chronicDisease)[i]
  }
  return(lymecomo)
}


lymecomoClass <- chronicDiseasePrep(lymecomo)
ctcomoClass <- chronicDiseasePrep(ctcomo)

# save memory
rm(lymecomo)
rm(ctcomo)

## *************************************
## intermediate data prep
dataprepClass <- function(lymecomoClass) {
  
  lymecomoClass$age <- 2017 - lymecomoClass$MemberBirthYear
  lymecomoClass[which(lymecomoClass$age > 100), "age"] <- 100
  
  lymecomoClass$year <- year(ymd(lymecomoClass$DateServiceStarted))
  lymecomoClass$coyear <- year(ymd(lymecomoClass$coICDdate))
  
  lymecomoClass <- lymecomoClass %>% select(-c(HMSAetnaLineID, DateServiceStarted, MemberBirthYear, coICDdate))
  
  lymecomoClass$yeartype <- case_when(
    lymecomoClass$coyear - lymecomoClass$year == 2 ~ "p5_post2",
    lymecomoClass$coyear - lymecomoClass$year == 1 ~ "p4_post1",
    lymecomoClass$coyear - lymecomoClass$year == 0 ~ "p3_baseline",
    lymecomoClass$coyear - lymecomoClass$year == -1 ~ "p2_pre1",
    lymecomoClass$coyear - lymecomoClass$year == -2 ~ "p1_pre2")
  
  lymecomoClass <- lymecomoClass %>% filter(!is.na(yeartype)) %>% distinct
  return(lymecomoClass)
}


lymecomoClassInt <- dataprepClass(lymecomoClass)
ctcomoClassInt <- dataprepClass(ctcomoClass)


# save memory
rm(lymecomoClass, lymeAllcomo)
rm(ctcomoClass, ctAllcomo)




## *************************************
# CREATE final df ----

GetGLMdf <- function(Rheumatoid_Arthritis, lymecomoClassInt, lymeWab, lymeID) {
  
  ## lyme
  tmp1a <- lymecomoClassInt %>% filter(yeartype == "p1_pre2", chronicClass == Rheumatoid_Arthritis)
  
  tmp2a <- tmp1a %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() # remove duplicate as disease instead of ICD
  
  tmp3a <- left_join(lymeWab, tmp2a, by = c("MemberId" = "MemberID"))
  tmp3.1a <- tmp3a %>% select(MemberId, MemberGender, age, chronicClass, EmployeeZipCode, uniqueCtIcd, DrugInfo, AntiInfo, stateID)
  tmp3.1a <- tmp3.1a %>% mutate(yeartype = "p1_pre2")
  tmp3.1a <- tmp3.1a %>% mutate(treatment = lymeID)
  tmp3.1a$chronicClass <- replace_na(tmp3.1a$chronicClass, "no")
  tmp3.1a$chronicDiseaseSwitch <- Rheumatoid_Arthritis
  tmpdf1 <- tmp3.1a
  
  
  # extract another time point
  tmp1a <- lymecomoClassInt %>% filter(yeartype == "p2_pre1", chronicClass == Rheumatoid_Arthritis)
  
  tmp2a <- tmp1a %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() # remove duplicate as disease instead of ICD
  
  tmp3a <- left_join(lymeWab, tmp2a, by = c("MemberId" = "MemberID"))
  tmp3.1a <- tmp3a %>% select(MemberId, MemberGender, age, chronicClass, EmployeeZipCode, uniqueCtIcd, DrugInfo, AntiInfo, stateID)
  tmp3.1a <- tmp3.1a %>% mutate(yeartype = "p2_pre1")
  tmp3.1a <- tmp3.1a %>% mutate(treatment = lymeID)
  tmp3.1a$chronicClass <- replace_na(tmp3.1a$chronicClass, "no")
  tmp3.1a$chronicDiseaseSwitch <- Rheumatoid_Arthritis
  tmpdf2 <- tmp3.1a
  
  # extract another time point
  tmp1a <- lymecomoClassInt %>% filter(yeartype == "p3_baseline", chronicClass == Rheumatoid_Arthritis)
  
  tmp2a <- tmp1a %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() # remove duplicate as disease instead of ICD
  
  tmp3a <- left_join(lymeWab, tmp2a, by = c("MemberId" = "MemberID"))
  tmp3.1a <- tmp3a %>% select(MemberId, MemberGender, age, chronicClass, EmployeeZipCode, uniqueCtIcd, DrugInfo, AntiInfo, stateID)
  tmp3.1a <- tmp3.1a %>% mutate(yeartype = "p3_baseline")
  tmp3.1a <- tmp3.1a %>% mutate(treatment = lymeID)
  tmp3.1a$chronicClass <- replace_na(tmp3.1a$chronicClass, "no")
  tmp3.1a$chronicDiseaseSwitch <- Rheumatoid_Arthritis
  tmpdf3 <- tmp3.1a
  
  # extract another time point
  tmp1a <- lymecomoClassInt %>% filter(yeartype == "p4_post1", chronicClass == Rheumatoid_Arthritis)
  
  tmp2a <- tmp1a %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() # remove duplicate as disease instead of ICD
  
  tmp3a <- left_join(lymeWab, tmp2a, by = c("MemberId" = "MemberID"))
  tmp3.1a <- tmp3a %>% select(MemberId, MemberGender, age, chronicClass, EmployeeZipCode, uniqueCtIcd, DrugInfo, AntiInfo, stateID)
  tmp3.1a <- tmp3.1a %>% mutate(yeartype = "p4_post1")
  tmp3.1a <- tmp3.1a %>% mutate(treatment = lymeID)
  tmp3.1a$chronicClass <- replace_na(tmp3.1a$chronicClass, "no")
  tmp3.1a$chronicDiseaseSwitch <- Rheumatoid_Arthritis
  tmpdf4 <- tmp3.1a
  
  # extract another time point
  tmp1a <- lymecomoClassInt %>% filter(yeartype == "p5_post2", chronicClass == Rheumatoid_Arthritis)

  tmp2a <- tmp1a %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() # remove duplicate as disease instead of ICD
  
  tmp3a <- left_join(lymeWab, tmp2a, by = c("MemberId" = "MemberID"))
  tmp3.1a <- tmp3a %>% select(MemberId, MemberGender, age, chronicClass, EmployeeZipCode, uniqueCtIcd, DrugInfo, AntiInfo, stateID)
  tmp3.1a <- tmp3.1a %>% mutate(yeartype = "p5_post2")
  tmp3.1a <- tmp3.1a %>% mutate(treatment = lymeID)
  tmp3.1a$chronicClass <- replace_na(tmp3.1a$chronicClass, "no")
  tmp3.1a$chronicDiseaseSwitch <- Rheumatoid_Arthritis
  tmpdf5 <- tmp3.1a
  
  ## bind
  fdata <- bind_rows(tmpdf1, tmpdf2, tmpdf3, tmpdf4, tmpdf5)
  
  return(fdata)
}


library(purrr)
maptmp1 <- map(names(chronicDisease), possibly(GetGLMdf, NA), lymecomoClassInt, lymeWab, "lyme")
maptmp2 <- map(names(chronicDisease), possibly(GetGLMdf, NA), ctcomoClassInt, ctWab, "ct")
comoDf <- bind_rows(maptmp1, maptmp2)
rm(maptmp1, maptmp2)




## **********************
## CHECK case count in the model's key strata

nCt1 <- comoDf %>% group_by(chronicDiseaseSwitch, yeartype, treatment) %>% summarise(
  cases = sum(chronicClass != "no"),
  totaln = n()
)

# to excel table oder, n count.

excelN <- function(tmp1) {
  tmp11 <- tmp1 %>% arrange(treatment, desc(yeartype)) %>% arrange(chronicDiseaseSwitch)
  tmp11 <- tmp11 %>% unite("Year_Treat", c("yeartype", "treatment")) %>% select(-totaln)
  tmp11 <- tmp11 %>% spread(Year_Treat, cases)
  tmp11 <- tmp11 %>% slice(match(names(chronicDisease), chronicDiseaseSwitch))
  tmp11 <- tmp11[order(match(tmp11$chronicDiseaseSwitch, names(chronicDisease))),]
  return(tmp11)
}

nCt2 <- excelN(nCt1)



## **********************
## PREPARE glmdf variable structure
comoDf$MemberGender <- as.factor(comoDf$MemberGender)

comoDf$chronicClass <- as.factor(comoDf$chronicClass)

comoDf$yeartype <- as.factor(comoDf$yeartype)

comoDf$treatment <- as.factor(comoDf$treatment)

comoDf$DrugInfo <- as.factor(comoDf$DrugInfo)

comoDf$AntiInfo <- as.factor(comoDf$AntiInfo)

comoDf$stateID <- as.factor(comoDf$stateID)

str(comoDf)

comoDf <- comoDf %>% mutate(outcome = if_else(chronicClass == "no", 0, 1))

str(comoDf)

comoDf <- comoDf[complete.cases(comoDf), ] 

levels(comoDf$yeartype)
contrasts(comoDf$yeartype) # TP: 0 = p1

levels(comoDf$treatment)
contrasts(comoDf$treatment) # lymedisease: 0 = ct, 1 = lyme





## **********************
## RUN glm 

## **********************
## Model 1 ----
library(geepack); library(broom)

## prepare 
pmapvar <- comoDf %>% select(yeartype, chronicDiseaseSwitch) %>% distinct
pmapvar$mergeNames <- str_c(pmapvar$chronicDiseaseSwitch, pmapvar$yeartype, sep = " ")


# pmapvar <- pmapvar[1:2, ]

## gee function
RunGeeGlmOR_m1 <- function(p1_pre2, chronicFatigue, tmpdata) {
  tmp1 <- tmpdata %>% filter(yeartype == p1_pre2, chronicDiseaseSwitch == chronicFatigue)
  tmp2 <- geeglm(outcome ~ treatment + age + MemberGender + uniqueCtIcd + AntiInfo + stateID, id = EmployeeZipCode, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- chronicFatigue
  tmp3$yeartype <- p1_pre2
  return(tmp3)
}


# Use OR and pmap
pmapvar <- pmapvar %>% filter(yeartype == "p3_baseline") #From 195 -> 39 rows
pmapvar <- pmapvar %>% filter(chronicDiseaseSwitch %in% c("chronicFatigue", "cognitiveImp", "pain")) #From 195 -> 39 rows
maptmp1 <- pmap(list(pmapvar$yeartype, pmapvar$chronicDiseaseSwitch), possibly(RunGeeGlmOR_m1, NA), comoDf) %>% setNames(pmapvar$mergeNames)

## Extract results to paper table
geeEstdf_m1 <- bind_rows(maptmp1, .id = "id") 

geeEstdf_m1$fdr <- p.adjust(geeEstdf_m1$p.value, method = 'fdr')




# ## START ****************************************************************************************
## **********************
# 082520 STROBE no adjust
## gee function
RunGeeGlmOR_m1_unadj <- function(p1_pre2, chronicFatigue, tmpdata) {
  tmp1 <- tmpdata %>% filter(yeartype == p1_pre2, chronicDiseaseSwitch == chronicFatigue)
  tmp2 <- geeglm(outcome ~ treatment, id = EmployeeZipCode, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- chronicFatigue
  tmp3$yeartype <- p1_pre2
  return(tmp3)
}


# Use OR and pmap
pmapvar <- pmapvar %>% filter(yeartype == "p3_baseline") #From 195 -> 39 rows
pmapvar <- pmapvar %>% filter(chronicDiseaseSwitch %in% c("chronicFatigue", "cognitiveImp", "pain")) #From 195 -> 39 rows
maptmp1unadj082520 <- pmap(list(pmapvar$yeartype, pmapvar$chronicDiseaseSwitch), possibly(RunGeeGlmOR_m1_unadj, NA), comoDf) %>% setNames(pmapvar$mergeNames)

## Extract results to paper table
geeEstdf_m1_unadj <- bind_rows(maptmp1unadj082520, .id = "id") 

geeEstdf_m1_unadj$fdr <- p.adjust(geeEstdf_m1_unadj$p.value, method = 'fdr')

# ## END ****************************************************************************************


## **********************
## Model 2 ----
### STEP 1: ----
## Get all P looped first and select P as covaraite in Model 1


## prepare 
pmapvar <- comoDf %>% select(yeartype, chronicDiseaseSwitch) %>% distinct
pmapvar$mergeNames <- str_c(pmapvar$chronicDiseaseSwitch, pmapvar$yeartype, sep = " ")

# Use OR and pmap
pmapvar <- pmapvar %>% filter(yeartype == "p3_baseline") #From 195 -> 39 rows
maptmp1 <- pmap(list(pmapvar$yeartype, pmapvar$chronicDiseaseSwitch), possibly(RunGeeGlmOR_m1, NA), comoDf) %>% setNames(pmapvar$mergeNames)
# 20 m

## Extract results to paper table
geeEstdf_m2_pLoop <- bind_rows(maptmp1, .id = "id") 
geeEstdf_m2_pLoop$fdr <- p.adjust(geeEstdf_m2_pLoop$p.value, method = 'fdr') # check results, see CSV



### STEP 2: ----

## prepare data
## function (add 4 como)
GetGLMdf1 <- function(chronicFatigue, df) {
  tmp1 <- df %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == chronicFatigue) #row = 26329
  tmp2 <- df %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "obese") %>% select(outcome) %>% setNames("obese")
  tmp3 <- df %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "Atrial_Fibrillation") %>% select(outcome) %>% setNames("Atrial_Fibrillation")
  tmp4 <- df %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "Chron_Kidney_Disease") %>% select(outcome) %>% setNames("Chron_Kidney_Disease")
  tmp5 <- df %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "Heart_Failure") %>% select(outcome) %>% setNames("Heart_Failure")
  
  comoAdjDf <- bind_cols(tmp1, tmp2, tmp3, tmp4, tmp5)
  comoAdjDf$obese <- as.factor(comoAdjDf$obese)
  comoAdjDf$Atrial_Fibrillation <- as.factor(comoAdjDf$Atrial_Fibrillation)
  comoAdjDf$Chron_Kidney_Disease <- as.factor(comoAdjDf$Chron_Kidney_Disease)
  comoAdjDf$Heart_Failure <- as.factor(comoAdjDf$Heart_Failure)
  
  return(comoAdjDf)
}


maptmp1 <- map(names(chronicDisease)[1:3], possibly(GetGLMdf1, NA), comoDf) %>% setNames(names(chronicDisease)[1:3])


## gee function
RunGeeGlmOR_m2 <- function(chronicFatigue, df) {
  tmp1 <- df[[chronicFatigue]]
  tmp2 <- geeglm(outcome ~ treatment + age + MemberGender + uniqueCtIcd + AntiInfo + stateID + obese + Atrial_Fibrillation + Chron_Kidney_Disease + Heart_Failure, id = EmployeeZipCode, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- chronicFatigue
  return(tmp3)
}

maptmp2 <- map(names(chronicDisease)[1:3], RunGeeGlmOR_m2, maptmp1) %>% setNames(names(chronicDisease)[1:3])
geeEstdf_m2 <- bind_rows(maptmp2, .id = "id") 
geeEstdf_m2$fdr <- p.adjust(geeEstdf_m2$p.value, method = 'fdr')



# ## START ****************************************************************************************
## **********************
# 082520 STROBE no adjust MAIN confounder set
## gee function
RunGeeGlmOR_m2_unadj <- function(chronicFatigue, df) {
  tmp1 <- df[[chronicFatigue]]
  tmp2 <- geeglm(outcome ~ treatment + obese + Atrial_Fibrillation + Chron_Kidney_Disease + Heart_Failure, id = EmployeeZipCode, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- chronicFatigue
  return(tmp3)
}


maptmp2unadj082520 <- map(names(chronicDisease)[1:3], RunGeeGlmOR_m2_unadj, maptmp1) %>% setNames(names(chronicDisease)[1:3])
geeEstdf_m2_unadj <- bind_rows(maptmp2unadj082520, .id = "id") 
geeEstdf_m2_unadj$fdr <- p.adjust(geeEstdf_m2_unadj$p.value, method = 'fdr')


# ## END ****************************************************************************************




## **********************
## Model 3 ----

## sample Lyme in df
sampleLyme <- function(fraction) {
  ## unique lyme ID
  tmp1 <- comoDf %>% filter(yeartype == "p3_baseline", treatment == "lyme")
  tmp2 <- tmp1$MemberId %>% unique
  set.seed(132) # must sit inside the function
  tmp3 <- sample(tmp2, fraction*length(tmp2))
  
  ## cut df
  tmp4 <- comoDf %>% filter(treatment == "lyme")
  tmp5 <- comoDf %>% filter(treatment == "ct")
  
  ## filter
  tmp6 <- tmp4 %>% filter(MemberId %in% tmp3)
  
  ## bind
  tmp7 <- bind_rows(tmp6, tmp5)
}


comoDf_m3 <- sampleLyme(0.3)





## **********************
## CHECK case count in the model's key strata

nCt1_m3 <- comoDf_m3 %>% group_by(chronicDiseaseSwitch, yeartype, treatment) %>% summarise(
    cases = sum(chronicClass != "no"),
    totaln = n()
)

# to excel table oder, n count.

excelN <- function(tmp1) {
    tmp11 <- tmp1 %>% arrange(treatment, desc(yeartype)) %>% arrange(chronicDiseaseSwitch)
    tmp11 <- tmp11 %>% unite("Year_Treat", c("yeartype", "treatment")) %>% select(-totaln)
    tmp11 <- tmp11 %>% spread(Year_Treat, cases)
    tmp11 <- tmp11 %>% slice(match(names(chronicDisease), chronicDiseaseSwitch))
    tmp11 <- tmp11[order(match(tmp11$chronicDiseaseSwitch, names(chronicDisease))),]
    return(tmp11)
}

nCt2_m3 <- excelN(nCt1_m3)




## prep data
# move comor from row to column, m2 function
maptmp_m3 <- map(names(chronicDisease)[1:3], possibly(GetGLMdf1, NA), comoDf_m3) %>% setNames(names(chronicDisease)[1:3])


## gee
# using m2 function
maptmp2_m3 <- map(names(chronicDisease)[1:3], RunGeeGlmOR_m2, maptmp_m3) %>% setNames(names(chronicDisease)[1:3])
geeEstdf_m3 <- bind_rows(maptmp2_m3, .id = "id") 
geeEstdf_m3$fdr <- p.adjust(geeEstdf_m3$p.value, method = 'fdr')


# ## START ****************************************************************************************
## **********************
# 082520 STROBE no adjust MAIN confounder set
# using m2 function

maptmp2unadj082520 <- map(names(chronicDisease)[1:3], RunGeeGlmOR_m2_unadj, maptmp_m3) %>% setNames(names(chronicDisease)[1:3])
geeEstdf_m3_unadj <- bind_rows(maptmp2unadj082520, .id = "id") 
geeEstdf_m3_unadj$fdr <- p.adjust(geeEstdf_m2_unadj$p.value, method = 'fdr')


# ## END ****************************************************************************************




## **********************
## Sensitivity Analaysis ----


## function
senAnaly <- function(fractionInput) {
  
  comoDf_m3 <- sampleLyme(fractionInput)
  
  
  ## prep data
  # move comor from row to column, m2 function
  maptmp_m3 <- map(names(chronicDisease)[1:3], possibly(GetGLMdf1, NA), comoDf_m3) %>% setNames(names(chronicDisease)[1:3])
  
  
  ## gee
  # using m2 function
  maptmp2_m3 <- map(names(chronicDisease)[1:3], RunGeeGlmOR_m2, maptmp_m3) %>% setNames(names(chronicDisease)[1:3])
  geeEstdf_m3 <- bind_rows(maptmp2_m3, .id = "id") 
  geeEstdf_m3$fdr <- p.adjust(geeEstdf_m3$p.value, method = 'fdr')

  ## return
  geeEstdf_m3
}

## run
fractVect <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

maptmp2_senAn <- map(fractVect, senAnaly) %>% setNames(fractVect)
geeEstdf_senAn <- bind_rows(maptmp2_senAn, .id = "fraction_lyme") 



# # ******** -----
# ## A.1. Saving Rdata ----
# 
outdirectory <- "LID_data_012721"
outfilename <- "case_control_ewas_wAB_030920_onlyAB.Rdata"
save(file=file.path(outdirectory,outfilename),
     lyme, ct, lymeWab, ctWab, comoDf, geeEstdf_m1, geeEstdf_m2_pLoop, geeEstdf_m2, geeEstdf_m3, geeEstdf_senAn, nCt1, nCt2)



write_csv(nCt2, path = file.path("LID_results_012721", "nCt2_m1m2.csv")) # same N in short format
write_csv(nCt2_m3, path = file.path("LID_results_012721", "nCt2_m3.csv")) # same N in short format
write_csv(geeEstdf_m1, path = file.path("LID_results_012721", "geeEstdf_m1.csv"))  # m1
write_csv(geeEstdf_m1_unadj, path = file.path("LID_results_012721", "geeEstdf_m1_unadj.csv"))  # m1, unadjusted
write_csv(geeEstdf_m2_pLoop, path = file.path("LID_results_012721", "geeEstdf_m2_pLoop.csv"))  # m2, step 1 (find P covariate)
write_csv(geeEstdf_m2, path = file.path("LID_results_012721", "geeEstdf_m2.csv"))  # m2, step 2 
write_csv(geeEstdf_m2_unadj, path = file.path("LID_results_012721", "geeEstdf_m2_unadj.csv"))  # m2, step 2, unadjusted
write_csv(geeEstdf_m3, path = file.path("LID_results_012721", "geeEstdf_m3.csv"))  # m3
write_csv(geeEstdf_m3_unadj, path = file.path("LID_results_012721", "geeEstdf_m3_unadj.csv"))  # m3, unadjusted
write_csv(geeEstdf_senAn, path = file.path("LID_results_012721", "geeEstdf_senAn.csv"))  # m3, sensitiivty analysis


