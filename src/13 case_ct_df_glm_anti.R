## Nov 28 2019
## Goal: Lyme and control data run regression, with Drug as cov

rm(list=ls()) # clear workspace
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000
options(warn=1) # default = 0. 

## Load data
load("./data/case_control_wAB_110419.Rdata")
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
## CREATE final df ----

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

# geedf %>% group_by(chronicDiseaseSwitch, TP, lymeDisease) %>% tally
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
library(geepack); library(broom)

## prepare 
pmapvar <- comoDf %>% select(yeartype, chronicDiseaseSwitch) %>% distinct
pmapvar$mergeNames <- str_c(pmapvar$chronicDiseaseSwitch, pmapvar$yeartype, sep = " ")


# pmapvar <- pmapvar[1:2, ]

## gee function
RunGeeGlmOR <- function(p1_pre2, chronicFatigue, tmpdata) {
    tmp1 <- tmpdata %>% filter(yeartype == p1_pre2, chronicDiseaseSwitch == chronicFatigue)
    tmp2 <- geeglm(outcome ~ treatment + age + MemberGender + uniqueCtIcd + DrugInfo + AntiInfo + stateID, id = EmployeeZipCode, family = binomial, data = tmp1, corstr = "exchangeable")
    tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
    tmp3$chronicDisease <- chronicFatigue
    tmp3$yeartype <- p1_pre2
    return(tmp3)
}


pmapvar <- pmapvar %>% filter(yeartype == "p3_baseline") #From 195 -> 39 rows
maptmp1 <- pmap(list(pmapvar$yeartype, pmapvar$chronicDiseaseSwitch), possibly(RunGeeGlmOR, NA), comoDf) %>% setNames(pmapvar$mergeNames)

## Extract results to paper table
geeEstdf <- bind_rows(maptmp1, .id = "id") 

geeEstdf$fdr <- p.adjust(geeEstdf$p.value, method = 'fdr')



# ## START ****************************************************************************************
## **********************
# 082520 STROBE without adjust
## gee function
RunGeeGlmORUNadj082520 <- function(p1_pre2, chronicFatigue, tmpdata) {
  tmp1 <- tmpdata %>% filter(yeartype == p1_pre2, chronicDiseaseSwitch == chronicFatigue)
  tmp2 <- geeglm(outcome ~ treatment, id = EmployeeZipCode, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- chronicFatigue
  tmp3$yeartype <- p1_pre2
  return(tmp3)
}


# Use OR and pmap
pmapvar <- pmapvar %>% filter(yeartype == "p3_baseline") #From 195 -> 39 rows
maptmp1unadj082520 <- pmap(list(pmapvar$yeartype, pmapvar$chronicDiseaseSwitch), possibly(RunGeeGlmORUNadj082520, NA), comoDf) %>% setNames(pmapvar$mergeNames)

## Extract results to paper table
geeEstdfUnadj082520 <- bind_rows(maptmp1unadj082520, .id = "id") 

geeEstdfUnadj082520$fdr <- p.adjust(geeEstdfUnadj082520$p.value, method = 'fdr')

# ## END ****************************************************************************************




## **********************
## RUN glm. additional 110919; sensitivity analysis 050620
## prepare data

## function (add 7 como)
GetGLMdf1 <- function(chronicFatigue) {
  tmp1 <- comoDf %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == chronicFatigue) #row = 26329
  tmp2 <- comoDf %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "coInfection") %>% select(outcome) %>% setNames("coInfection")
  tmp3 <- comoDf %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "obese") %>% select(outcome) %>% setNames("obese")
  tmp4 <- comoDf %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "narcolepsy") %>% select(outcome) %>% setNames("narcolepsy")
  tmp5 <- comoDf %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "autoimmune") %>% select(outcome) %>% setNames("autoimmune")
  tmp6 <- comoDf %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "Hypothyroidism") %>% select(outcome) %>% setNames("Hypothyroidism")
  tmp7 <- comoDf %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "Chron_Kidney_Disease") %>% select(outcome) %>% setNames("Chron_Kidney_Disease")
  tmp8 <- comoDf %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "Heart_Failure") %>% select(outcome) %>% setNames("Heart_Failure")
  tmp9 <- comoDf %>% filter(yeartype == "p3_baseline", chronicDiseaseSwitch == "Pelvic_Fracture") %>% select(outcome) %>% setNames("Pelvic_Fracture")
  
  comoAdjDf <- bind_cols(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9)
  comoAdjDf$coInfection <- as.factor(comoAdjDf$coInfection)
  comoAdjDf$obese <- as.factor(comoAdjDf$obese)
  comoAdjDf$narcolepsy <- as.factor(comoAdjDf$narcolepsy)
  comoAdjDf$autoimmune <- as.factor(comoAdjDf$autoimmune)
  comoAdjDf$Hypothyroidism <- as.factor(comoAdjDf$Hypothyroidism)
  comoAdjDf$Chron_Kidney_Disease <- as.factor(comoAdjDf$Chron_Kidney_Disease)
  comoAdjDf$Heart_Failure <- as.factor(comoAdjDf$Heart_Failure)
  comoAdjDf$Pelvic_Fracture <- as.factor(comoAdjDf$Pelvic_Fracture)
  
  return(comoAdjDf)
}


maptmp1 <- map(names(chronicDisease)[1:3], possibly(GetGLMdf1, NA)) %>% setNames(names(chronicDisease)[1:3])

## run geeglm

## gee function
RunGeeGlmOR1 <- function(chronicFatigue) {
    tmp1 <- maptmp1[[chronicFatigue]]
    tmp2 <- geeglm(outcome ~ treatment + age + MemberGender + uniqueCtIcd + DrugInfo + AntiInfo + stateID + coInfection + obese + narcolepsy + autoimmune + Chron_Kidney_Disease +Heart_Failure + Pelvic_Fracture, id = EmployeeZipCode, family = binomial, data = tmp1, corstr = "exchangeable")
    tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
    tmp3$chronicDisease <- chronicFatigue
    return(tmp3)
}

maptmp2 <- map(names(chronicDisease)[1:3], RunGeeGlmOR1) %>% setNames(names(chronicDisease)[1:3])
geeEstdf1 <- bind_rows(maptmp2, .id = "id") 
geeEstdf1$fdr <- p.adjust(geeEstdf1$p.value, method = 'fdr')



# ## START ****************************************************************************************
## **********************
# 082520 STROBE no adjust MAIN confounder set
## gee function
RunGeeGlmOR1unadj082520 <- function(chronicFatigue) {
  tmp1 <- maptmp1[[chronicFatigue]]
  tmp2 <- geeglm(outcome ~ treatment + coInfection + obese + narcolepsy + autoimmune + Chron_Kidney_Disease + Heart_Failure + Pelvic_Fracture, id = EmployeeZipCode, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- chronicFatigue
  return(tmp3)
}


maptmp2unadj082520 <- map(names(chronicDisease)[1:3], RunGeeGlmOR1unadj082520) %>% setNames(names(chronicDisease)[1:3])
geeEstdf1unadjust082520 <- bind_rows(maptmp2unadj082520, .id = "id") 
geeEstdf1unadjust082520$fdr <- p.adjust(geeEstdf1unadjust082520$p.value, method = 'fdr')


# ## END ****************************************************************************************





# # ******** -----
# ## A.1. Saving Rdata ----
# 
outdirectory <- "data"
outfilename <- "case_control_ewas_wAB_110419.Rdata"
save(file=file.path(outdirectory,outfilename),
     lyme, ct, lymeWab, ctWab, comoDf, geeEstdf, geeEstdf1, nCt1, nCt2)

write_csv(nCt1, path = file.path("results", "nCt1.csv")) # same N in long format
write_csv(nCt2, path = file.path("results", "nCt2.csv")) # same N in short format
write_csv(geeEstdf, path = file.path("results", "geeEstdfMain.csv")) # main analysis
write_csv(geeEstdfUnadj082520, path = file.path("results", "geeEstdfMainUnadj.csv")) # main analysis without adjust
write_csv(geeEstdf1, path = file.path("results", "geeEstdfSensAdjust.csv")) # sensitivity analysis by adjustig selected Y
write_csv(geeEstdf1unadjust082520, path = file.path("results", "geeEstdfSensAdjustUnadj.csv")) # sensitivity analysis by adjustig selected Y, without adjust main confounder set
