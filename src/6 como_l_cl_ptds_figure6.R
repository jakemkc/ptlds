## Nov 29 2018
## Goal: get comorbidity

rm(list=ls()) # clear workspace;  # ls() # list objects in the workspace
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000, and rstudio set at 5000
options(warn=1) # default = 0. To check loop warnings



# Load data
load("/Volumes/O2_transfer/data/desc_cleanup_lyme_clyme_ptlds_exclusive.Rdata") 
lyme3como <- readRDS("/Volumes/O2_transfer/data/SQL/lymecomo113018.rds")  # lyme



# ******** -----
# A. get exclusive disease como fraction ---------------------------------------------------------------------------

library(tidyverse); library(lubridate); library(CGPfunctions)

# only extracted lyme's as one file in aetna server
lymecomo <- lyme3como[which(lyme3como$MemberID %in% lyme$MemberId), ]
clymecomo <- lyme3como[which(lyme3como$MemberID %in% clyme$MemberId), ]
ptldscomo <- lyme3como[which(lyme3como$MemberID %in% ptlds$MemberId), ]



## find most freq como in lyme

freqCount <- function(lymecomo) {
    
    lymecomo$age <- 2017 - lymecomo$MemberBirthYear
    lymecomo[which(lymecomo$age > 100), "age"] <- 100
    
    lymecomo$year <- year(ymd(lymecomo$DateServiceStarted))
    lymecomo$coyear <- year(ymd(lymecomo$coICDdate))
    
    lymecomo <- lymecomo %>% select(-c(HMSAetnaLineID, DateServiceStarted, MemberBirthYear, coICDdate))
    
    lymecomo$yeartype <- case_when(
        lymecomo$coyear - lymecomo$year == 2 ~ "p5_post2",
        lymecomo$coyear - lymecomo$year == 1 ~ "p4_post1",
        lymecomo$coyear - lymecomo$year == 0 ~ "p3_baseline",
        lymecomo$coyear - lymecomo$year == -1 ~ "p2_pre1",
        lymecomo$coyear - lymecomo$year == -2 ~ "p1_pre2")
    
    return(lymecomo)
}


# Rough check of NA (not by icd not by class, just all as a whole)
x1 <- freqCount(lymecomo) %>% filter(!is.na(yeartype)) %>% distinct %>% group_by(yeartype) %>% count %>% arrange(desc(n))
x1 <- freqCount(clymecomo) %>% filter(!is.na(yeartype)) %>% distinct %>% group_by(yeartype) %>% count %>% arrange(desc(n))
x1 <- freqCount(ptldscomo) %>% filter(!is.na(yeartype)) %>% distinct %>% group_by(yeartype) %>% count %>% arrange(desc(n))


# for top10 group 
tmp1 <- freqCount(lymecomo) %>% filter(yeartype == "p2_pre1") %>% distinct %>% group_by(coICD) %>% count %>% arrange(desc(n))
tmp2 <- freqCount(lymecomo) %>% filter(yeartype == "p4_post1") %>% distinct %>% group_by(coICD) %>% count %>% arrange(desc(n))

intersect(tmp1$coICD[1:10], tmp2$coICD[1:10]) %>% length # 10 highest abundant icd to look at
top10lymeco<- intersect(tmp1$coICD[1:10], tmp2$coICD[1:10]) # %>% dput 


tmp1 <- freqCount(clymecomo) %>% filter(yeartype == "p2_pre1") %>% distinct %>% group_by(coICD) %>% count %>% arrange(desc(n))
tmp2 <- freqCount(clymecomo) %>% filter(yeartype == "p4_post1") %>% distinct %>% group_by(coICD) %>% count %>% arrange(desc(n))

intersect(tmp1$coICD[1:10], tmp2$coICD[1:10]) %>% length # 10 highest abundant icd to look at
top10clymeco<- intersect(tmp1$coICD[1:10], tmp2$coICD[1:10]) # %>% dput 


tmp1 <- freqCount(ptldscomo) %>% filter(yeartype == "p2_pre1") %>% distinct %>% group_by(coICD) %>% count %>% arrange(desc(n))
tmp2 <- freqCount(ptldscomo) %>% filter(yeartype == "p4_post1") %>% distinct %>% group_by(coICD) %>% count %>% arrange(desc(n))

intersect(tmp1$coICD[1:11], tmp2$coICD[1:11]) %>% length # 11 highest abundant icd to look at
top10ptldsco <- intersect(tmp1$coICD[1:11], tmp2$coICD[1:11]) # %>% dput 


## create variable

dataprepVar <- function(lymecomo, filterICD) {

  lymecomo$age <- 2017 - lymecomo$MemberBirthYear
  lymecomo[which(lymecomo$age > 100), "age"] <- 100
  
  lymecomo$year <- year(ymd(lymecomo$DateServiceStarted))
  lymecomo$coyear <- year(ymd(lymecomo$coICDdate))
  
  lymecomo <- lymecomo %>% select(-c(HMSAetnaLineID, DateServiceStarted, MemberBirthYear, coICDdate))
  
  lymecomo$yeartype <- case_when(
      lymecomo$coyear - lymecomo$year == 2 ~ "p5_post2",
      lymecomo$coyear - lymecomo$year == 1 ~ "p4_post1",
      lymecomo$coyear - lymecomo$year == 0 ~ "p3_baseline",
      lymecomo$coyear - lymecomo$year == -1 ~ "p2_pre1",
      lymecomo$coyear - lymecomo$year == -2 ~ "p1_pre2")
  
  lymecomo <- lymecomo %>% filter(!is.na(yeartype))
  
  lymecomo <- lymecomo %>% filter(coICD %in% filterICD) %>% distinct
  
  return(lymecomo)
}

## prep intermediate dataframe

lymecomotop <- dataprepVar(lymecomo, top10lymeco)
clymecomotop <- dataprepVar(clymecomo, top10clymeco)
ptldscomotop <- dataprepVar(ptldscomo, top10ptldsco)





### ........ ---------------------------------------------------------------
### C. Plot ------------------------------------------------------------


## prep data
plotPrepData <- function(lymecomotop, lymecomo) {
    tmp1 <- lymecomotop %>% group_by(yeartype, coICD) %>% count() %>% arrange(desc(coICD)) %>% as.data.frame # input df is distinct already
    tmp2 <- freqCount(lymecomo) %>% filter(!is.na(yeartype)) %>% distinct %>% group_by(yeartype) %>% count %>% arrange(desc(n))
    tmp3 <- left_join(tmp1, tmp2, by = "yeartype")
    tmp3$percentN <- (tmp3$n.x/tmp3$n.y*100) %>% round(., digits = 2)
    tmp3$coICD <- tmp3$coICD %>% as.factor
    tmp3$yeartype <- tmp3$yeartype %>% factor(., ordered = TRUE) # can't use as.factor() here
    levels(tmp3$yeartype) <- c("previous_2_yrs", "previous_1_yr", "baseline", "next_1_yr", "next_2_yrs")
    return(tmp3)
}




tmp1 <- plotPrepData(lymecomotop, lymecomo)
p <- newggslopegraph(
    dataframe = tmp1,
    Times = yeartype,
    # Measurement = n, 
    Measurement = percentN, 
    Grouping = coICD,
    Title = "Top 10 comorbidity",
    SubTitle = "+ and - 2 years from first report of lyme; ICD % is shown")

# ggsave("./results/lymecomoTop10.png", plot = p, scale = 1, dpi = 400)


tmp1 <- plotPrepData(clymecomotop, clymecomo)
p <- newggslopegraph(
    dataframe = tmp1,
    Times = yeartype,
    # Measurement = n, 
    Measurement = percentN, 
    Grouping = coICD,
    Title = "Top 10 comorbidity",
    SubTitle = "+ and - 2 years from first report of lyme; ICD % is shown")

# ggsave("./results/clymecomoTop10.png", plot = p, scale = 1, dpi = 400)

tmp1 <- plotPrepData(ptldscomotop, ptldscomo)
p <- newggslopegraph(
    dataframe = tmp1,
    Times = yeartype,
    # Measurement = n, 
    Measurement = percentN, 
    Grouping = coICD,
    Title = "Top 10 comorbidity", # 11
    SubTitle = "+ and - 2 years from first report of lyme; ICD % is shown")

# ggsave("./results/ptldscomoTop10.png", plot = p, scale = 1, dpi = 400)



### ........ ---------------------------------------------------------------
### D. Plot-byclass ------------------------------------------------------------

## prepare chronic disease class
chronicDisease = list(
    Hypothyroidism = c(
        '244.0',
        '244.1',
        '244.2',
        '244.3',
        '244.8',
        '244.9'),
    Acute_Myocardial_Infarction = c(
        '410.01',
        '410.11',
        '410.21',
        '410.31',
        '410.41',
        '410.51',
        '410.61',
        '410.71',
        '410.81',
        '410.91'),
    Anemia = c(
        '280.0',
        '280.1',
        '280.8',
        '280.9',
        '281.1',
        '281.2',
        '281.3',
        '281.4',
        '281.8',
        '281.9',
        '282.0',
        '282.1',
        '282.2',
        '282.3',
        '282.40',
        '282.41',
        '282.42',
        '282.43',
        '282.44',
        '282.45',
        '282.46',
        '282.47',
        '282.49',
        '282.5',
        '282.60',
        '282.61',
        '282.62',
        '282.63',
        '282.64',
        '282.68',
        '282.69',
        '282.7',
        '282.8',
        '282.9',
        '283.0',
        '283.10',
        '283.11',
        '283.19',
        '283.2',
        '283.9',
        '284.01',
        '284.09',
        '284.11',
        '284.12',
        '284.19',
        '284.2',
        '284.81',
        '284.89',
        '284.9',
        '285.0',
        '285.1',
        '285.21',
        '285.22',
        '285.29',
        '285.3',
        '285.8',
        '285.9'),
    Asthma = c(
        '493.00',
        '493.01',
        '493.02',
        '493.10',
        '493.11',
        '493.12',
        '493.20',
        '493.21',
        '493.22',
        '493.81',
        '493.82',
        '493.90',
        '493.91',
        '493.92'),
    Atrial_Fibrillation = c(
        '427.31'),
    Prostatic_Hyperplasia = c(
        '600.00',
        '600.01',
        '600.10',
        '600.11',
        '600.20',
        '600.21',
        '600.3',
        '600.90',
        '600.91'),
    Cataract = c(
        '366.01',
        '366.02',
        '366.03',
        '366.04',
        '366.09',
        '366.10',
        '366.12',
        '366.13',
        '366.14',
        '366.15',
        '366.16',
        '366.17',
        '366.18',
        '366.19',
        '366.20',
        '366.21',
        '366.22',
        '366.23',
        '366.30',
        '366.45',
        '366.46',
        '366.50',
        '366.51',
        '366.52',
        '366.53',
        '366.8',
        '366.9',
        '379.26',
        '379.31',
        '379.39',
        '743.30',
        '743.31',
        '743.32',
        '743.33',
        'V43.1'),
    Chron_Kidney_Disease = c(
        '016.00',
        '016.01',
        '016.02',
        '016.03',
        '016.04',
        '016.05',
        '016.06',
        '095.4',
        '223.0',
        '249.40',
        '249.41',
        '250.40',
        '250.41',
        '250.42',
        '250.43',
        '271.4',
        '274.10',
        '283.11',
        '403.01',
        '403.11',
        '403.91',
        '404.02',
        '404.03',
        '404.12',
        '404.13',
        '404.92',
        '404.93',
        '440.1',
        '442.1',
        '580.0',
        '580.4',
        '580.81',
        '580.89',
        '580.9',
        '581.0',
        '581.1',
        '581.2',
        '581.3',
        '581.81',
        '581.89',
        '581.9',
        '582.0',
        '582.1',
        '582.2',
        '582.4',
        '582.81',
        '582.89',
        '582.9',
        '583.0',
        '583.1',
        '583.2',
        '583.4',
        '583.6',
        '583.7',
        '583.81',
        '583.89',
        '583.9',
        '584.5',
        '584.6',
        '584.7',
        '584.8',
        '584.9',
        '585.1',
        '585.2',
        '585.3',
        '585.4',
        '585.5',
        '585.6',
        '585.9',
        '586',
        '587',
        '588.0',
        '588.1',
        '588.81',
        '588.89',
        '588.9',
        '591',
        '753.12',
        '753.13',
        '753.14',
        '753.15',
        '753.16',
        '753.17',
        '753.19',
        '753.20',
        '753.21',
        '753.22',
        '753.23',
        '753.29',
        '794.4'),
    Pulmonary_Disease = c(
        '490',
        '491.0',
        '491.1',
        '491.8',
        '491.9',
        '492.0',
        '492.8',
        '491.20',
        '491.21',
        '491.22',
        '494.0',
        '494.1',
        '496'),
    Diabetes = c(
        '249.00',
        '249.10',
        '249.20',
        '249.30',
        '249.40',
        '249.41',
        '249.50',
        '249.60',
        '249.70',
        '249.80',
        '249.90',
        '250.00',
        '250.10',
        '250.11',
        '250.20',
        '250.30',
        '250.40',
        '250.41',
        '250.42',
        '250.43',
        '250.50',
        '250.51',
        '250.60',
        '250.61',
        '250.70',
        '250.71',
        '250.73',
        '250.80',
        '250.81',
        '250.90',
        '357.2',
        '362.01',
        '362.02',
        '362.03',
        '362.04',
        '362.05',
        '362.06',
        '366.41'),
    Glaucoma = c(
        '362.85',
        '365.00',
        '365.01',
        '365.02',
        '365.03',
        '365.04',
        '365.10',
        '365.11',
        '365.12',
        '365.13',
        '365.15',
        '365.20',
        '365.21',
        '365.22',
        '365.23',
        '365.24',
        '365.31',
        '365.32',
        '365.41',
        '365.42',
        '365.43',
        '365.51',
        '365.52',
        '365.59',
        '365.60',
        '365.61',
        '365.62',
        '365.63',
        '365.64',
        '365.65',
        '365.81',
        '365.82',
        '365.83',
        '365.89',
        '365.9',
        '377.14'),
    Heart_Failure = c(
        '398.91',
        '402.01',
        '402.11',
        '402.91',
        '404.01',
        '404.03',
        '404.11',
        '404.13',
        '404.91',
        '404.93',
        '428.0',
        '428.1',
        '428.20',
        '428.21',
        '428.22',
        '428.23',
        '428.30',
        '428.31',
        '428.32',
        '428.33',
        '428.40',
        '428.41',
        '428.42',
        '428.43',
        '428.9'),
    Pelvic_Fracture = c(
        '733.14',
        '733.15',
        '733.96',
        '733.97',
        '733.98',
        '808.0',
        '808.1',
        '808.2',
        '808.3',
        '808.41',
        '808.42',
        '808.43',
        '808.44',
        '808.49',
        '808.51',
        '808.52',
        '808.53',
        '808.54',
        '808.59',
        '808.8',
        '808.9',
        '820.00',
        '820.01',
        '820.02',
        '820.03',
        '820.09',
        '820.10',
        '820.11',
        '820.12',
        '820.13',
        '820.19',
        '820.20',
        '820.21',
        '820.22',
        '820.30',
        '820.31',
        '820.32',
        '820.8',
        '820.9'),
    Hyperlipidemia = c(
        '272.0',
        '272.1',
        '272.2',
        '272.3',
        '272.4'),
    Hypertension = c(
        '362.11',
        '401.0',
        '401.1',
        '401.9',
        '402.00',
        '402.01',
        '402.10',
        '402.11',
        '402.90',
        '402.91',
        '403.00',
        '403.01',
        '403.10',
        '403.11',
        '403.90',
        '403.91',
        '404.00',
        '404.01',
        '404.02',
        '404.03',
        '404.10',
        '404.11',
        '404.12',
        '404.13',
        '404.90',
        '404.91',
        '404.92',
        '404.93',
        '405.01',
        '405.09',
        '405.11',
        '405.19',
        '405.91',
        '405.99',
        '437.2'),
    Ischemic_Heart_Disease = c(
        '410.00',
        '410.01',
        '410.02',
        '410.10',
        '410.11',
        '410.12',
        '410.20',
        '410.21',
        '410.22',
        '410.30',
        '410.31',
        '410.32',
        '410.40',
        '410.41',
        '410.42',
        '410.50',
        '410.51',
        '410.52',
        '410.60',
        '410.61',
        '410.62',
        '410.70',
        '410.71',
        '410.72',
        '410.80',
        '410.81',
        '410.82',
        '410.90',
        '410.91',
        '410.92',
        '411.0',
        '411.1',
        '411.81',
        '411.89',
        '412',
        '413.0',
        '413.1',
        '413.9',
        '414.00',
        '414.01',
        '414.02',
        '414.03',
        '414.04',
        '414.05',
        '414.06',
        '414.07',
        '414.12',
        '414.2',
        '414.3',
        '414.4',
        '414.8',
        '414.9'),
    Osteoporosis = c(
        '733.00',
        '733.01',
        '733.02',
        '733.03',
        '733.09'),
    Rheumatoid_Arthritis = c(
        '715.00',
        '715.04',
        '715.09',
        '715.10',
        '715.11',
        '715.12',
        '715.13',
        '715.14',
        '715.15',
        '715.16',
        '715.17',
        '715.18',
        '715.20',
        '715.21',
        '715.22',
        '715.23',
        '715.24',
        '715.25',
        '715.26',
        '715.27',
        '715.28',
        '715.30',
        '715.31',
        '715.32',
        '715.33',
        '715.34',
        '715.35',
        '715.36',
        '715.37',
        '715.38',
        '715.80',
        '715.89',
        '715.90',
        '715.91',
        '715.92',
        '715.93',
        '715.94',
        '715.95',
        '715.96',
        '715.97',
        '715.98',
        '720.0',
        '721.0',
        '721.1',
        '721.2',
        '721.3',
        '721.90',
        '721.91'),
    Stroke = c(
        '430',
        '431',
        '433.01',
        '433.11',
        '433.21',
        '433.31',
        '433.81',
        '433.91',
        '434.00',
        '434.01',
        '434.10',
        '434.11',
        '434.90',
        '434.91',
        '435.0',
        '435.1',
        '435.3',
        '435.8',
        '435.9',
        '436',
        '997.02')
)


chronicDiseasePrep <- function(lymecomo) {
  
    lymecomo$chronicClass <- "n____"
  
    for(i in 1:length(chronicDisease)) {
        mtchC <- which(lymecomo$coICD %in% chronicDisease[[i]])
        lymecomo$chronicClass[mtchC] <- names(chronicDisease)[i]
    }
    return(lymecomo)
}


lymecomoClass <- chronicDiseasePrep(lymecomo)
clymecomoClass <- chronicDiseasePrep(clymecomo)
ptldscomoClass <- chronicDiseasePrep(ptldscomo)


# intermediate data prep

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
clymecomoclassInt <- dataprepClass(clymecomoClass)
ptldscomoClassInt <- dataprepClass(ptldscomoClass)



tmp1 <- ptldscomoClassInt %>% filter(chronicClass == "Rheumatoid_Arthritis")
tmp1$MemberID %>% unique %>% length

# plot data prep
plotPrepClass <- function(lymecomoClassInt, lymecomo) {
    tmp1 <- lymecomoClassInt %>% group_by(yeartype, chronicClass) %>% count() %>% arrange(desc(chronicClass)) %>% as.data.frame 
    tmp2 <- freqCount(lymecomo) %>% filter(!is.na(yeartype)) %>% distinct %>% group_by(yeartype) %>% count %>% arrange(desc(n))
    tmp3 <- left_join(tmp1, tmp2, by = "yeartype")
    tmp3$percentN <- (tmp3$n.x/tmp3$n.y*100) %>% round(., digits = 2)
    tmp3$chronicClass <- tmp3$chronicClass %>% as.factor
    tmp3$yeartype <- tmp3$yeartype %>% factor(., ordered = TRUE) # can't use as.factor() here
    levels(tmp3$yeartype) <- c("previous_2_yrs", "previous_1_yr", "baseline", "next_1_yr", "next_2_yrs")
    return(tmp3)
}


plotPrepClass2 <- function(ptldscomoClassInt, ptlds) {
    tmp1 <- ptldscomoClassInt %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() %>% group_by(yeartype, chronicClass) %>% count() %>% arrange(desc(chronicClass)) %>% as.data.frame 
    tmp2 <- tmp1 %>% filter(!(yeartype == "p1_pre2")) %>% mutate(total = nrow(ptlds))
    tmp2$percentN <- (tmp2$n/tmp2$total*100) %>% round(., digits = 2)
    tmp2$chronicClass <- tmp2$chronicClass %>% as.factor
    tmp2$yeartype <- tmp2$yeartype %>% factor(., ordered = TRUE) # can't use as.factor() here
    levels(tmp2$yeartype) <- c("previous_1_yr", "baseline", "next_1_yr", "next_2_yrs")
    return(tmp2)
}




## plot
# tmp1 <- plotPrepClass(lymecomoClassInt, lymecomo)
tmp1 <- plotPrepClass2(lymecomoClassInt, lyme)
p <- newggslopegraph(
    dataframe = tmp1,
    Times = yeartype,
    # Measurement = n.x, 
    Measurement = percentN, 
    Grouping = chronicClass,
    # tune for on screen
    # XTextSize = 15, # top baseline x label sie
    # YTextSize = 3, # disease name size
    # DataTextSize = 3.5,
    # tuen for printing
    XTextSize = 20, # top baseline x label sie
    YTextSize = 4.5, # disease name size
    DataTextSize = 4.5,
    WiderLabels = TRUE,
    Title = "chronic diseases comobidity",
    SubTitle = "+ and - 2 years from first report of lyme; ICD % is shown")

# ggsave("./results/lymecomoClass.png", plot = p, scale = 1, dpi = 400)
# ggsave("./results/lymecomoClass.pdf", plot = p, scale = 1, dpi = 400)



# tmp1 <- plotPrepClass(ptldscomoClassInt, ptldscomo)
tmp1 <- plotPrepClass2(ptldscomoClassInt, ptlds)
p <- newggslopegraph(
    dataframe = tmp1,
    Times = yeartype,
    # Measurement = n.x, 
    Measurement = percentN, 
    Grouping = chronicClass,
    # tune for on screen
    # XTextSize = 15, # top baseline x label sie
    # YTextSize = 3, # disease name size
    # DataTextSize = 3.5,
    # tuen for printing
    XTextSize = 20, # top baseline x label sie
    YTextSize = 4.5, # disease name size
    DataTextSize = 4.5,
    WiderLabels = TRUE,
    Title = "chronic diseases comobidity",
    SubTitle = "+ and - 2 years from first report of lyme; ICD % is shown")

# ggsave("./results/ptldscomoClass.png", plot = p, scale = 1, dpi = 400)
# ggsave("./results/ptldscomoClass.pdf", plot = p, scale = 1, dpi = 400)

### ........ ---------------------------------------------------------------
### E. updated Plot-byclass-age ------------------------------------------------------------
## updated code. same Y interpretation as above without by age, and in cross-over sense



# intermediate data prep, age group

dataprepClassAge <- function(lymecomoClass, switchAge) {
  
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
  
  if (switchAge == "1") {
    lymecomoClass <- lymecomoClass %>% filter(age <= 35)
  } else {
    lymecomoClass <- lymecomoClass %>% filter(age > 35)
  }
  
  return(lymecomoClass)
}



lymecomoClassAgeInt1 <- dataprepClassAge(lymecomoClass, "1")
lymecomoClassAgeInt2 <- dataprepClassAge(lymecomoClass, "2")

clymecomoclassAgeInt1 <- dataprepClassAge(clymecomoClass, "1")
clymecomoclassAgeInt2 <- dataprepClassAge(clymecomoClass, "2")

ptldscomoClassAgeInt1 <- dataprepClassAge(ptldscomoClass, "1")
ptldscomoClassAgeInt2 <- dataprepClassAge(ptldscomoClass, "2")


# similar function as in D. 
plotPrepClass3_young35 <- function(ptldscomoClassAgeInt1, ptlds) {
  tmp1 <- ptldscomoClassAgeInt1 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() %>% group_by(yeartype, chronicClass) %>% count() %>% arrange(desc(chronicClass)) %>% as.data.frame 
  tmpAge <- ptlds %>% filter(age <= 35)
  tmp2 <- tmp1 %>% filter(!(yeartype == "p1_pre2")) %>% mutate(total = nrow(tmpAge))
  tmp2$percentN <- (tmp2$n/tmp2$total*100) %>% round(., digits = 2)
  tmp2$chronicClass <- tmp2$chronicClass %>% as.factor
  tmp2$yeartype <- tmp2$yeartype %>% factor(., ordered = TRUE) # can't use as.factor() here
  levels(tmp2$yeartype) <- c("previous_1_yr", "baseline", "next_1_yr", "next_2_yrs")
  return(tmp2)
}

plotPrepClass3_old35 <- function(ptldscomoClassAgeInt1, ptlds) {
  tmp1 <- ptldscomoClassAgeInt1 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() %>% group_by(yeartype, chronicClass) %>% count() %>% arrange(desc(chronicClass)) %>% as.data.frame 
  tmpAge <- ptlds %>% filter(age > 35)
  tmp2 <- tmp1 %>% filter(!(yeartype == "p1_pre2")) %>% mutate(total = nrow(tmpAge))
  tmp2$percentN <- (tmp2$n/tmp2$total*100) %>% round(., digits = 2)
  tmp2$chronicClass <- tmp2$chronicClass %>% as.factor
  tmp2$yeartype <- tmp2$yeartype %>% factor(., ordered = TRUE) # can't use as.factor() here
  levels(tmp2$yeartype) <- c("previous_1_yr", "baseline", "next_1_yr", "next_2_yrs")
  return(tmp2)
}



## plot
# tmp1 <- plotPrepClass(lymecomoClassAgeInt3, lymecomo)
tmp1 <- plotPrepClass3_young35(lymecomoClassAgeInt1, lyme)
tmp1 <- plotPrepClass3_old35(lymecomoClassAgeInt2, lyme)
p <- newggslopegraph(
  dataframe = tmp1,
  Times = yeartype,
  # Measurement = n.x, 
  Measurement = percentN, 
  Grouping = chronicClass,
  # # tune
  # XTextSize = 15, # top baseline x label sie
  # YTextSize = 4, # disease name size
  # DataTextSize = 3.5,
  # tuen for printing
  XTextSize = 20, # top baseline x label sie
  YTextSize = 4.5, # disease name size
  DataTextSize = 4.5,
  WiderLabels = TRUE,
  Title = "chronic diseases comobidity",
  SubTitle = "+ and - 2 years from first report of lyme; ICD incidence is shown")

# ggsave("./results/lymecomoClassAgeyoung35.pdf", plot = p, scale = 1, dpi = 400)
# ggsave("./results/lymecomoClassAgeold35.pdf", plot = p, scale = 1, dpi = 400)

# ggsave("./results/lymecomoClassAge1.png", plot = p, scale = 1, dpi = 400)
# ggsave("./results/lymecomoClassAge3.png", plot = p, scale = 1, dpi = 400)



# tmp1 <- plotPrepClass(ptldscomoClassAgeInt3, ptldscomo)
tmp1 <- plotPrepClass3_young35(ptldscomoClassAgeInt1, ptlds)
tmp1 <- plotPrepClass3_old35(ptldscomoClassAgeInt2, ptlds)
p <- newggslopegraph(
  dataframe = tmp1,
  Times = yeartype,
  # Measurement = n.x, 
  Measurement = percentN, 
  Grouping = chronicClass,
  # # tune
  # XTextSize = 15, # top baseline x label sie
  # YTextSize = 4, # disease name size
  # DataTextSize = 3.5,
  # tuen for printing
  XTextSize = 20, # top baseline x label sie
  YTextSize = 4.5, # disease name size
  DataTextSize = 4.5,
  WiderLabels = TRUE,
  Title = "chronic diseases comobidity",
  SubTitle = "+ and - 2 years from first report of lyme; ICD incidence is shown")


# ggsave("./results/ptldscomoClassAgeyoung35.pdf", plot = p, scale = 1, dpi = 400)
# ggsave("./results/ptldscomoClassAgeold35.pdf", plot = p, scale = 1, dpi = 400)

# ggsave("./results/ptldscomoClassAge1.png", plot = p, scale = 1, dpi = 400)
# ggsave("./results/ptldscomoClassAge3.png", plot = p, scale = 1, dpi = 400)





### ........ ---------------------------------------------------------------
### F. Test ------------------------------------------------------------



## Join comorbidity info to lyme for a cross-over wide format df

mcnemarTest <- function(Rheumatoid_Arthritis, ptlds, ptldscomoClassInt, p4_post1) {
  tmp1 <- ptldscomoClassInt %>% filter(yeartype == "p2_pre1", chronicClass == Rheumatoid_Arthritis)

  tmp2 <- tmp1 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() # remove duplicate as disease instead of ICD
  
  tmp3 <- left_join(ptlds, tmp2, by = c("MemberId" = "MemberID"))
  
  # extract another time point
  tmp4 <- ptldscomoClassInt %>% filter(yeartype == p4_post1, chronicClass == Rheumatoid_Arthritis)
  
  tmp5 <- tmp4 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct()
  
  tmp6 <- left_join(tmp3, tmp5, by = c("MemberId" = "MemberID"))
 
  ## replace NA in chronic varaible
  tmp6$chronicClass.x <- replace_na(tmp6$chronicClass.x, "no") 
  tmp6$chronicClass.y <- replace_na(tmp6$chronicClass.y, "no") 
  
  ## create table
  mat1 <- table(tmp6$chronicClass.x, tmp6$chronicClass.y)
  
  names(dimnames(mat1)) = c("p2_pre1", "p4_pos1")
   
  # margin.table(mat1, 1)
  # margin.table(mat1, 2)
  # sum(mat1)
  
  tmp7 <- mcnemar.test(mat1, correct=FALSE)
  
  tmp7$p.value
  
}



lymeMcNemar <- tibble(disease = names(chronicDisease))
ptldsMcNemar <- tibble(disease = names(chronicDisease))

lymeMcNemar <- bind_cols(lymeMcNemar, "pre_1_post_1" = map_dbl(names(chronicDisease), possibly(mcnemarTest, NA), lyme, lymecomoClassInt, "p4_post1"))
lymeMcNemar$fdr <- p.adjust(lymeMcNemar$pre_1_post_1)


ptldsMcNemar <- bind_cols(ptldsMcNemar, "pre_1_post_1" = map_dbl(names(chronicDisease), possibly(mcnemarTest, NA), ptlds, ptldscomoClassInt, "p4_post1"))
ptldsMcNemar$fdr <- p.adjust(ptldsMcNemar$pre_1_post_1)



mcnemarPercentPre <- function(Rheumatoid_Arthritis, ptlds, ptldscomoClassInt, p4_post1) {
    tmp1 <- ptldscomoClassInt %>% filter(yeartype == "p2_pre1", chronicClass == Rheumatoid_Arthritis)
    
    tmp2 <- tmp1 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() # remove duplicate as disease instead of ICD
    
    tmp3 <- left_join(ptlds, tmp2, by = c("MemberId" = "MemberID"))
    
    # extract another time point
    tmp4 <- ptldscomoClassInt %>% filter(yeartype == p4_post1, chronicClass == Rheumatoid_Arthritis)
    
    tmp5 <- tmp4 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct()
    
    tmp6 <- left_join(tmp3, tmp5, by = c("MemberId" = "MemberID"))
    
    ## replace NA in chronic varaible
    tmp6$chronicClass.x <- replace_na(tmp6$chronicClass.x, "no") 
    tmp6$chronicClass.y <- replace_na(tmp6$chronicClass.y, "no") 
    
    ## percentage of disease
    tmp7 <- summarytools::freq(tmp6$chronicClass.x, order = "freq")
    
    tmp7[2,2]
}


lymeMcNemar <- bind_cols(lymeMcNemar, "PercentPre" = map_dbl(names(chronicDisease), possibly(mcnemarPercentPre, NA), lyme, lymecomoClassInt, "p4_post1"))
ptldsMcNemar <- bind_cols(ptldsMcNemar, "PercentPre" = map_dbl(names(chronicDisease), possibly(mcnemarPercentPre, NA), ptlds, ptldscomoClassInt, "p4_post1"))


mcnemarPercentPost <- function(Rheumatoid_Arthritis, ptlds, ptldscomoClassInt, p4_post1) {
    tmp1 <- ptldscomoClassInt %>% filter(yeartype == "p2_pre1", chronicClass == Rheumatoid_Arthritis)
    
    tmp2 <- tmp1 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct()
    
    tmp3 <- left_join(ptlds, tmp2, by = c("MemberId" = "MemberID"))
    
    # extract another time point
    tmp4 <- ptldscomoClassInt %>% filter(yeartype == p4_post1, chronicClass == Rheumatoid_Arthritis)
    
    tmp5 <- tmp4 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct()
    
    tmp6 <- left_join(tmp3, tmp5, by = c("MemberId" = "MemberID"))
    
    ## replace NA in chronic varaible
    tmp6$chronicClass.x <- replace_na(tmp6$chronicClass.x, "no")
    tmp6$chronicClass.y <- replace_na(tmp6$chronicClass.y, "no")
    
    ## percentage of disease
    tmp7 <- summarytools::freq(tmp6$chronicClass.y, order = "freq")
    
    tmp7[2,2]
}

lymeMcNemar <- bind_cols(lymeMcNemar, "PercentPost" = map_dbl(names(chronicDisease), possibly(mcnemarPercentPost, NA), lyme, lymecomoClassInt, "p4_post1"))
lymeMcNemar$changefromPre <- lymeMcNemar$PercentPost-lymeMcNemar$PercentPre

ptldsMcNemar <- bind_cols(ptldsMcNemar, "PercentPost" = map_dbl(names(chronicDisease), possibly(mcnemarPercentPost, NA), ptlds, ptldscomoClassInt, "p4_post1"))
ptldsMcNemar$changefromPre <- ptldsMcNemar$PercentPost-ptldsMcNemar$PercentPre







# ........ ---------------------------------------------------------------
### G. GEE model ----

## Prepare dataframe



GetGEEdf <- function(Rheumatoid_Arthritis, p4_post1) {
  
  ## ptlds
  tmp1 <- ptldscomoClassInt %>% filter(yeartype == "p2_pre1", chronicClass == Rheumatoid_Arthritis)
  
  tmp2 <- tmp1 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() # remove duplicate as disease instead of ICD
  
  tmp3 <- left_join(ptlds, tmp2, by = c("MemberId" = "MemberID"))
  
  tmp3.1 <- tmp3 %>% select(MemberId, MemberGender, age, chronicClass)
  
  tmp3.1 <- tmp3.1 %>% mutate(TP = "pre")
  
  tmp3.1 <- tmp3.1 %>% mutate(lymeDisease = "ptlds")
  
  tmp3.1$chronicClass <- replace_na(tmp3.1$chronicClass, "no")
  
  tmp3.1$chronicDiseaseSwitch <- Rheumatoid_Arthritis
  
  # extract another time point
  tmp4 <- ptldscomoClassInt %>% filter(yeartype == p4_post1, chronicClass == Rheumatoid_Arthritis)
  
  tmp5 <- tmp4 %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct()
  
  tmp6 <- left_join(ptlds, tmp5, by = c("MemberId" = "MemberID"))
  
  tmp6.1 <- tmp6 %>% select(MemberId, MemberGender, age, chronicClass)
  
  tmp6.1 <- tmp6.1 %>% mutate(TP = "post")
  
  tmp6.1 <- tmp6.1 %>% mutate(lymeDisease = "ptlds")
  
  tmp6.1$chronicClass <- replace_na(tmp6.1$chronicClass, "no")
  
  tmp6.1$chronicDiseaseSwitch <- Rheumatoid_Arthritis


  ## lyme
  tmp1a <- lymecomoClassInt %>% filter(yeartype == "p2_pre1", chronicClass == Rheumatoid_Arthritis)
  
  tmp2a <- tmp1a %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct() # remove duplicate as disease instead of ICD
  
  tmp3a <- left_join(lyme, tmp2a, by = c("MemberId" = "MemberID"))
  
  tmp3.1a <- tmp3a %>% select(MemberId, MemberGender, age, chronicClass)
  
  tmp3.1a <- tmp3.1a %>% mutate(TP = "pre")
  
  tmp3.1a <- tmp3.1a %>% mutate(lymeDisease = "lyme")
  
  tmp3.1a$chronicClass <- replace_na(tmp3.1a$chronicClass, "no")
  
  tmp3.1a$chronicDiseaseSwitch <- Rheumatoid_Arthritis

  
  # extract another time point
  tmp4a <- lymecomoClassInt %>% filter(yeartype == p4_post1, chronicClass == Rheumatoid_Arthritis)
  
  tmp5a <- tmp4a %>% select(-c(coICD, age, EmployeeZipCode, MemberGender)) %>% distinct()
  
  tmp6a <- left_join(lyme, tmp5a, by = c("MemberId" = "MemberID"))
  
  tmp6.1a <- tmp6a %>% select(MemberId, MemberGender, age, chronicClass)
  
  tmp6.1a <- tmp6.1a %>% mutate(TP = "post")
  
  tmp6.1a <- tmp6.1a %>% mutate(lymeDisease = "lyme")
  
  tmp6.1a$chronicClass <- replace_na(tmp6.1a$chronicClass, "no")
  
  tmp6.1a$chronicDiseaseSwitch <- Rheumatoid_Arthritis
  
  ## bind
  fdata <- bind_rows(tmp3.1, tmp6.1, tmp3.1a, tmp6.1a)

}



library(purrr); library(tidyverse)
## MAP the comobidity
maptmp1 <- map(names(chronicDisease), possibly(GetGEEdf, NA), "p4_post1")

geedf <- bind_rows(maptmp1)



## CHECK case count in the model's key strata
# all age
# geedf %>% group_by(chronicDiseaseSwitch, TP, lymeDisease) %>% tally
tmp1 <- geedf %>% group_by(chronicDiseaseSwitch, TP, lymeDisease) %>% summarise(
  cases = sum(chronicClass != "no"),
  totaln = n()
  )



excelN <- function(tmp1) {
  tmp11 <- tmp1 %>% arrange(lymeDisease, desc(TP)) %>% arrange(chronicDiseaseSwitch)
  tmp11 <- tmp11 %>% unite("TP_Lyme", c("TP", "lymeDisease")) %>% select(-totaln)
  tmp11 <- tmp11 %>% spread(TP_Lyme, cases)
  tmp11 <- tmp11 %>% slice(match(names(chronicDisease), chronicDiseaseSwitch))
  tmp11 <- tmp11[order(match(tmp11$chronicDiseaseSwitch, names(chronicDisease))),]
  return(tmp11)
}

tmpa <- excelN(tmp1)



# all age <= 35
# geedf %>% group_by(chronicDiseaseSwitch, TP, lymeDisease) %>% tally
tmp2 <- geedf %>% filter(age <= 35) %>%  group_by(chronicDiseaseSwitch, TP, lymeDisease) %>% summarise(
  cases = sum(chronicClass != "no"),
  totaln = n()
)

tmpb <- excelN(tmp2)

# all age
# geedf %>% group_by(chronicDiseaseSwitch, TP, lymeDisease) %>% tally
tmp3 <- geedf %>% filter(age >35) %>%  group_by(chronicDiseaseSwitch, TP, lymeDisease) %>% summarise(
  cases = sum(chronicClass != "no"),
  totaln = n()
)

tmpc <- excelN(tmp3)


## PREPARE geedf variable structure
geedf$MemberGender <- as.factor(geedf$MemberGender)

geedf$chronicClass <- as.factor(geedf$chronicClass)

geedf$TP <- as.factor(geedf$TP)

geedf$lymeDisease <- as.factor(geedf$lymeDisease)

str(geedf)

geedf <- geedf %>% mutate(outcome = if_else(chronicClass == "no", 0, 1))

str(geedf)

# summary(geedf); sex has na
# summarytools::dfSummary(geedf) %>% summarytools::view()

geedf <- geedf[complete.cases(geedf), ] # removed 6 row of NA with sex for each comorbidity

geedf$TP <- factor(geedf$TP, levels=c("pre", "post")) #first is the ref lv
levels(geedf$TP)

contrasts(geedf$TP) # TP: 0 = pre, 1 = post
contrasts(geedf$lymeDisease) # lymedisease: 0 = lyme, 1 = ptlds




## RUN GEE glm 
library(geepack); library(broom); library(tidyverse)


## **********************
## all ages
RunGeeGlm <- function(Rheumatoid_Arthritis, geedf) {
  tmp1 <- geedf %>% filter(chronicDiseaseSwitch == Rheumatoid_Arthritis)
  tmp2 <- geeglm(outcome ~ TP*lymeDisease + age + MemberGender, id = MemberId, family = binomial, data = tmp1, corstr = "exchangeable") %>% tidy
  tmp2$chronicDisease <- Rheumatoid_Arthritis
  return(tmp2)
}

RunGeeGlmOR <- function(Rheumatoid_Arthritis, geedf) {
  tmp1 <- geedf %>% filter(chronicDiseaseSwitch == Rheumatoid_Arthritis)
  tmp2 <- geeglm(outcome ~ TP*lymeDisease + age + MemberGender, id = MemberId, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- Rheumatoid_Arthritis
  return(tmp3)
}


# Use OR function
maptmp2 <- map(names(chronicDisease), possibly(RunGeeGlmOR, NA), geedf)

## Extract results to paper table 2. easy way
geeEstdf <- bind_rows(maptmp2)
geeEstdf$fdr <- p.adjust(geeEstdf$p.value, method = 'fdr')



# ## START ****************************************************************************************
## unadjusted 082520
RunGeeGlmORUnadj082520 <- function(Rheumatoid_Arthritis, geedf) {
  tmp1 <- geedf %>% filter(chronicDiseaseSwitch == Rheumatoid_Arthritis)
  tmp2 <- geeglm(outcome ~ TP*lymeDisease, id = MemberId, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- Rheumatoid_Arthritis
  return(tmp3)
}


# Use OR function
maptmp2082520 <- map(names(chronicDisease), possibly(RunGeeGlmORUnadj082520, NA), geedf)


## Extract results to paper table 2. easy way
geeEstdf082520 <- bind_rows(maptmp2082520) 
geeEstdf082520$fdr <- p.adjust(geeEstdf082520$p.value, method = 'fdr')

# ## END ****************************************************************************************





## **********************
## age <= 35

RunGeeGlm35 <- function(Rheumatoid_Arthritis, geedf) {
  tmp1 <- geedf %>% filter(chronicDiseaseSwitch == Rheumatoid_Arthritis, age <= 35)
  tmp2 <- geeglm(outcome ~ TP*lymeDisease + age + MemberGender, id = MemberId, family = binomial, data = tmp1, corstr = "exchangeable") %>% tidy
  tmp2$chronicDisease <- Rheumatoid_Arthritis
  return(tmp2)
}

RunGeeGlm35OR <- function(Rheumatoid_Arthritis, geedf) {
  tmp1 <- geedf %>% filter(chronicDiseaseSwitch == Rheumatoid_Arthritis, age <= 35)
  tmp2 <- geeglm(outcome ~ TP*lymeDisease + age + MemberGender, id = MemberId, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- Rheumatoid_Arthritis
  return(tmp3)
}


# Use OR
maptmp3 <- map(names(chronicDisease), possibly(RunGeeGlm35OR, NA), geedf)
geeEstdf3 <- bind_rows(maptmp3)
geeEstdf3$fdr <- p.adjust(geeEstdf3$p.value, method = 'fdr')





# ## START ****************************************************************************************
## unadjusted 082520
RunGeeGlm35ORUnadj082520 <- function(Rheumatoid_Arthritis, geedf) {
  tmp1 <- geedf %>% filter(chronicDiseaseSwitch == Rheumatoid_Arthritis, age <= 35)
  tmp2 <- geeglm(outcome ~ TP*lymeDisease, id = MemberId, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- Rheumatoid_Arthritis
  return(tmp3)
}


# Use OR
maptmp3082520 <- map(names(chronicDisease), possibly(RunGeeGlm35ORUnadj082520, NA), geedf)
geeEstdf3082520 <- bind_rows(maptmp3082520)
geeEstdf3082520$fdr <- p.adjust(geeEstdf3082520$p.value, method = 'fdr')

# ## END ****************************************************************************************




## **********************
## age >35

RunGeeGlm100 <- function(Rheumatoid_Arthritis, geedf) {
  tmp1 <- geedf %>% filter(chronicDiseaseSwitch == Rheumatoid_Arthritis, age > 35)
  tmp2 <- geeglm(outcome ~ TP*lymeDisease + age + MemberGender, id = MemberId, family = binomial, data = tmp1, corstr = "exchangeable") %>% tidy
  tmp2$chronicDisease <- Rheumatoid_Arthritis
  return(tmp2)
}


RunGeeGlm100OR <- function(Rheumatoid_Arthritis, geedf) {
  tmp1 <- geedf %>% filter(chronicDiseaseSwitch == Rheumatoid_Arthritis, age > 35)
  tmp2 <- geeglm(outcome ~ TP*lymeDisease + age + MemberGender, id = MemberId, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- Rheumatoid_Arthritis
  return(tmp3)
}



# Use OR
maptmp4 <- map(names(chronicDisease), possibly(RunGeeGlm100OR, NA), geedf)
geeEstdf4 <- bind_rows(maptmp4)
geeEstdf4$fdr <- p.adjust(geeEstdf4$p.value, method = 'fdr')




# ## START ****************************************************************************************
## unadjusted 082520

RunGeeGlm100ORUnadj082520 <- function(Rheumatoid_Arthritis, geedf) {
  tmp1 <- geedf %>% filter(chronicDiseaseSwitch == Rheumatoid_Arthritis, age > 35)
  tmp2 <- geeglm(outcome ~ TP*lymeDisease, id = MemberId, family = binomial, data = tmp1, corstr = "exchangeable")
  tmp3 <- tidy(tmp2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE)
  tmp3$chronicDisease <- Rheumatoid_Arthritis
  return(tmp3)
}



# Use OR
maptmp4082520 <- map(names(chronicDisease), possibly(RunGeeGlm100ORUnadj082520, NA), geedf)
geeEstdf4082520 <- bind_rows(maptmp4082520)
geeEstdf4082520$fdr <- p.adjust(geeEstdf4082520$p.value, method = 'fdr')


# ## END ****************************************************************************************


