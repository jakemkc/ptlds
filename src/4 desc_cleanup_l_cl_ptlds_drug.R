## Nov 29 2018
## Goal: Lyme, clyme, ptlds with drug info data clean up

rm(list=ls()) # clear workspace;  # ls() # list objects in the workspace
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000, and rstudio set at 5000
options(warn=1) # default = 0. To check loop warnings
# quartz(height=6, width=8)
# dev.off() # reset par

## Load data

# Load data
lymedrug040118 <- readRDS("/Volumes/O2_transfer/data/SQL/lymealldrug112918.rds")  # lyme
lymeanti040118 <- readRDS("/Volumes/O2_transfer/data/SQL/lymeanti112918.rds")  # lyme

clymedrug040118 <- readRDS("/Volumes/O2_transfer/data/SQL/clymealldrug112918.rds")  # clyme
clymeanti040118 <- readRDS("/Volumes/O2_transfer/data/SQL/clymeanti112918.rds")  # clyme

ptldsdrug040118 <- readRDS("/Volumes/O2_transfer/data/SQL/ptldsalldrug112918.rds")  # ptlds
ptldsanti040118 <- readRDS("/Volumes/O2_transfer/data/SQL/ptldsanti112918.rds")  # ptlds

# ******** -----
# A. Exploration & Cleanup -----------------------------------------------------------------

library(tidyverse); library(lubridate)

## \\ lyme-all-drug ----
str(lymedrug040118)

# def class
lymedrug040118$HmsAetnaLineId <- as.character(lymedrug040118$HmsAetnaLineId)
lymedrug040118$DateServiceStarted <- ymd(lymedrug040118$DateServiceStarted)
lymedrug040118$DateServiceStopped <- ymd(lymedrug040118$DateServiceStopped)
lymedrug040118$d_DispenseDate <- ymd(lymedrug040118$d_DispenseDate)

# check

skimr::skim(lymedrug040118)

summarytools::dfSummary(lymedrug040118) %>% summarytools::view() 
# 
hist(as.numeric(lymedrug040118$HmsAetnaLineId)) 
summary(as.numeric(lymedrug040118$HmsAetnaLineId)) 

hist(as.numeric(lymedrug040118$MemberId)) 
summary(as.numeric(lymedrug040118$MemberId)) # OK

hist(as.numeric(lymedrug040118$DateServiceStarted)) 
summary(as.numeric(lymedrug040118$DateServiceStarted))

hist(as.numeric(lymedrug040118$DateServiceStopped)) 
summary(as.numeric(lymedrug040118$DateServiceStopped))

hist(lymedrug040118$MemberBirthYear) 
summary(lymedrug040118$MemberBirthYear) 

table(lymedrug040118$MemberGender, useNA = "always") 
lymedrug040118[lymedrug040118$MemberGender == "U", "MemberGender"] <- NA



as.data.frame(table(lymedrug040118$MemberRelationshiptoEmployee, useNA = "always")) 

hist(as.numeric(lymedrug040118$EmployeeZipCode))
summary(as.numeric(lymedrug040118$EmployeeZipCode))
table(is.na(lymedrug040118$EmployeeZipCode)) 

hist(as.numeric(lymedrug040118$SubscriberId)) # 0 bin?
summary(as.numeric(lymedrug040118$SubscriberId)) # 
table(is.na(lymedrug040118$SubscriberId)) # 


## Drug

summary(as.numeric(lymedrug040118$d_NationalDrug)) 
table(is.na(lymedrug040118$d_NationalDrug)) 

table(is.na(lymedrug040118$d_NdcDesc)) 

table(is.na(lymedrug040118$d_BrandGenInd)) 
table(lymedrug040118$d_BrandGenInd) #

table(is.na(lymedrug040118$d_SingMultiSour)) 
table(lymedrug040118$d_SingMultiSour) 

table(is.na(lymedrug040118$d_NewRefill)) 
table(lymedrug040118$d_NewRefill) 

hist(as.numeric(lymedrug040118$d_Quantity)) 
summary(as.numeric(lymedrug040118$d_Quantity)) 

hist(as.numeric(lymedrug040118$d_DaysSupply)) 
summary(as.numeric(lymedrug040118$d_DaysSupply))

table(is.na(lymedrug040118$d_MaintDrugInd)) 
table(lymedrug040118$d_MaintDrugInd) 


## \\ lyme-3-anti ----
str(lymeanti040118)

# def class
lymeanti040118$HmsAetnaLineId <- as.character(lymeanti040118$HmsAetnaLineId)
lymeanti040118$DateServiceStarted <- ymd(lymeanti040118$DateServiceStarted)
lymeanti040118$DateServiceStopped <- ymd(lymeanti040118$DateServiceStopped)
lymeanti040118$d_DispenseDate <- ymd(lymeanti040118$d_DispenseDate)

# check

skimr::skim(lymedrug040118)

summarytools::dfSummary(lymedrug040118) %>% summarytools::view() 


## comments in antibiotic section of lyme is from original, didn't check, and rely on skim and dfsummary
hist(as.numeric(lymeanti040118$HmsAetnaLineId)) 
summary(as.numeric(lymeanti040118$HmsAetnaLineId)) 

hist(as.numeric(lymeanti040118$MemberId)) 
summary(as.numeric(lymeanti040118$MemberId)) # OK

hist(as.numeric(lymeanti040118$DateServiceStarted)) 
summary(as.numeric(lymeanti040118$DateServiceStarted))

hist(as.numeric(lymeanti040118$DateServiceStopped)) 
summary(as.numeric(lymeanti040118$DateServiceStopped))

hist(lymeanti040118$MemberBirthYear) 
summary(lymeanti040118$MemberBirthYear) 

table(lymeanti040118$MemberGender, useNA = "always") 
lymeanti040118[lymeanti040118$MemberGender == "U", "MemberGender"] <- NA

as.data.frame(table(lymeanti040118$MemberRelationshiptoEmployee, useNA = "always")) 

hist(as.numeric(lymeanti040118$EmployeeZipCode))
summary(as.numeric(lymeanti040118$EmployeeZipCode)) 
table(is.na(lymeanti040118$EmployeeZipCode)) # 7 NA;

hist(as.numeric(lymeanti040118$SubscriberId)) # 0 bin?
summary(as.numeric(lymeanti040118$SubscriberId)) # 46 NA
table(is.na(lymeanti040118$SubscriberId)) # 46 NA


## Drug

summary(as.numeric(lymeanti040118$d_NationalDrug)) 
table(is.na(lymeanti040118$d_NationalDrug)) # 7 NA

table(is.na(lymeanti040118$d_NdcDesc)) 
table(is.na(lymeanti040118$d_BrandGenInd)) # 7 NA;
table(lymeanti040118$d_BrandGenInd) 

table(is.na(lymeanti040118$d_SingMultiSour)) # 7 NA;
table(lymeanti040118$d_SingMultiSour) # 2 cateogries

table(is.na(lymeanti040118$d_NewRefill)) # 7 NA;
table(lymeanti040118$d_NewRefill) 
hist(as.numeric(lymeanti040118$d_Quantity)) # 0 - 2000
summary(as.numeric(lymeanti040118$d_Quantity)) # 1 - 2400, NA 7

hist(as.numeric(lymeanti040118$d_DaysSupply)) # 0-80
summary(as.numeric(lymeanti040118$d_DaysSupply)) # 1 - 90, NA 7

table(is.na(lymeanti040118$d_MaintDrugInd)) 
table(lymeanti040118$d_MaintDrugInd) 



## \\ clyme-all-drug ----
str(clymedrug040118)

# def class
clymedrug040118$HmsAetnaLineId <- as.character(clymedrug040118$HmsAetnaLineId)
clymedrug040118$DateServiceStarted <- ymd(clymedrug040118$DateServiceStarted)
clymedrug040118$DateServiceStopped <- ymd(clymedrug040118$DateServiceStopped)
clymedrug040118$d_DispenseDate <- ymd(clymedrug040118$d_DispenseDate)


# check

skimr::skim(clymedrug040118)

summarytools::dfSummary(clymedrug040118) %>% summarytools::view() 

## numeric variables
table(is.na(clymedrug040118$MemberBirthYear), useNA = "always") # No NA
summary(clymedrug040118$MemberBirthYear)

table(is.na(clymedrug040118$EmployeeZipCode), useNA = "always") # no NA
summary(as.numeric(clymedrug040118$EmployeeZipCode)) 


## character varaibles
# NA
sapply(clymedrug040118, function(x) summary(as.numeric(x)))

# "" empty cell
sapply(clymedrug040118, function(x) 
    if(!is.Date(x)) 
        x == ""
) %>% sapply(., function(x) table(x))  # no, all false

# " " space cell
sapply(clymedrug040118, function(x) 
    if(!is.Date(x))
        x == " "
) %>% sapply(., function(x) table(x)) # no, all false

# table of factor vars
table(clymedrug040118$MemberGender, useNA = "always") # U =1
clymedrug040118[clymedrug040118$MemberGender == "U", "MemberGender"] <- NA


table(clymedrug040118$MemberRelationshiptoEmployee, useNA = "always") # NA =1



## Drug


summary(as.numeric(clymedrug040118$d_NationalDrug)) 
table(is.na(clymedrug040118$d_NationalDrug)) # 6655 NA

table(is.na(clymedrug040118$d_NdcDesc)) 
table(is.na(clymedrug040118$d_BrandGenInd)) # 6655 NA;
table(clymedrug040118$d_BrandGenInd) 

table(is.na(clymedrug040118$d_SingMultiSour)) # 6655 NA;
table(clymedrug040118$d_SingMultiSour) # 2 cateogries

table(is.na(clymedrug040118$d_NewRefill)) # 6655 NA;
table(clymedrug040118$d_NewRefill) # <10 cateogries

hist(as.numeric(clymedrug040118$d_Quantity)) # 0 - 800
summary(as.numeric(clymedrug040118$d_Quantity)) # 0.24 - 900, NA 6655

hist(as.numeric(clymedrug040118$d_DaysSupply)) # 0 - 100
summary(as.numeric(clymedrug040118$d_DaysSupply)) # 1 - 90, NA 6655

table(is.na(clymedrug040118$d_MaintDrugInd)) # 10857 NA; This is not likely a standardized field to record in db
table(clymedrug040118$d_MaintDrugInd) # 1 category


## \\ clym-3-anti ----
str(clymeanti040118)


# def class
clymeanti040118$HmsAetnaLineId <- as.character(clymeanti040118$HmsAetnaLineId)
clymeanti040118$DateServiceStarted <- ymd(clymeanti040118$DateServiceStarted)
clymeanti040118$DateServiceStopped <- ymd(clymeanti040118$DateServiceStopped)
clymeanti040118$d_DispenseDate <- ymd(clymeanti040118$d_DispenseDate)


# check
skimr::skim(clymedrug040118)

summarytools::dfSummary(clymedrug040118) %>% summarytools::view() 

## numeric variables
table(is.na(clymeanti040118$MemberBirthYear), useNA = "always") # No NA
summary(clymeanti040118$MemberBirthYear)

table(is.na(clymeanti040118$EmployeeZipCode), useNA = "always") # no NA
summary(as.numeric(clymeanti040118$EmployeeZipCode)) 


## character varaibles
# NA
sapply(clymeanti040118, function(x) summary(as.numeric(x)))

# "" empty cell
sapply(clymeanti040118, function(x) 
    if(!is.Date(x)) 
        x == ""
) %>% sapply(., function(x) table(x))  # no, all false

# " " space cell
sapply(clymeanti040118, function(x) 
    if(!is.Date(x))
        x == " "
) %>% sapply(., function(x) table(x)) # no, all false

# table of factor vars
table(clymeanti040118$MemberGender, useNA = "always") # U =1
clymeanti040118[clymeanti040118$MemberGender == "U", "MemberGender"] <- NA



table(clymeanti040118$MemberRelationshiptoEmployee, useNA = "always") # 



## Drug

summary(as.numeric(clymeanti040118$d_NationalDrug)) 
table(is.na(clymeanti040118$d_NationalDrug)) # 0 NA

table(is.na(clymeanti040118$d_NdcDesc)) 

table(is.na(clymeanti040118$d_BrandGenInd)) # 0 NA;
table(clymeanti040118$d_BrandGenInd) 

table(is.na(clymeanti040118$d_SingMultiSour)) # 0 NA;
table(clymeanti040118$d_SingMultiSour) # 2 cateogries

table(is.na(clymeanti040118$d_NewRefill)) # 0 NA;
table(clymeanti040118$d_NewRefill) # <10 cateogries

hist(as.numeric(clymeanti040118$d_Quantity)) # 0 - 700
summary(as.numeric(clymeanti040118$d_Quantity)) # 0.24 - 750, NA 0

hist(as.numeric(clymeanti040118$d_DaysSupply)) # 0 - 100
summary(as.numeric(clymeanti040118$d_DaysSupply)) # 1 - 90, NA 0

table(is.na(clymeanti040118$d_MaintDrugInd)) 
table(clymeanti040118$d_MaintDrugInd) # 1 category




## \\ ptlds-all-drug ----
str(ptldsdrug040118)

# def class
ptldsdrug040118$HmsAetnaLineId <- as.character(ptldsdrug040118$HmsAetnaLineId)
ptldsdrug040118$DateServiceStarted <- ymd(ptldsdrug040118$DateServiceStarted)
ptldsdrug040118$DateServiceStopped <- ymd(ptldsdrug040118$DateServiceStopped)
ptldsdrug040118$d_DispenseDate <- ymd(ptldsdrug040118$d_DispenseDate)

# check

skimr::skim(ptldsdrug040118)

summarytools::dfSummary(ptldsdrug040118) %>% summarytools::view() # about half have drug info

table(ptldsdrug040118$MemberGender, useNA = "always") # U group?
ptldsdrug040118[ptldsdrug040118$MemberGender == "U", "MemberGender"] <- NA



## \\ ptlds-3-anti ----
str(ptldsanti040118)

# def class
ptldsanti040118$HmsAetnaLineId <- as.character(ptldsanti040118$HmsAetnaLineId)
ptldsanti040118$DateServiceStarted <- ymd(ptldsanti040118$DateServiceStarted)
ptldsanti040118$DateServiceStopped <- ymd(ptldsanti040118$DateServiceStopped)
ptldsanti040118$d_DispenseDate <- ymd(ptldsanti040118$d_DispenseDate)

# check

skimr::skim(lymedrug040118)

summarytools::dfSummary(lymedrug040118) %>% summarytools::view() # about half have drug info


table(ptldsanti040118$MemberGender, useNA = "always") # U group?
ptldsanti040118[ptldsanti040118$MemberGender == "U", "MemberGender"] <- NA





# ******** -----
## \\ A1. Create age var ----
# lymedrug
summary(lymedrug040118$MemberBirthYear) 
lymedrug040118$age <- 2017 - lymedrug040118$MemberBirthYear

summary(lymedrug040118$age) # 
length(lymedrug040118[which(lymedrug040118$age > 100), "age"]) # 
lymedrug040118[which(lymedrug040118$age > 100), "age"] <- 100

# lymeanti
summary(lymeanti040118$MemberBirthYear) # 
lymeanti040118$age <- 2017 - lymeanti040118$MemberBirthYear

summary(lymeanti040118$age) # 
length(lymeanti040118[which(lymeanti040118$age > 100), "age"]) # 4
lymeanti040118[which(lymeanti040118$age > 100), "age"] <- 100


# clymedrug
summary(clymedrug040118$MemberBirthYear) # 1916 - 2011
clymedrug040118$age <- 2017 - clymedrug040118$MemberBirthYear

summary(clymedrug040118$age) # 6 - 101
length(clymedrug040118[which(clymedrug040118$age > 100), "age"]) # 3
clymedrug040118[which(clymedrug040118$age > 100), "age"] <- 100


# clymeanti
summary(clymeanti040118$MemberBirthYear) # 
clymeanti040118$age <- 2017 - clymeanti040118$MemberBirthYear

summary(clymeanti040118$age) # 
length(clymeanti040118[which(clymeanti040118$age > 100), "age"]) # 
clymeanti040118[which(clymeanti040118$age > 100), "age"] <- 100


# ptldsdrug
summary(ptldsdrug040118$MemberBirthYear) # 
ptldsdrug040118$age <- 2017 - ptldsdrug040118$MemberBirthYear

summary(ptldsdrug040118$age) # 
length(ptldsdrug040118[which(ptldsdrug040118$age > 100), "age"]) # 
ptldsdrug040118[which(ptldsdrug040118$age > 100), "age"] <- 100

# ptldsanti
summary(ptldsanti040118$MemberBirthYear) # 
ptldsanti040118$age <- 2017 - ptldsanti040118$MemberBirthYear

summary(ptldsanti040118$age) # 2 - 106
length(ptldsanti040118[which(ptldsanti040118$age > 100), "age"]) # 6
ptldsanti040118[which(ptldsanti040118$age > 100), "age"] <- 100


## how many ppl (non exlucsive)
#' they may have antibiotics from other provider, or not getting the antitobic within the extract date range

# all drug (this has no use because it was a left join with lyme patient, but a check)
unique(lymedrug040118$MemberId) %>% length
unique(clymedrug040118$MemberId) %>% length
unique(ptldsdrug040118$MemberId) %>% length


# how many patients have no drug info?
lymedrug040118$MemberId[is.na(lymedrug040118$d_NationalDrug)] %>% unique %>% length #36610
clymedrug040118$MemberId[is.na(clymedrug040118$d_NationalDrug)] %>% unique %>% length #3243
ptldsdrug040118$MemberId[is.na(ptldsdrug040118$d_NationalDrug)] %>% unique %>% length #1182

# how many pateitns have drug info
lymedrug040118$MemberId[!is.na(lymedrug040118$d_NationalDrug)] %>% unique %>% length #11986
clymedrug040118$MemberId[!is.na(clymedrug040118$d_NationalDrug)] %>% unique %>% length #933
ptldsdrug040118$MemberId[!is.na(ptldsdrug040118$d_NationalDrug)] %>% unique %>% length #336

# how many patient have antibiotlic info
lymeanti040118$MemberId %>% unique %>% length
clymeanti040118$MemberId %>% unique %>% length
ptldsanti040118$MemberId %>% unique %>% length




# ******** -----
# \\ A2.  ---------------------------------------------------------------------------


# ******** -----
## A2. Saving Rdata ----

outdirectory <- "data"
outfilename <- "desc_cleanup_l_cl_ptlds_drug_NOT_exclusive.Rdata"
save(file=file.path(outdirectory,outfilename), 
     lymedrug040118, lymeanti040118, clymedrug040118, clymeanti040118, ptldsdrug040118, ptldsanti040118)





# ******** -----
## PROCESS the tables with DRUG info:
## unique lyme
tmp1 <- setdiff(lymedrug040118$HmsAetnaLineId, clymedrug040118$HmsAetnaLineId)
tmp1 %>% length # 44 420
tmp2 <- anti_join(lymedrug040118, clymedrug040118, by = "HmsAetnaLineId")

## unique clyme
tmp3 <- setdiff(clymedrug040118$HmsAetnaLineId, ptldsdrug040118$HmsAetnaLineId)
tmp3 %>% length # 2685
tmp4 <- anti_join(clymedrug040118, ptldsdrug040118, by = "HmsAetnaLineId")

## PTLD is already unique to PLTDS

# all drug
unique(tmp2$MemberId) %>% length
unique(tmp4$MemberId) %>% length

lymedrug040118 <- tmp2
clymedrug040118 <- tmp4


# Lyme
lymedrug040118 %>% nrow # 68141
lymedrug040118 %>% distinct %>% nrow # 50004; 
lymedrug040118$MemberId %>% unique %>% length # 44420
lymedrug040118$MemberId %>% is.na %>% table 
lymedrug040118$d_NdcDesc %>% table(useNA = "always") ## 33367 NA
lymedrug040118 %>% distinct %>% filter(!is.na(d_NdcDesc)) %>% .$MemberId %>% unique %>% length #11053



# cLyme
clymedrug040118 %>% nrow # 3953
clymedrug040118 %>% distinct %>% nrow 
clymedrug040118$MemberId %>% unique %>% length # 2658
clymedrug040118$MemberId %>% is.na %>% table # no NA in memberID
clymedrug040118$d_NdcDesc %>% table(useNA = "always") ## 2061 NA
clymedrug040118 %>% distinct %>% filter(!is.na(d_NdcDesc)) %>% .$MemberId %>% unique %>% length #597




# ptlds
ptldsdrug040118 %>% nrow # 2224
ptldsdrug040118 %>% distinct %>% nrow 
ptldsdrug040118$MemberId %>% unique %>% length # 1518
ptldsdrug040118$MemberId %>% is.na %>% table # no NA in memberID
ptldsdrug040118$d_NdcDesc %>% table(useNA = "always") ## 1182 NA
ptldsdrug040118 %>% distinct %>% filter(!is.na(d_NdcDesc)) %>% .$MemberId %>% unique %>% length #336





# ******** -----
## PROCESS the tables with ANTIbiotic info:
## unique lyme
tmp1 <- setdiff(lymeanti040118$HmsAetnaLineId, clymeanti040118$HmsAetnaLineId)
tmp1 %>% length # 7503
tmp2 <- anti_join(lymeanti040118, clymeanti040118, by = "HmsAetnaLineId")

## unique clyme
tmp3 <- setdiff(clymeanti040118$HmsAetnaLineId, ptldsanti040118$HmsAetnaLineId)
tmp3 %>% length # 275
tmp4 <- anti_join(clymeanti040118, ptldsanti040118, by = "HmsAetnaLineId")

## PTLD is already unique to PLTDS

# all antibiotic
unique(tmp2$MemberId) %>% length
unique(tmp4$MemberId) %>% length

lymeanti040118 <- tmp2
clymeanti040118 <- tmp4




## let's count patient in ANTIbiotic tables (updated 041819)
# Lyme
lymeanti040118 %>% nrow # 17065
lymeanti040118 %>% distinct %>% nrow 
lymeanti040118$MemberId %>% unique %>% length # 7503
lymeanti040118$MemberId %>% is.na %>% table # no NA in memberID
lymeanti040118$d_NdcDesc %>% table(useNA = "always")
lymeanti040118 %>% distinct %>% filter(!is.na(d_NdcDesc)) %>% .$MemberId %>% unique %>% length #7503



# cLyme
clymeanti040118 %>% nrow # 597
clymeanti040118 %>% distinct %>% nrow # 287; in SQL lyme memberID is unique, likely drug table got redundent by other line no.
clymeanti040118$MemberId %>% unique %>% length # 275
clymeanti040118$MemberId %>% is.na %>% table # no NA in memberID
clymeanti040118$d_NdcDesc %>% table(useNA = "always") ## 0 NA; in SQL, I extract by antibiotic instead of just join drug's memberID to LymeS, so no NA
clymeanti040118 %>% distinct %>% filter(!is.na(d_NdcDesc)) %>% .$MemberId %>% unique %>% length #275




# ptlds
ptldsanti040118 %>% nrow # 469
ptldsanti040118 %>% distinct %>% nrow # 215; in SQL lyme memberID is unique, likely drug table got redundent by other line no.
ptldsanti040118$MemberId %>% unique %>% length # 209
ptldsanti040118$MemberId %>% is.na %>% table # no NA in memberID
ptldsanti040118$d_NdcDesc %>% table(useNA = "always") ## 0 NA; in SQL, I extract by antibiotic instead of just join drug's memberID to LymeS, so no NA
ptldsanti040118 %>% distinct %>% filter(!is.na(d_NdcDesc)) %>% .$MemberId %>% unique %>% length #209


ptldsdrug040118$MemberId %>% unique %>% length # 1518
clymedrug040118$MemberId %>% unique %>% length # 2658
lymedrug040118$MemberId %>% unique %>% length # 44420

ptldsanti040118$MemberId %>% unique %>% length # 209
clymeanti040118$MemberId %>% unique %>% length # 275
lymeanti040118$MemberId %>% unique %>% length # 7503


# ******** -----
## A2. Saving Rdata ----

outdirectory <- "data"
outfilename <- "desc_cleanup_l_cl_ptlds_drug_exclusive.Rdata"
save(file=file.path(outdirectory,outfilename), 
     lymedrug040118, lymeanti040118, clymedrug040118, clymeanti040118, ptldsdrug040118, ptldsanti040118)


