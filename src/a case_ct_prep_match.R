## Oct 21 2019
## Goal: Lyme and control data prep and match

rm(list=ls()) # clear workspace
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000, and rstudio set at 5000
options(warn=1) # default = 0


# Load data
lyme <- readRDS("./data/SQL/lyme112518.rds")
lymeCtIcd <- readRDS("./data/SQL/lymeCtIcd101819.rds")
ctSQL <- readRDS("./data/SQL/control101819.rds")
ctSQLCtIcd <- readRDS("./data/SQL/randomCtIcd101819.rds")



# ******** -----
# A. LYME -----------------------------------------------------------------

library(tidyverse); library(lubridate)

str(lyme)

# 1. def class
lyme$HmsAetnaLineId <- as.character(lyme$HmsAetnaLineId)
lyme$DateServiceStarted <- ymd(lyme$DateServiceStarted)
lyme$DateServiceStopped <- ymd(lyme$DateServiceStopped)



### univariate summary
summarytools::dfSummary(lyme) %>% summarytools::view()


# 2. check
hist(as.numeric(lyme$MemberId)) 
summary(as.numeric(lyme$MemberId)) # OK

hist(as.numeric(lyme$DateServiceStarted)) 
summary(as.numeric(lyme$DateServiceStarted))

hist(as.numeric(lyme$DateServiceStopped)) 
summary(as.numeric(lyme$DateServiceStopped))

hist(lyme$MemberBirthYear) 
summary(lyme$MemberBirthYear) 

table(lyme$MemberGender, useNA = "always") 
lyme[which(lyme$MemberGender == "U"), "MemberGender"] <- NA


summary(as.numeric(lyme$EmployeeZipCode))
table(is.na(lyme$EmployeeZipCode)) 
lyme[which(lyme$EmployeeZipCode == "U"), "EmployeeZipCode"] <- NA 
lyme[which(lyme$EmployeeZipCode == "00000"), "EmployeeZipCode"] <- NA 



# create-age-var ----


# lyme
summary(lyme$MemberBirthYear) 
lyme$age <- 2017 - lyme$MemberBirthYear

summary(lyme$age)
length(lyme[which(lyme$age > 100), "age"]) 
lyme[which(lyme$age > 100), "age"] <- 100


# join-CtIcd ----
lyme <- lyme %>% left_join(lymeCtIcd, by = c("MemberId" = "MemberId"))



# ******** -----
# B. SQL-CONTROl -----------------------------------------------------------------



# 1. def class
ctSQL$HmsAetnaLineId <- as.character(ctSQL$HmsAetnaLineId)
ctSQL$DateServiceStarted <- ymd(ctSQL$DateServiceStarted)



# 2. check
hist(as.numeric(ctSQL$MemberId)) 
summary(as.numeric(ctSQL$MemberId))

hist(as.numeric(ctSQL$DateServiceStarted)) 
summary(as.numeric(ctSQL$DateServiceStarted))

hist(ctSQL$MemberBirthYear) 
summary(ctSQL$MemberBirthYear) 

table(ctSQL$MemberGender, useNA = "always")
ctSQL[which(ctSQL$MemberGender == "U"), "MemberGender"] <- NA 


summary(as.numeric(ctSQL$EmployeeZipCode)) 
table(is.na(ctSQL$EmployeeZipCode))
ctSQL[which(ctSQL$EmployeeZipCode == "U"), "EmployeeZipCode"] <- NA
ctSQL[which(ctSQL$EmployeeZipCode == "00000"), "EmployeeZipCode"] <- NA



# create-age-var ----

# ctSQL
summary(ctSQL$MemberBirthYear)
ctSQL$age <- 2017 - ctSQL$MemberBirthYear

summary(ctSQL$age) 
length(ctSQL[which(ctSQL$age > 100), "age"])
ctSQL[which(ctSQL$age > 100), "age"] <- 100


# join-CtIcd ----
ctSQL <- ctSQL %>% left_join(ctSQLCtIcd, by = c("MemberId" = "MemberId"))


# create-state-var ----
zipCDConv <- read_csv("data/zip_to_larger_unit_050420/ZIP_CD_032012.csv", 
                      col_types = cols(CD = col_character(), ZIP = col_character()))

zipCDConv <- zipCDConv %>% mutate(stateID = str_sub(CD, 1, 2)) 
zipCDConv <- zipCDConv %>% group_by(ZIP) %>% slice(which.max(TOT_RATIO)) %>% select(ZIP, stateID) 

lyme <- lyme %>% left_join(zipCDConv, by = c("EmployeeZipCode" = "ZIP"))

ctSQL <- ctSQL %>% left_join(zipCDConv, by = c("EmployeeZipCode" = "ZIP"))



# ******** -----
# C. CREATE case-control -----------------------------------------------------------------


lyme <- lyme %>% select(MemberId, DateServiceStarted, MemberBirthYear, MemberGender, EmployeeZipCode, age, uniqueCtIcd, stateID)
lyme$treat <- 1
lyme$caseId <- "lyme"

ct <- ctSQL %>% select(MemberId, DateServiceStarted, MemberBirthYear, MemberGender, EmployeeZipCode, age, uniqueCtIcd, stateID)
ct$treat <- 0
ct$caseId <- "control"

# REMOVE lyme member in control
tmp1 <- setdiff(ct$MemberId, lyme$MemberId) 
tmp2 <- which(ct$MemberId %in% tmp1)

# real controls
ct <- ct[tmp2, ] 



# ******** -----
## A.1. Saving Rdata ----

outdirectory <- "data"
outfilename <- "case_control_102819.Rdata"
save(file=file.path(outdirectory,outfilename), 
     lyme, ct)

