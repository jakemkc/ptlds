## Nov 27 2018
## Goal: Lyme, clyme, and PTLDS data cleaenup

rm(list=ls()) # clear workspace;  # ls() # list objects in the workspace
# cmd-shit-f10: restart R. reset loaded settings & library etc
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000, and rstudio set at 5000
options(warn=1) # default = 0. To check loop warnings
# quartz(height=6, width=8)
# dev.off() # reset par
# dev.new() # new plot window
# getwd()


# Load data
lyme <- readRDS("/Volumes/O2_transfer/data/SQL/lyme112518.rds")  # lyme
clyme <- readRDS("/Volumes/O2_transfer/data/SQL/clyme112518.rds")  # clyme
ptlds <- readRDS("/Volumes/O2_transfer/data/SQL/ptlds112518.rds")  # ptlds





# ******** -----
# A. Exploration & Cleanup -----------------------------------------------------------------

library(tidyverse); library(lubridate)


# .\ lyme ----
str(lyme)

# 1. def class
lyme$HmsAetnaLineId <- as.character(lyme$HmsAetnaLineId)
lyme$DateServiceStarted <- ymd(lyme$DateServiceStarted)
lyme$DateServiceStopped <- ymd(lyme$DateServiceStopped)

### univariate summary
skimr::skim(lyme)

summarytools::dfSummary(lyme) %>% summarytools::view()


# 2. check
hist(as.numeric(lyme$HmsAetnaLineId)) # 0 bin
summary(as.numeric(lyme$HmsAetnaLineId)) # no zero, no NA

hist(as.numeric(lyme$MemberId)) # 0 bin
summary(as.numeric(lyme$MemberId)) # OK

hist(as.numeric(lyme$DateServiceStarted)) 
summary(as.numeric(lyme$DateServiceStarted))

hist(as.numeric(lyme$DateServiceStopped)) 
summary(as.numeric(lyme$DateServiceStopped))

hist(lyme$MemberBirthYear) 
summary(lyme$MemberBirthYear) 

table(lyme$MemberGender, useNA = "always") 
lyme[lyme$MemberGender == "U", "MemberGender"] <- NA 

table(lyme$MemberRelationshiptoEmployee, useNA = "always")

hist(as.numeric(lyme$EmployeeZipCode))
summary(as.numeric(lyme$EmployeeZipCode)) 
table(is.na(lyme$EmployeeZipCode)) 

hist(as.numeric(lyme$SubscriberId)) 
summary(as.numeric(lyme$SubscriberId))
table(is.na(lyme$SubscriberId)) 



# .\ clyme ----
str(clyme)

# 1. def class
clyme$HmsAetnaLineId <- as.character(clyme$HmsAetnaLineId)
clyme$DateServiceStarted <- ymd(clyme$DateServiceStarted)
clyme$DateServiceStopped <- ymd(clyme$DateServiceStopped)


### univariate summary
skimr::skim(clyme)

summarytools::dfSummary(clyme) %>% summarytools::view()

# 2. check
## numeric variables
table(is.na(clyme$MemberBirthYear), useNA = "always")
summary(clyme$MemberBirthYear)

table(is.na(clyme$EmployeeZipCode), useNA = "always")
summary(as.numeric(clyme$EmployeeZipCode))

## character varaibles

sapply(clyme, function(x) summary(as.numeric(x))) 

sapply(clyme, function(x) 
    if(!is.Date(x)) 
        x == ""
) %>% sapply(., function(x) table(x))  


sapply(clyme, function(x) 
    if(!is.Date(x))
        x == " "
) %>% sapply(., function(x) table(x)) 

# table of factor vars
table(clyme$MemberGender, useNA = "always") 
clyme[clyme$MemberGender == "U", "MemberGender"] <- NA


table(clyme$MemberRelationshiptoEmployee, useNA = "always")




# .\ ptlds ----
str(ptlds)

# 1. def class
ptlds$HmsAetnaLineId <- as.character(ptlds$HmsAetnaLineId)
ptlds$DateServiceStarted <- ymd(ptlds$DateServiceStarted)
ptlds$DateServiceStopped <- ymd(ptlds$DateServiceStopped)

### univariate summary
skimr::skim(ptlds)

summarytools::dfSummary(ptlds) %>% summarytools::view()

# 2. check
hist(as.numeric(ptlds$HmsAetnaLineId)) 
summary(as.numeric(ptlds$HmsAetnaLineId)) 

hist(as.numeric(ptlds$MemberId)) 
summary(as.numeric(ptlds$MemberId)) 

hist(as.numeric(ptlds$DateServiceStarted)) 
summary(as.numeric(ptlds$DateServiceStarted))

hist(as.numeric(ptlds$DateServiceStopped)) 
summary(as.numeric(ptlds$DateServiceStopped))

hist(ptlds$MemberBirthYear) 
summary(ptlds$MemberBirthYear) 

table(ptlds$MemberGender, useNA = "always") 
ptlds[ptlds$MemberGender == "U", "MemberGender"] <- NA

table(ptlds$MemberRelationshiptoEmployee, useNA = "always") 

hist(as.numeric(ptlds$EmployeeZipCode))
summary(as.numeric(ptlds$EmployeeZipCode)) 
table(is.na(ptlds$EmployeeZipCode))

hist(as.numeric(ptlds$SubscriberId)) 
summary(as.numeric(ptlds$SubscriberId))
table(is.na(ptlds$SubscriberId))






# .\ a1-create-age-var ----

# lyme
summary(lyme$MemberBirthYear) # 1912 - 2012
lyme$age <- 2017 - lyme$MemberBirthYear

summary(lyme$age) # 5 - 105
length(lyme[which(lyme$age > 100), "age"]) # 15
lyme[which(lyme$age > 100), "age"] <- 100

# chronic lyme
summary(clyme$MemberBirthYear) # 1915 - 2011
clyme$age <- 2017 - clyme$MemberBirthYear

summary(clyme$age) # 6 - 102
length(clyme[which(clyme$age > 100), "age"]) # 2
clyme[which(clyme$age > 100), "age"] <- 100

# ptlds
summary(ptlds$MemberBirthYear) # 1918 - 2011
ptlds$age <- 2017 - ptlds$MemberBirthYear

summary(ptlds$age) # 6 - 99
length(ptlds[which(ptlds$age > 100), "age"]) # 0
ptlds[which(ptlds$age > 100), "age"] <- 100


# ******** -----
## A.1. Saving Rdata ----

outdirectory <- "data"
outfilename <- "desc_cleanup_lyme_clyme_ptlds_NOT_exclusive.Rdata"
save(file=file.path(outdirectory,outfilename), 
     lyme, clyme, ptlds)


# .\ a2-unique-n-table ----

## unique lyme
tmp1 <- setdiff(lyme$HmsAetnaLineId, clyme$HmsAetnaLineId)
tmp1 %>% length # 44 420
lyme <- anti_join(lyme, clyme, by = "HmsAetnaLineId")

## unique clyme
tmp2 <- setdiff(clyme$HmsAetnaLineId, ptlds$HmsAetnaLineId)
tmp2 %>% length # 2685
clyme <- anti_join(clyme, ptlds, by = "HmsAetnaLineId")


# ******** -----
## A.1. Saving Rdata ----

outdirectory <- "data"
outfilename <- "desc_cleanup_lyme_clyme_ptlds_exclusive.Rdata"
save(file=file.path(outdirectory,outfilename), 
     lyme, clyme, ptlds)
