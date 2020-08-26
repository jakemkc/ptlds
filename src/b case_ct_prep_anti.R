## Nov 4 2019
## Goal: Lyme and control data prep with Drug variable

rm(list=ls()) # clear workspace
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000
options(warn=1) # default = 0.


## Load data
load("./data/case_control_102819.Rdata")
lymedrug040118 <- readRDS("./data/SQL/lymealldrug112918.rds")  # lyme 
lymeanti040118 <- readRDS("./data/SQL/lymeanti112918.rds")  # lyme


# SQL drug and anti df
ctdrug101719 <- readRDS("./data/SQL/controlDrug101719.rds")  # ct
ctanti101719 <- readRDS("./data/SQL/controlAnti101719.rds")  # ct


# ******** -----
# A. CHECK var -----------------------------------------------------------------
library(tidyverse); library(lubridate)


## Lyme
# how many patients have no drug info?
lymedrug040118$MemberId[is.na(lymedrug040118$d_NationalDrug)] %>% unique %>% length #36610
# how many pateitns have drug info
lymedrug040118$MemberId[!is.na(lymedrug040118$d_NationalDrug)] %>% unique %>% length #11986
# how many patient have antibiotlic info
lymeanti040118$MemberId %>% unique %>% length #7987


## control
# how many patients have no drug info?
ctdrug101719$MemberId[is.na(ctdrug101719$d_NationalDrug)] %>% unique %>% length #164867
# how many pateitns have drug info
ctdrug101719$MemberId[!is.na(ctdrug101719$d_NationalDrug)] %>% unique %>% length #33148
# how many patient have antibiotlic info
ctanti101719$MemberId %>% unique %>% length #1229


## Create drug category
drugList = list(
  Amoxicillin = c(
    "Amoxicillin", # drug class
    "Augmentin",
    "Amoxil",
    "Biomox",
    "Trimox",
    "Moxatag",
    "Clavamox",
    "Amoxi Drop",
    "Amoxi-tabs"
  ),
  Doxycycline = c(
    "Doxycycline", # drug class
    "Ocudox",
    "Morgidox",
    "Vibramycin",
    "Doryx",
    "Monodox",
    "Periostat",
    "Atridox",
    "Adoxa",
    "Doxy",
    "Oracea",
    "Alodox",
    "Oraxyl",
    "Avidoxy",
    "Acticlate",
    "Mondoxyne",
    "Targadox"
  ),
  Cefuroxime = c(
    "Cefuroxime", # drug class
    "Ceftin", 
    "Zinacef"
  ),
  Ceftriaxone = c(
    "Ceftriaxone" # drug class
  ),
  PenicillinG = c(
    "Penicillin G", # drug class
    "Bactracillin G",
    "Norocillin",
    "Agri-Cillin",
    "Pen-Aqueous",
    "Bicillin L-A",
    "Bicillin",
    "Pfizerpen",
    "PenJect",
    "Bactracillin G Benzathine",
    "Go-dry",
    "Masti-clear",
    "Albadry",
    "Quartermaster",
    "Vetripen",
    "BenzaPen",
    "Combi-Pen-48",
    "Pro-Pen-G"
  )
)



# ******** -----
# A. PREP var -----------------------------------------------------------------

addABVar <- function(lyme, lymedrug040118, lymeanti040118) {
  
    
  ## *************************************
  ## FILTER rows with drug info only
  lymeDrugMemId <- lymedrug040118$MemberId[!is.na(lymedrug040118$d_NationalDrug)] %>% unique
  lyme1 <- lyme %>% filter(MemberId %in% lymeDrugMemId)
  
  
  ## *************************************
  ## REMOVE rows with patient take >1 AB only
  
  lymeanti040118$drugcateg <- NA
  
  for(i in 1:length(drugList)) {
      mtch <- grep(paste(drugList[[i]], collapse = "|"), lymeanti040118$d_NdcDesc, ignore.case = TRUE)
      lymeanti040118$drugcateg[mtch] <- names(drugList)[i]
  }
  
  
  ## Find member taking 2 antibotics in AB df
  lymeAnti1 <- lymeanti040118 %>% select(MemberId, drugcateg) %>%  distinct
  lymeAnti2 <- lymeAnti1 %>% group_by(MemberId) %>% 
      summarise(
          nMember = n()) %>% filter(nMember != "1") 
  
  ## REMOVE rows in DRUG df with patient take >1 AB only
  tmpInd1 <- which(lyme1$MemberId %in% lymeAnti2$MemberId)
  
  lyme2 <- lyme1[-tmpInd1, ]
  
  
  ## JOIN AB info
  lymeAnti3 <- lymeAnti1 %>% group_by(MemberId) %>% 
      summarise(
          nMember = n(),
          drugcateg = first(drugcateg)) %>% filter(nMember == "1") %>% select(-nMember) #a few taking 3 ABs
  
  lyme3 <- lyme2 %>% left_join(lymeAnti3, by = "MemberId")
  
  # replace NA
  lyme3$drugcateg[is.na(lyme3$drugcateg)] <- "no3ABs"
  
  lyme3$drugcateg <- as.factor(lyme3$drugcateg)
  
  lyme3$drugcateg <- factor(lyme3$drugcateg, levels=c("no3ABs", "Amoxicillin", "Cefuroxime", "Doxycycline")) 
  
  
  return(lyme3)
}


addDrugInfoVar <- function(lyme, lymedrug040118) {
  # memberID with drug info
  lymeDrugMemId <- lymedrug040118$MemberId[!is.na(lymedrug040118$d_NationalDrug)] %>% unique
  tmpindex <- which(lyme$MemberId %in% lymeDrugMemId)
  lyme$DrugInfo <- 0
  lyme$DrugInfo[tmpindex] <- 1
  return(lyme)
}


addAntiInfo <- function(lyme, lymeanti040118) {
  # memberID with antibiotic info
  tmp1 <- lymeanti040118$MemberId %>% unique
  tmpindex <- which(lyme$MemberId %in% tmp1)
  lyme$AntiInfo <- 0
  lyme$AntiInfo[tmpindex] <- 1
  return(lyme)
}


## Run function
lymeWab <- addDrugInfoVar(lyme, lymedrug040118)
lymeWab <- addAntiInfo(lymeWab, lymeanti040118)

ctWab <- addDrugInfoVar(ct, ctdrug101719)
ctWab <- addAntiInfo(ctWab, ctanti101719)



## Descriptive for paper
summarytools::dfSummary(lymeWab) %>% summarytools::view()
summarytools::dfSummary(ctWab) %>% summarytools::view()
summarytools::freq(ctWab$stateID, order = "freq")

# # ******** -----
## A.1. Saving Rdata ----

outdirectory <- "data"
outfilename <- "case_control_wAB_110419.Rdata"
save(file=file.path(outdirectory,outfilename),
     lyme, ct, lymeWab, ctWab)
