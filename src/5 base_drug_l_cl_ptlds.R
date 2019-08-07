## Nov 29 2018
## Goal: Describe Lyme, clyme, ptlds with drug info distribution

rm(list=ls()) # clear workspace;  # ls() # list objects in the workspace
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000, and rstudio set at 5000
options(warn=1) # default = 0. To check loop warnings
# quartz(height=6, width=8)
# dev.off() # reset par


# Load data
load("/Volumes/O2_transfer/data/desc_cleanup_l_cl_ptlds_drug_exclusive.Rdata")  



# ******** -----
# A. Drugs-x5  ---------------------------------------------------------------------------
library(tidyverse); library(lubridate)




# Create drug category
drug = list(
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

lymeanti040118$drugcateg <- NA
clymeanti040118$drugcateg <- NA
ptldsanti040118$drugcateg <- NA

for(i in 1:length(drug)) {
    mtch <- grep(paste(drug[[i]], collapse = "|"), lymeanti040118$d_NdcDesc, ignore.case = TRUE)
    lymeanti040118$drugcateg[mtch] <- names(drug)[i]
}

for(i in 1:length(drug)) {
    mtch <- grep(paste(drug[[i]], collapse = "|"), clymeanti040118$d_NdcDesc, ignore.case = TRUE)
    clymeanti040118$drugcateg[mtch] <- names(drug)[i]
}

for(i in 1:length(drug)) {
    mtch <- grep(paste(drug[[i]], collapse = "|"), ptldsanti040118$d_NdcDesc, ignore.case = TRUE)
    ptldsanti040118$drugcateg[mtch] <- names(drug)[i]
}



# \\ anti-number ----
# How many antibiotics are taking per patient in lyme and clyme?

# \\ lyme
l_7_count <- lymeanti040118 %>% distinct %>% count(MemberId)
table(l_7_count$n) # half patients got 1 drug claim (24k/28k = 86%)
hist(l_7_count$n[l_7_count$n <= c(20)]) #


# \\ clyme
cl_8_count <- clymeanti040118 %>% distinct %>% count(MemberId) 
table(cl_8_count$n) # most patients got 1 drug claim (600/700 = 85%)
hist(cl_8_count$n[cl_8_count$n <= c(20)]) 

# \\ ptlds
ptldsanti_count <- ptldsanti040118 %>% distinct %>%  count(MemberId) 
table(ptldsanti_count$n) # half patients got 1 drug claim (24k/28k = 86%)
hist(ptldsanti_count$n[ptldsanti_count$n <= c(20)]) #


# plot dataframe
tmp_lyme <- as.data.frame(table(l_7_count$n, useNA = "always"))
tmp_lyme <- transform(tmp_lyme, cumFreq = cumsum(Freq), relative = prop.table(Freq)) # df$var = add one new var
tmp_lyme$grp <- "lyme" 

tmp_clyme <- as.data.frame(table(cl_8_count$n, useNA = "always"))
tmp_clyme <- transform(tmp_clyme, cumFreq = cumsum(Freq), relative = prop.table(Freq))
tmp_clyme$grp <- "clyme"

tmp_ptlds <- as.data.frame(table(ptldsanti_count$n, useNA = "always"))
tmp_ptlds <- transform(tmp_ptlds, cumFreq = cumsum(Freq), relative = prop.table(Freq))
tmp_ptlds$grp <- "ptlds"

drug_no_l_cl_ptlds <- rbind(tmp_lyme, tmp_clyme, tmp_ptlds)


library(ggplot2)

p <- ggplot(drug_no_l_cl_ptlds, aes(x = Var1, y = Freq, fill = factor(grp)))
p <- p + geom_bar(stat="identity",position="dodge")

# normalized
p <- ggplot(drug_no_l_cl_ptlds, aes(x = Var1, y = relative, fill = grp))
p <- p + geom_bar(stat = "identity", position = "dodge")
p <- p + labs(fill = "Group", x = "No. of selected antibiotics taken per patient", y = "Relative freq")

# ggsave("results/barplot_drug_no_per_patient_l_cl_ptlds.png", scale=1, dpi=400)



# \\ anti-ndc ----

# \\ lyme
l_7_ndc_count <- lymeanti040118 %>% distinct %>% count(d_NationalDrug) # by unique drug code

# \\ clyme
cl_8_ndc_count <- clymeanti040118 %>% distinct %>% count(d_NationalDrug) # 


ptldsanti_ndc_count <- ptldsanti040118 %>% distinct %>% count(d_NationalDrug) # by unique drug code



# plot dataframe
tmp_lyme1 <- l_7_ndc_count
tmp_lyme1 <- transform(tmp_lyme1, cumFreq = cumsum(n), relative = prop.table(n))
tmp_lyme1$grp <- "lyme"

tmp_clyme1 <- cl_8_ndc_count
tmp_clyme1 <- transform(tmp_clyme1, cumFreq = cumsum(n), relative = prop.table(n))
tmp_clyme1$grp <- "clyme"

tmp_ptlds1 <- ptldsanti_ndc_count
tmp_ptlds1 <- transform(tmp_ptlds1, cumFreq = cumsum(n), relative = prop.table(n))
tmp_ptlds1$grp <- "ptlds"

drug_no_l_cl_ptlds1 <- rbind(tmp_lyme1, tmp_clyme1, tmp_ptlds1)

# ggplot (boxplot didn't normalize, side-by-side hard to compare here; stack too)
library(ggplot2)

p <- ggplot(drug_no_l_cl_ptlds1, aes(x = d_NationalDrug, y = n, fill = factor(grp)))
p <- p + geom_bar(stat="identity",position="dodge")

p <- ggplot(drug_no_l_cl_ptlds1, aes(x = d_NationalDrug, y = relative, fill = grp))
p <- p + geom_bar(stat = "identity", position = "dodge")


# ggsave("results/barplot_freq_per_NDC_l_cl_ptlds.png", scale=1, dpi=400)


# \\ anti-desc ----
# drug desc distribution between lyme and clyme?

# \\ lyme
l_7_desc_count <- lymeanti040118 %>% distinct %>% count(d_NdcDesc)


# \\ clyme
cl_8_desc_count <- clymeanti040118 %>% distinct %>% count(d_NdcDesc) # 
 

ptldsanti_desc_count <- ptldsanti040118 %>% distinct %>% count(d_NdcDesc) 


# plot dataframe
tmp_lyme2 <- l_7_desc_count
tmp_lyme2 <- transform(tmp_lyme2, cumFreq = cumsum(n), relative = prop.table(n))
tmp_lyme2$grp <- "lyme"

tmp_clyme2 <- cl_8_desc_count
tmp_clyme2 <- transform(tmp_clyme2, cumFreq = cumsum(n), relative = prop.table(n))
tmp_clyme2$grp <- "clyme"

tmp_ptlds2 <- ptldsanti_desc_count
tmp_ptlds2 <- transform(tmp_ptlds2, cumFreq = cumsum(n), relative = prop.table(n))
tmp_ptlds2$grp <- "ptlds"

drug_no_l_cl_ptlds2 <- rbind(tmp_lyme2, tmp_clyme2, tmp_ptlds2)

# ggplot (boxplot didn't normalize, side-by-side hard to compare here; stack too)
library(ggplot2)

p <- ggplot(drug_no_l_cl_ptlds2, aes(x = d_NdcDesc, y = n, fill = factor(grp)))
p <- p + geom_bar(stat="identity",position="dodge")

p <- ggplot(drug_no_l_cl_ptlds2, aes(x = d_NdcDesc, y = relative, fill = grp))
p <- p + geom_bar(stat = "identity", position = "dodge")

# ggsave("results/barplot_freq_per_NDC_desc_l_cl_ptlds.png", scale=1, dpi=400)


# \\ drug-brand ----
# Generic drug?

# \\ lyme
table(is.na(lymeanti040118$d_NationalDrug)) # 
table(lymeanti040118$d_BrandGenInd) # 345/17732 = 1.9% not taking generic drug

# \\ clyme
table(is.na(clymeanti040118$d_NationalDrug)) #
table(clymeanti040118$d_BrandGenInd) # 12/1054 = 1.1% not taking generic drug

# \\ ptlds
table(is.na(ptldsanti040118$d_NationalDrug)) # 
table(ptldsanti040118$d_BrandGenInd) # 9/405 = 2.2% not taking generic drug


# \\ anti-class ----
# distribution of 3 classes

lymeanti040118 %>% distinct %>% group_by(drugcateg) %>% summarise(n = n()) %>% mutate(freq_percent = round(100 * n/sum(n), 0))


clymeanti040118 %>% distinct %>% group_by(drugcateg) %>% summarise(n = n()) %>% mutate(freq_percent = round(100 * n/sum(n), 0))


ptldsanti040118 %>% distinct %>% group_by(drugcateg) %>% summarise(n = n()) %>% mutate(freq_percent = round(100 * n/sum(n), 0))


# \\ anti-class \ association ----
# just putting extra on the above table


tmp_lyme3 <- lymeanti040118 %>% distinct %>% group_by(drugcateg) %>% 
    summarise(
        n = n(), 
        daysupply = round(mean(d_DaysSupply, na.rm = TRUE), 1),
        daysupply_sd = round(sd(d_DaysSupply, na.rm = TRUE), 1),
        age = round(mean(age, na.rm = TRUE), 1),
        age_sd = sd(age, na.rm = TRUE)) %>% 
    mutate(freq_of_n = round(100 * n/sum(n), 1), grp = "lyme")


tmp_clyme3 <- clymeanti040118 %>% distinct %>% group_by(drugcateg) %>% 
    summarise(
        n = n(), 
        daysupply = round(mean(d_DaysSupply, na.rm = TRUE), 1),
        daysupply_sd = round(sd(d_DaysSupply, na.rm = TRUE), 1),
        age = round(mean(age, na.rm = TRUE), 1),
        age_sd = sd(age, na.rm = TRUE)) %>% 
    mutate(freq_of_n= round(100 * n/sum(n), 1), grp = "clyme")


tmp_ptlds3 <- ptldsanti040118 %>% distinct %>% group_by(drugcateg) %>% 
    summarise(
        n = n(), 
        daysupply = round(mean(d_DaysSupply, na.rm = TRUE), 1),
        daysupply_sd = round(sd(d_DaysSupply, na.rm = TRUE), 1),
        age = round(mean(age, na.rm = TRUE), 1),
        age_sd = sd(age, na.rm = TRUE)) %>% 
    mutate(freq_of_n= round(100 * n/sum(n), 1), grp = "ptlds")


# plot dataframe

drug_no_l_cl_ptlds3 <- rbind(tmp_lyme3, tmp_clyme3, tmp_ptlds3)

drug_no_l_cl_ptlds3[!is.na(drug_no_l_cl_ptlds3$drugcateg),]

# ggplot (boxplot didn't normalize, side-by-side hard to compare here; stack too)
library(ggplot2)

p <- ggplot(drug_no_l_cl_ptlds3[!is.na(drug_no_l_cl_ptlds3$drugcateg),], aes(x = drugcateg, y = freq_of_n, fill = grp))
p <- p + geom_bar(stat = "identity", position = "dodge")
p <- p + labs(fill = "Group", x = "Drug", y = "Relative freq")


# ggsave("results/barplot_freq_per_Antibiotic_l_cl_ptlds.png", scale=1, dpi=400)




## chisquare test
## are lyme disease ratio different in those taking Cefuroxime
lymeanti040118 %>% colnames
clymeanti040118 %>% colnames
ptldsanti040118 %>% colnames

lymeanti040118$disease <- "lyme"
clymeanti040118$disease <- "clyme"
ptldsanti040118$disease <- "ptlds"

ctest1 <- rbind(lymeanti040118, clymeanti040118, ptldsanti040118)

# with and without this distinct, make a huge diff. 120318
ctest1 <- ctest1 %>% distinct() 



# Cefuroxime
ctest1$drugtestCefuroxime <- "Cefuroxime"
ctest1$drugtestCefuroxime[ctest1$drugcateg != ctest1$drugtestCefuroxime] <- "Not-Cef"



tbl <-  table(ctest1$drugtestCefuroxime, ctest1$disease) 



chisq.test(tbl)


# amoxicillin

ctest1$drugtestAmoxicillin <- "Amoxicillin"
ctest1$drugtestAmoxicillin[ctest1$drugcateg != ctest1$drugtestAmoxicillin] <- "Not-Amoxicillin"

tbl <-  table(ctest1$drugtestAmoxicillin, ctest1$disease) 

chisq.test(tbl)


# Doxycycline
ctest1$drugtestDoxycycline <- "Doxycycline"
ctest1$drugtestDoxycycline[ctest1$drugcateg != ctest1$drugtestDoxycycline] <- "Not-Amoxicillin"

tbl <-  table(ctest1$drugtestDoxycycline, ctest1$disease) 

chisq.test(tbl)




# \\ anti-classAge \ association ----
# just putting extra on the above table

tmp_lyme3 <- lymeanti040118 %>% distinct %>% group_by(drugcateg) %>% 
    summarise(
        n = n(), 
        age_mean = round(mean(age, na.rm = TRUE), 1),
        age_sd = sd(age, na.rm = TRUE)) %>% 
    mutate(freq_of_n = round(100 * n/sum(n), 1), grp = "lyme")


tmp_clyme3 <- clymeanti040118 %>% distinct %>% group_by(drugcateg) %>% 
    summarise(
        n = n(), 
        age_mean = round(mean(age, na.rm = TRUE), 1),
        age_sd = sd(age, na.rm = TRUE)) %>% 
    mutate(freq_of_n= round(100 * n/sum(n), 1), grp = "clyme")


tmp_ptlds3 <- ptldsanti040118 %>% distinct %>% group_by(drugcateg) %>% 
    summarise(
        n = n(), 
        age_mean = round(mean(age, na.rm = TRUE), 1),
        age_sd = sd(age, na.rm = TRUE)) %>% 
    mutate(freq_of_n= round(100 * n/sum(n), 1), grp = "ptlds")


## how many people under age 8
tmp1 <- lymeanti040118 %>% distinct %>% distinct(MemberId) #7503
tmp2 <- lymeanti040118 %>% distinct %>% filter(age <=8) %>% distinct(MemberId)
tmp3 <- lymeanti040118 %>% distinct %>% filter(age > 8 & age <=35) %>% distinct(MemberId)

tmp1 <- clymeanti040118 %>% distinct %>% distinct(MemberId) #275
tmp2 <- clymeanti040118 %>% distinct %>% filter(age <=8) %>% distinct(MemberId)# n = 0, 0%
tmp3 <- clymeanti040118 %>% distinct %>% filter(age > 8 & age <=35) %>% distinct(MemberId) #n = 28


tmp1 <- ptldsanti040118 %>% distinct %>% distinct(MemberId) # 209
tmp2 <- ptldsanti040118 %>% distinct %>% filter(age <=8) %>% distinct(MemberId)# n = 1,  ~< 0.5%
tmp3 <- ptldsanti040118 %>% distinct %>% filter(age > 8 & age <=35) %>% distinct(MemberId) # n = 48






# ******** -----
# B. Models  ---------------------------------------------------------------------------

## PTLDS
ptlds1 <- ptldsanti040118 %>% distinct
ptlds2 <- ptlds1 %>% group_by(MemberId) %>% 
    summarise(
        nMember = n(),
        nDrugcat = n_distinct(drugcateg),
        sex = first(MemberGender),
        age = first(age),
        lymeClass = "ptlds")


# see mean ~ SD assumption, no overdispersion
summary(ptlds2)
mean(ptlds2$nDrugcat)
sd(ptlds2$nDrugcat)


## LYME
lyme1 <- lymeanti040118 %>% distinct
lyme2 <- lyme1 %>% group_by(MemberId) %>% 
    summarise(
        nMember = n(),
        nDrugcat = n_distinct(drugcateg),
        sex = first(MemberGender),
        age = first(age),
        lymeClass = "lyme")

# see mean ~ SD assumption, no overdispersion
summary(lyme2)
mean(lyme2$nDrugcat)
sd(lyme2$nDrugcat)


## bind tables
q1df <- bind_rows(ptlds2, lyme2)
q1df$lymeClass <- as.factor(q1df$lymeClass)

# check plot
p <- ggplot(q1df, aes(nDrugcat, fill = lymeClass)) +
    geom_histogram(binwidth=.5, position="dodge")

# run model
q1result <- glm(nDrugcat ~ lymeClass + age + sex, family="poisson", data = q1df)

summary(q1result)



# test overdispersion
library(pscl)
q1nb <- MASS::glm.nb(nDrugcat ~ lymeClass + age + sex, data = q1df, trace = TRUE)
odTest(q1nb) #p = 0.5, no need to use NB




##' Q2: Given those patients taking Antibiotics, do lyme and ptlds taking "different type of antibiotic" in a different no. pattern?



PreQ2df <- function(Amoxicillin) {
  ## PTLDS Amoxicillin
  ptlds1 <- ptldsanti040118 %>% distinct
  ptlds2 <- ptlds1 %>% group_by(MemberId) %>% 
      summarise(
          nMember = n(),
          selectedABcount = sum(drugcateg == Amoxicillin),
          sex = first(MemberGender),
          age = first(age),
          lymeClass = "ptlds") %>% 
      mutate(
          selectedABbin = if_else(selectedABcount == 0, 0, 1),
          drugID = Amoxicillin)
  
  
  ## Lyme Amoxicillin
  lyme1 <- lymeanti040118 %>% distinct
  lyme2 <- lyme1 %>% group_by(MemberId) %>% 
      summarise(
          nMember = n(),
          selectedABcount = sum(drugcateg == Amoxicillin),
          sex = first(MemberGender),
          age = first(age),
          lymeClass = "lyme") %>% 
      mutate(
          selectedABbin = if_else(selectedABcount == 0, 0, 1),
          drugID = Amoxicillin)

  
  ## bind tables
  q2df <- bind_rows(ptlds2, lyme2)
  q2df$lymeClass <- as.factor(q2df$lymeClass)
  q2df$selectedABbin <- as.factor(q2df$selectedABbin)
  q2df$sex <- as.factor(q2df$sex)
  return(q2df)
}



## PREP df
names(drug)

Amodf <- PreQ2df("Amoxicillin")
table(Amodf$selectedABbin) # okay
Doxdf <- PreQ2df("Doxycycline")
table(Doxdf$selectedABbin) # okay
Cefudf <- PreQ2df("Cefuroxime")
table(Cefudf$selectedABbin) # okay
Ceftdf <- PreQ2df("Ceftriaxone")
table(Ceftdf$selectedABbin) # hm.., 6 has 1 only, all lyme
Penidf <- PreQ2df("PenicillinG")
table(Penidf$selectedABbin) # hm.., 2 has 1 only, all lyme

q2modelInpt <- bind_rows(Amodf, Doxdf, Cefudf, Ceftdf, Penidf)


## COUNT no. of cases

tmp1 <- q2modelInpt %>% select(lymeClass, selectedABbin, drugID) %>%  group_by(lymeClass, drugID) %>% summarise(
  cases = sum(selectedABbin == "1"),
  totaln = n()
)




## RUN logistic analysis by drug group


# this is chirag l tidyverse way
q2fit <- q2modelInpt %>% group_by(drugID) %>% do(fit = glm(selectedABbin ~ lymeClass + age + sex, data = ., family = "binomial"))

library(broom)


# get OR
q2coeff <- q2fit %>% tidy(fit, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, quick = FALSE) %>% ungroup
q2coeff$fdr <- p.adjust(q2coeff$p.value, method = 'fdr')





##' Q3: similar to Q1, Given those patients taking Antibiotics, do lyme and ptlds patients are getting different prescription length?
#' Ans: no. as a whole or by group



## PTLDS
ptlds1 <- ptldsanti040118 %>% distinct
ptlds2 <- ptlds1 %>% group_by(MemberId) %>% 
    summarise(
        nMember = n(),
        dLength = sum(d_DaysSupply),
        nDrugcat = n_distinct(drugcateg),
        Drug = first(drugcateg),
        sex = first(MemberGender),
        age = first(age),
        lymeClass = "ptlds")



## LYME
lyme1 <- lymeanti040118 %>% distinct
lyme2 <- lyme1 %>% group_by(MemberId) %>% 
    summarise(
        nMember = n(),
        dLength = sum(d_DaysSupply),
        nDrugcat = n_distinct(drugcateg),
        Drug = first(drugcateg),
        sex = first(MemberGender),
        age = first(age),
        lymeClass = "lyme")



## bind tables
q3df <- bind_rows(ptlds2, lyme2)
q3df$lymeClass <- as.factor(q3df$lymeClass)
q3df$sex <- as.factor(q3df$sex)

levels(q3df$lymeClass)
levels(q3df$sex)

## RUN model
glm(dLength ~ lymeClass + age + sex, data = q3df, family = "gaussian") %>% summary


# BY GrOUP
table(q3df$Drug)
# Amoxicillin Ceftriaxone  Cefuroxime Doxycycline PenicillinG 
# 1637           5         364        5704           2 

q3df <- q3df %>% filter(Drug %in% c("Amoxicillin", "Cefuroxime", "Doxycycline"))



q3fit <- q3df %>% group_by(Drug) %>% do(fit = glm(dLength ~ lymeClass + age + sex, data = ., family = "gaussian"))

library(broom)
q3coeff <- q3fit %>% tidy(fit) %>% ungroup






