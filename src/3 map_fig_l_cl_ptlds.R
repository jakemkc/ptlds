## Nov 27 2018
# Goal: show state map prevalence and other state level statistics

rm(list=ls()) # clear workspace;  # ls() # list objects in the workspace
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000, and rstudio set at 5000
options(warn=1) # default = 0. To check loop warnings
# quartz(height=6, width=8)
# dev.off() # reset par


## load
library(tidyverse)
library(maps)
library(lubridate)


## ******** ---
##' Switch
switchAllZip <- "Y" # Y = ref pop will all zip from aetna; N = only lyme's matching zip
## ******** ---




### ........ ---------------------------------------------------------------
### A. read-data ------------------------------------------------------------

# exclusive data
load("/Volumes/O2_transfer/data/desc_cleanup_lyme_clyme_ptlds_exclusive.Rdata")


# ref pop
if (switchAllZip == "Y") {
    refpop <- readRDS("/Volumes/O2_transfer/data/allzipbyyearpop.rds")
} else {
    refpop <- readRDS("/Volumes/O2_transfer/data/zipbyyearpop.rds") 
}


# zip to zcta
zctatable <- read.csv("data/zip_to_zcta_2016.csv", colClasses = c("character"), stringsAsFactors = FALSE) 

# stateabbr to state
statenameabbv <- read_csv("data/stateabbr_to_state.csv")




### ........ ---------------------------------------------------------------
### B. prep dataframe ------------------------------------------------------------


## ***********
## refpop
# zip to zcta
zctatable <- zctatable[, c("ZIP", "ZCTA_USE", "StateAbbr")]
refpop <- left_join(refpop, zctatable, by = c("zip" = "ZIP"))

# long format
colnames(refpop) <- c("zip", "2009", "2010", "2011", 
                      "2012", "2013", "ZCTA_USE", "StateAbbr")

refpopLong <- refpop %>% gather("2009", "2010", "2011", "2012", "2013", key = year, value = "popcount") %>% select(-"zip")


## ***********
## disease case

prepDataFig <- function(lyme, Lyme) { # data, ID value
    
  ## refpop df by State
  tmp1a <- refpopLong %>% group_by(StateAbbr, year) %>% summarise(poptotal = sum(popcount, na.rm = TRUE))

  ## Disease df by State
  tmp1 <- left_join(lyme, zctatable, by = c("EmployeeZipCode" = "ZIP"))
  
  # create year var
  tmp1$yearstarted <- tmp1$DateServiceStarted %>% year() %>% as.character
  
  # summarize by zip & year (rows)
  
  # summarize by state & year (rows)
  tmp2 <- tmp1 %>% group_by(StateAbbr, yearstarted) %>% tally %>% ungroup
  
  # join refpop value
  tmp3 <- left_join(tmp2, tmp1a, by = c("StateAbbr" = "StateAbbr", "yearstarted" = "year"))
  
  tmp4 <- tmp3[complete.cases(tmp3), ]
  
  # join state full name
  tmp5 <- left_join(tmp4, statenameabbv[c("statename", "uspsabbr")], by = c("StateAbbr" = "uspsabbr"))
  
  tmp5$disease <- Lyme
  
  return(tmp5)
  
}


## prep intermediate dataframe
lymeInt <- prepDataFig(lyme, "Lyme")
clymeInt <- prepDataFig(clyme, "cLyme")
ptldsInt <- prepDataFig(ptlds, "ptlds")


### ........ ---------------------------------------------------------------
### C. Plot ------------------------------------------------------------

## ***********
## barplot (no trend) ----

## Prep barplot dataframe. Incidence prop from 2009 to 2013
bindLymes <- bind_rows(lymeInt, clymeInt, ptldsInt)

# incidence case prep: 09-13 sum
tmp1 <- bindLymes %>% group_by(statename, disease) %>% summarise(diseasetotal = sum(n, na.rm = TRUE))

# incidence pop prep: 09
tmp2 <- refpopLong %>% group_by(StateAbbr, year) %>% summarise(poptotal = sum(popcount, na.rm = TRUE)) %>% filter(year == "2009") %>% select(-year)
tmp3 <- left_join(tmp2, statenameabbv[c("statename", "uspsabbr")], by = c("StateAbbr" = "uspsabbr"))

# merge and create incidence proportion
stateIncidenceProp <- left_join(tmp1, tmp3, by = c("statename" = "statename"))

stateIncidenceProp$diseaseProp <- stateIncidenceProp$diseasetotal/stateIncidenceProp$poptotal * 1000000

## plot incidence prop from 2009 to 2013
p <- ggplot(data = stateIncidenceProp, aes(x = statename, y = diseaseProp, fill = disease))
p <- p + geom_bar(stat = "identity") + coord_flip()
p

## plot incidence prop from 2009 to 2013 (same as above in log scale)
p <- ggplot(data = stateIncidenceProp, aes(x = statename, y = log(diseaseProp), fill = disease))
p <- p + geom_bar(stat = "identity") + coord_flip()
p

## ***********
## split into two plots (high and low)

# 1st abundant half data
firsthalfname <- stateIncidenceProp %>% filter(disease == "Lyme") %>% arrange(desc(diseaseProp)) %>% .[1:13, ] %>% .$statename

firsthalfdata <- stateIncidenceProp %>% filter(statename %in% firsthalfname)


## plot incidence prop from 2009 to 2013
p <- ggplot(data = firsthalfdata, aes(x = statename, y = diseaseProp, fill = disease))
p <- p + geom_bar(stat = "identity") + coord_flip()
p



# second not abundant half data
secondhalfname <- stateIncidenceProp %>% filter(disease == "Lyme") %>% arrange(desc(diseaseProp)) %>% .[14:nrow(.), ] %>% .$statename

secondhalfdata <- stateIncidenceProp %>% filter(statename %in% secondhalfname)

## plot incidence prop from 2009 to 2013
p <- ggplot(data = secondhalfdata, aes(x = statename, y = diseaseProp, fill = disease))
p <- p + geom_bar(stat = "identity") + coord_flip()
p




## ***********
## line plot (trend) ----

## Prep barplot dataframe. Incidence prop from 2009 to 2013
bindLymes <- bind_rows(lymeInt, clymeInt, ptldsInt)

stateIncidencePropYear <- bindLymes %>% mutate(
            diseaseProp = n/poptotal * 1000000)

## plot incidence prop of selected state, by diesase
p <- ggplot(filter(stateIncidencePropYear, statename == "Massachusetts"), aes(x = yearstarted, y = diseaseProp, group = disease))
p <- p + geom_line(aes(color = disease))
p



## plot incidence prop if disease by all state
# noisy in non-lyme; lyme is not new info
p <- ggplot(filter(stateIncidencePropYear, disease == "Lyme"), aes(x = yearstarted, y = diseaseProp, group = statename))
p <- p + geom_line(aes(color = statename))
p

p <- ggplot(filter(stateIncidencePropYear, disease == "cLyme"), aes(x = yearstarted, y = diseaseProp, group = statename))
p <- p + geom_line(aes(color = statename))
p

p <- ggplot(filter(stateIncidencePropYear, disease == "ptlds"), aes(x = yearstarted, y = diseaseProp, group = statename))
p <- p + geom_line(aes(color = statename))
p


## plot incidence prop if disease by top ptlds state
plotdata <- filter(stateIncidencePropYear, disease == "Lyme")

ptldsStateName <- ptldsInt %>% group_by(statename) %>% summarise(diseasetotal = sum(n, na.rm = TRUE)) %>% arrange(desc(diseasetotal))
ptldsStateName <- ptldsStateName$statename[1:10]

plotdata <- filter(stateIncidencePropYear, statename %in% ptldsStateName)


p <- ggplot(filter(plotdata, disease == "Lyme"), aes(x = yearstarted, y = diseaseProp, group = statename))
p <- p + geom_point(aes(color = statename))
p <- p + geom_smooth(aes(color = statename), method = "lm", se = FALSE)
p
# consider make line color transparent, lm solid; consider other otehrs, consider label as ewas


p <- ggplot(filter(plotdata, disease == "cLyme"), aes(x = yearstarted, y = diseaseProp, group = statename))
p <- p + geom_point(aes(color = statename))
p <- p + geom_smooth(aes(color = statename), method = "lm", se = FALSE)
p

p <- ggplot(filter(plotdata, disease == "ptlds"), aes(x = yearstarted, y = diseaseProp, group = statename))
p <- p + geom_point(aes(color = statename))
p <- p + geom_smooth(aes(color = statename), method = "lm", se = FALSE)
p



## Regression ----

plotdata$yearstarted <- as.numeric(plotdata$yearstarted)
str(plotdata)


library(broom)

# Lyme
lymefit1 <- plotdata %>% group_by(StateAbbr) %>% filter(disease == "Lyme") %>% do(fit = lm(diseaseProp ~ yearstarted, data = .))
lymecoef <- lymefit1 %>% tidy(fit) %>% filter (term == "yearstarted") %>% ungroup
lymecoef$fdr <- p.adjust(lymecoef$p.value, method = 'fdr')


# ptlds
pltdsfit1 <- plotdata %>% group_by(StateAbbr) %>% filter(disease == "ptlds") %>% do(fit = lm(diseaseProp ~ yearstarted, data = .))
ptldscoef <- pltdsfit1 %>% tidy(fit) %>% filter (term == "yearstarted") %>% ungroup
ptldscoef$fdr <- p.adjust(ptldscoef$p.value, method = 'fdr')





