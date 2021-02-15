## Mar 22 2019
# Sample from https://github.com/mtennekes/tmap/tree/master/demo/USChoropleth
# Goal: Show lyme and c.lyme geographical distribution (wo drug data)

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
library(tmap); library(tmaptools); library(grid)


## ******** ---
##' Switch
switchAllZip <- "Y" # Y = ref pop will all zip from aetna; N = only lyme's matching zip
## ******** ---




### ........ ---------------------------------------------------------------
### A. read-data ------------------------------------------------------------

# exclusive data
load("LID_data_012721/desc_cleanup_lyme_clyme_ptlds_exclusive.Rdata")

# ref pop
if (switchAllZip == "Y") {
    refpop <- readRDS("data/allzipbyyearpop.rds")
} else {
    refpop <- readRDS("data/zipbyyearpop.rds") 
}


# zip to zcta
zctatable <- read.csv("data/zip_to_zcta_2016.csv", colClasses = c("character"), stringsAsFactors = FALSE) 



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
## Prep intermediate data function

prepDataFig <- function(lyme, Lyme) { # data, ID value
    
    ## refpop df by ZCTA
    tmp1a <- refpopLong %>% drop_na %>% group_by(ZCTA_USE, year) %>% summarise(poptotal = sum(popcount, na.rm = TRUE))
    
    # disease dataframe
    tmp1 <- left_join(lyme, zctatable, by = c("EmployeeZipCode" = "ZIP")) # employeezipcode has NA and U; zcta match them with NA
    
    # create year var
    tmp1$yearstarted <- tmp1$DateServiceStarted %>% year() %>% as.character
    
    # summarize by zcta & year (rows)
    tmp2 <- tmp1 %>% group_by(ZCTA_USE, yearstarted) %>% tally %>% ungroup %>% drop_na(ZCTA_USE) 
    
    
    # join refpop value
    tmp3 <- left_join(tmp2, tmp1a, by = c("ZCTA_USE" = "ZCTA_USE", "yearstarted" = "year")) # row count of tmp3 = tmp2
    
    tmp4 <- tmp3[complete.cases(tmp3), ] 
    
    tmp4$disease <- Lyme
    
    return(tmp4)
    
}


## prep intermediate dataframe
lymeInt <- prepDataFig(lyme, "Lyme")
clymeInt <- prepDataFig(clyme, "cLyme")
ptldsInt <- prepDataFig(ptlds, "ptlds")



## ***********
## Prep map shape file
# map file class = "sf"

# function to obtain US 2014 ZCTA shapefile
get_US_zcta_2014_shape <- function() {
    dir <- tempdir()
    download.file("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_zcta510_500k.zip", destfile = file.path(dir, "cb_2014_us_zcta510_500k.zip"))
    # ref: https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html, select natinal ZCTA to download
    unzip(file.path(dir, "cb_2014_us_zcta510_500k.zip"), exdir = dir)
    read_shape(file.path(dir, "cb_2014_us_zcta510_500k.shp"))
}

# obtain US ZCTA shape
US <- get_US_zcta_2014_shape()

# add State info
US <- append_data(US, zctatable[, c("ZCTA_USE", "StateAbbr")], key.shp = "ZCTA5CE10", key.data = "ZCTA_USE", ignore.duplicates = TRUE)




# function to obtain US 2014 ZCTA shapefile
get_US_state_2014_shape <- function() {
    dir <- tempdir()
    download.file("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_state_5m.zip", destfile = file.path(dir, "cb_2014_us_state_5m.zip"))
    unzip(file.path(dir, "cb_2014_us_state_5m.zip"), exdir = dir)
    read_shape(file.path(dir, "cb_2014_us_state_5m.shp"))
}

# obtain US state shape
US_state_boundary <- get_US_state_2014_shape()



plotPTLDS <- function(year, saveName) {
    
  # ******** -----
  # C. Prep sf data ---------------------------------------------------------------------------

  # check color range
  # hist(ptldsInt$n)


  ## prepare sf data
  ptldsappend <- ptldsInt %>% select(ZCTA_USE, yearstarted, n) %>% filter(yearstarted == year)
  
  
  
  # append data to shape (US file)
  US_ptlds <- append_data(US, ptldsappend, key.shp = "ZCTA5CE10", key.data = "ZCTA_USE", ignore.duplicates = TRUE)
  # Under coverage: 27145 out of 33144 

  
  # ******** -----
  # D Prepare main and inset ---------------------------------------------------------------------------
  
  ## prepare main and inset
  US_cont <- US_ptlds %>%
      subset(!StateAbbr %in% c("AK", "HI", "PR")) %>% 
      simplify_shape(0.2) # take time to run
  
  US_cont <- US_cont[!is.na(US_cont$n),] # main US plot faster
  
  
  US_AK <- US_ptlds %>% 
      subset(StateAbbr == "AK") %>% 
      simplify_shape(0.2) 
  # dont remove NA in "n" to prevent error if empty inset and they are not slow
  
  US_HI <- US_ptlds %>% 
      subset(StateAbbr == "HI") %>% 
      simplify_shape(0.2) 
  # dont remove NA in "n" to prevent error if empty inset and they are not slow
  
  
  
  ## prepare state boundary for main and inset
  US_state_boundary_cont <- US_state_boundary %>% subset(!STUSPS %in% c("AK", "HI", "PR", "GU", "MP", "VI", "AS")) %>% 
      simplify_shape(0.2) 
  
  
  US_state_boundary_AK <- US_state_boundary %>% subset(STUSPS == "AK") %>% 
      simplify_shape(0.2) # take time to run%>% simplify_shape(0.2) # smaller value less detail (0-1)
  
  US_state_boundary_HI <- US_state_boundary %>% subset(STUSPS == "HI") %>% 
      simplify_shape(0.2) # take time to run%>% simplify_shape(0.2) # smaller value less detail (0-1)
  
  
  

  # ******** -----
  # E. plots ---------------------------------------------------------------------------
  
  
  ## palette
  # tmaptools::palette_explorer() # very good shiny app to get tmap and general code
  # palette = "RdYlBu"
  # palette = "Blues", n = 10, contrast = c(0.57, 1)
  pal2 <- colorRampPalette(c("red", "blue"))
  
  

  
  m_cont <- tm_shape(US_state_boundary_cont, projection = 2163) +
      tm_borders(lwd = 0.8, col = "black", alpha = 0.5) +
      tm_shape(US_cont) +
      tm_polygons("n", border.col = NA, border.alpha = NA, lwd = 0, breaks = seq(0.9, 3, by = 0.9), 
                  palette = pal2(6), title = "legend", showNA = FALSE, colorNA = NULL) + 
      tm_layout(title = "Count of PTLDS, per ZCTA", 
                title.position = c("center", "top"), 
                legend.position = c("right", "bottom"), 
                frame = FALSE, 
                inner.margins = c(0.1, 0.1, 0.05, 0.05))
  
  
  ## plot Alaska inset
  m_AK <- tm_shape(US_AK, projection = 3338) +
      tm_polygons("n", border.col = NA, border.alpha = NA, lwd = 0, breaks = seq(0.9, 3, by = 0.9), showNA = FALSE, colorNA = NULL) +
      tm_shape(US_state_boundary_AK) + 
      tm_borders(lwd = 1, col = "black", alpha = .5) +
      tm_layout("Alaska", legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)
  
  
  ## plot Hawaii inset
  m_HI <- tm_shape(US_HI, projection = 3759) +
      tm_polygons("n", border.col = NA, border.alpha = NA, lwd = 0, breaks = seq(0.9, 3, by = 0.9), showNA = FALSE, colorNA = NULL) +
      tm_shape(US_state_boundary_HI) + 
      tm_borders(lwd = 1, col = "black", alpha = .5) +
      tm_layout("Hawaii", legend.show = FALSE, bg.color = NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame = FALSE)
  
  
  ## specify viewports for Alaska and Hawaii
  vp_AK <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
  vp_HI <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)
  
  
  ## save map
  tmap_mode("plot")
  
  outdirectory <- "LID_results_012721"
  outfilename <- sprintf("map_%s_%s.png", saveName, year)
  tmap_save(m_cont, file.path(outdirectory, outfilename), scale = 0.8, outer.margins = 0, dpi = 900,
            insets_tm = list(m_AK, m_HI), 
            insets_vp = list(vp_AK, vp_HI))
}

# sup figure 4
plotPTLDS(2009, "ptlds_count")
# plotPTLDS(2010, "ptlds_count")
# plotPTLDS(2011, "ptlds_count")
# plotPTLDS(2012, "ptlds_count")
plotPTLDS(2013, "ptlds_count")





# it has a color and cut range set for all ptlds plot
plotLYME <- function(year, saveName) {
    
    # ******** -----
    # C. Prep sf data ---------------------------------------------------------------------------
    
    # check color range
    # hist(ptldsInt$n)
    
    
    ## prepare sf data
    lymeappend <- lymeInt %>% select(ZCTA_USE, yearstarted, n) %>% filter(yearstarted == year)
    
    
    
    # append data to shape (US file)
    US_lyme <- append_data(US, lymeappend, key.shp = "ZCTA5CE10", key.data = "ZCTA_USE", ignore.duplicates = TRUE)
    # Under coverage: 27145 out of 33144 
    
    # unmatched_data <- under_coverage()
    # str(unmatched_data)
    
    
    # ******** -----
    # D Prepare main and inset ---------------------------------------------------------------------------
    
    ## prepare main and inset
    US_cont <- US_lyme %>%
        subset(!StateAbbr %in% c("AK", "HI", "PR")) %>% 
        simplify_shape(0.2) # take time to run
    
    US_cont <- US_cont[!is.na(US_cont$n),] # main US plot faster
    
    
    US_AK <- US_lyme %>% 
        subset(StateAbbr == "AK") %>% 
        simplify_shape(0.2) 
    # dont remove NA in "n" to prevent error if empty inset and they are not slow
    
    US_HI <- US_lyme %>% 
        subset(StateAbbr == "HI") %>% 
        simplify_shape(0.2) 
    # dont remove NA in "n" to prevent error if empty inset and they are not slow
    
    
    
    ## prepare state boundary for main and inset
    US_state_boundary_cont <- US_state_boundary %>% subset(!STUSPS %in% c("AK", "HI", "PR", "GU", "MP", "VI", "AS")) %>% 
        simplify_shape(0.2) # take time to run%>% simplify_shape(0.2) # smaller value less detail (0-1)
    # remove GU, MP, VI etc for better auto centering in "plot mode" - only provide main US
    # qtm(US_state_boundary_cont, projection = 2163)
    
    
    US_state_boundary_AK <- US_state_boundary %>% subset(STUSPS == "AK") %>% 
        simplify_shape(0.2) # take time to run%>% simplify_shape(0.2) # smaller value less detail (0-1)
    
    US_state_boundary_HI <- US_state_boundary %>% subset(STUSPS == "HI") %>% 
        simplify_shape(0.2) # take time to run%>% simplify_shape(0.2) # smaller value less detail (0-1)
    
    
    

    
    # ******** -----
    # E. plots ---------------------------------------------------------------------------
    
    
    ## palette
    # tmaptools::palette_explorer() # very good shiny app to get tmap and general code
    # palette = "RdYlBu"
    # palette = "Blues", n = 10, contrast = c(0.57, 1)
    pal2 <- colorRampPalette(c("red", "blue"))
    
    

    
    m_cont <- tm_shape(US_state_boundary_cont, projection = 2163) +
        tm_borders(lwd = 0.8, col = "black", alpha = 0.5) +
        tm_shape(US_cont) +
        tm_polygons("n", border.col = NA, border.alpha = NA, lwd = 0, breaks = seq(1, 12, by = 2), 
                    palette = pal2(6), title = "legend", showNA = FALSE, colorNA = NULL) + # showNA = legend with missing or not; colorNA = Null (transparent)
        tm_layout(title = "Count of Lymes, per ZCTA", 
                  title.position = c("center", "top"), 
                  legend.position = c("right", "bottom"), 
                  frame = FALSE, 
                  inner.margins = c(0.1, 0.1, 0.05, 0.05))
    
    
    ## plot Alaska inset
    m_AK <- tm_shape(US_AK, projection = 3338) +
        tm_polygons("n", border.col = NA, border.alpha = NA, lwd = 0, breaks = seq(1, 12, by = 2), showNA = FALSE, colorNA = NULL) +
        tm_shape(US_state_boundary_AK) + 
        tm_borders(lwd = 1, col = "black", alpha = .5) +
        tm_layout("Alaska", legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)
    
    
    ## plot Hawaii inset
    m_HI <- tm_shape(US_HI, projection = 3759) +
        tm_polygons("n", border.col = NA, border.alpha = NA, lwd = 0, breaks = seq(1, 12, by = 2), showNA = FALSE, colorNA = NULL) +
        tm_shape(US_state_boundary_HI) + 
        tm_borders(lwd = 1, col = "black", alpha = .5) +
        tm_layout("Hawaii", legend.show = FALSE, bg.color = NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame = FALSE)
    
    
    ## specify viewports for Alaska and Hawaii
    vp_AK <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
    vp_HI <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)
    
    
    ## save map
    tmap_mode("plot")
    
    outdirectory <- "LID_results_012721"
    outfilename <- sprintf("map_%s_%s.png", saveName, year)
    tmap_save(m_cont, file.path(outdirectory, outfilename), scale = 0.8, outer.margins = 0, dpi = 900,
              insets_tm = list(m_AK, m_HI), 
              insets_vp = list(vp_AK, vp_HI))
}

# sup figure 4
plotLYME(2009, "lyme_count")
# plotLYME(2010, "lyme_count")
# plotLYME(2011, "lyme_count")
# plotLYME(2012, "lyme_count")
plotLYME(2013, "lyme_count")






