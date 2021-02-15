## Nov 28 2018
## Goal: Describe lyme, clyme, PTLDS distribution

rm(list=ls()) # clear workspace;  # ls() # list objects in the workspace
cat("\014")   # same as ctrl-L
options(max.print=3000) # default 1000, and rstudio set at 5000
options(warn=1) # default = 0. To check loop warnings
# quartz(height=6, width=8)
# dev.off() # reset par

# Load data
load("LID_data_012721/desc_cleanup_lyme_clyme_ptlds_exclusive.Rdata")  


# ******** -----
# A. Lyme ---------------------------------------------------------------------------

library(tidyverse); library(lubridate); library(gridExtra)


## Functions ----
theme_Publication <- function(base_size=14, base_family="Helvetica") {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size=base_size, base_family=base_family)
        + theme(plot.title = element_text(face = "bold",
                                          size = rel(1.2), hjust = 0.5),
                text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(face = "bold",size = rel(1)),
                axis.title.y = element_text(angle=90,vjust =2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(), 
                axis.line = element_line(colour="black"),
                axis.ticks = element_line(),
                panel.grid.major = element_line(colour="#f0f0f0"),
                panel.grid.minor = element_blank(),
                legend.key = element_rect(colour = NA),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.key.size= unit(0.2, "cm"),
                legend.margin = unit(0, "cm"),
                legend.title = element_text(face="italic"),
                plot.margin=unit(c(10,5,5,5),"mm"),
                strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                strip.text = element_text(face="bold")
        ))
    
}

scale_fill_Publication <- function(...){
    library(scales)
    discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
    
}

scale_colour_Publication <- function(...){
    library(scales)
    discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
    
}



# \\ sex ----
table(lyme$MemberGender)
# F     M 
# 22812 21605 
lyme_gender <- table(lyme$MemberGender)
round(lyme_gender/sum(lyme_gender), 3)
# F     M 
# 0.514 0.486 

# \\ age ----
quantile(lyme$age, na.rm = TRUE)
# 0%  25%  50%  75% 100% 
# 5   29   52   62  100 
summary(lyme$age)
mean(lyme$age, na.rm = TRUE)
# 47.44811
sd(lyme$age, na.rm = TRUE)
# 20.66524


# age distribution
p <- ggplot(lyme, aes(x = age))
p <- p + geom_histogram()
# p

# ggsave("LID_results_012721/hist_l_age.png", scale=1, dpi=400)


# age * gender distribution
p <- ggplot(data=subset(lyme, !is.na(MemberGender)), aes(x = age, color = MemberGender))
p <- p + geom_freqpoly()
p <- p + scale_x_continuous(breaks = seq(0, 100, by = 10))

grid.arrange(p, (p + scale_fill_Publication() + theme_Publication()), nrow = 1)

ggsave("LID_results_012721/hist_freq_l_age_by_sex.pdf", scale=1, dpi=400) #supp fig 2A


p <- ggplot(data = subset(lyme, !is.na(MemberGender)), aes(x = MemberGender, y = age)) 
p <- p + geom_violin(aes(fill = MemberGender)) + geom_boxplot(width = 0.2)
p <- p + geom_jitter(shape=16, alpha = 0.05, size = 0.1, width = 0.05)

# ggsave("LID_results_012721/boxplot_l_age_by_sex.png", scale=1, dpi=400)



# \\ when ----
library(zoo) 
lyme$yearmon <- as.Date(as.yearmon(lyme$DateServiceStarted)) 

# create dataset
lyme_monthcount <- as.data.frame(table(lyme$yearmon))
colnames(lyme_monthcount) <- c("date_l", "count_l")

# class adj
lyme_monthcount$date_l <- as.Date(lyme_monthcount$date_l)

lyme_monthcount$month_l <- month(lyme_monthcount$date_l)

# most are from 2009 to 2013

table(year(lyme$DateServiceStarted))

# restrict from 2009 to 2015
lyme_mcplot <- subset(lyme_monthcount, date_l >= ymd("2009-01-01"))

# plot
p <- ggplot(lyme_mcplot, aes(date_l, count_l, label = month_l))
p <- p + geom_point() + geom_line() + geom_label(nudge_x = 30, size = 3, label.padding = unit(0.1, "lines"))
p <- p + theme(plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"))


# ggsave("LID_results_012721/timeseries_l.png", scale=1, dpi=400)

# when * gender
# create dataset
lyme_monthcount_s <- as.data.frame(table(lyme$yearmon, lyme$MemberGender))
colnames(lyme_monthcount_s) <- c("date_l", "sex_l", "count_l")

# class adj
lyme_monthcount_s$date_l <- as.Date(lyme_monthcount_s$date_l)

lyme_monthcount_s$month_l <- month(lyme_monthcount_s$date_l)

# most are from 2009 to 2013

table(year(lyme$DateServiceStarted))

# restrict from 2008 to 2015
lyme_mcplot_s <- subset(lyme_monthcount_s, date_l >= ymd("2009-01-01"))

# plot
p <- ggplot(lyme_mcplot_s, aes(date_l, count_l, color = sex_l, label = month_l))
p <- p + geom_point() + geom_line() + geom_label(nudge_x = 30, size = 6, label.padding = unit(0.1, "lines"))
p <- p + theme(plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"))
grid.arrange(p, (p + scale_fill_Publication() + theme_Publication()), nrow = 1)
# p

ggsave("LID_results_012721/timeseries_l_by_sex.pdf", scale=1, dpi=400) # suppl figure 3A


# when * age
lyme_monthcount_a <- as.data.frame(table(lyme$yearmon, cut(lyme$age, breaks = c(0, 35, 100))))
colnames(lyme_monthcount_a) <- c("date_l", "age_l", "count_l")


# class adj
lyme_monthcount_a$date_l <- as.Date(lyme_monthcount_a$date_l)

lyme_monthcount_a$month_l <- month(lyme_monthcount_a$date_l)

# restrict from 2009 to 2013
lyme_mcplot_a <- subset(lyme_monthcount_a, date_l >= ymd("2009-01-01"))

# plot
p <- ggplot(lyme_mcplot_a, aes(date_l, count_l, color = age_l, label = month_l))
p <- p + geom_point() + geom_line() + geom_label(nudge_x = 30, size = 3, label.padding = unit(0.1, "lines"))
p <- p + theme(plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"))
grid.arrange(p, (p + scale_fill_Publication() + theme_Publication()), nrow = 1)


# ggsave("LID_results_012721/timeseries_l_by_age.pdf", scale=1, dpi=400)

# \\ location ----


# ******** -----
# B. Chronic-Lyme ---------------------------------------------------------------------------

library(tidyverse); library(lubridate)

# \\ sex ----
table(clyme$MemberGender)
# F    M 
# 1725 1220 
clyme_gender <- table(clyme$MemberGender)
round(clyme_gender/sum(clyme_gender), 3)
# F     M 
# 0.586 0.414 

# \\ age ----
quantile(clyme$age, na.rm = TRUE)
# 0%  25%  50%  75% 100% 
# 6   46   56   66  100 
summary(clyme$age)
mean(clyme$age, na.rm = TRUE)
# 55.20713
sd(clyme$age, na.rm = TRUE)
# 16.36863

# age distribution
p <- ggplot(clyme, aes(x = age))
p <- p + geom_histogram()

# ggsave("LID_results_012721/hist_cl_age.png", scale=1, dpi=400)


# age * gender distribution
p <- ggplot(clyme, aes(x = age, color = MemberGender))
p <- p + geom_freqpoly()
grid.arrange(p, (p + scale_fill_Publication() + theme_Publication()), nrow = 1)

# ggsave("LID_results_012721/hist_freq_cl_age_by_sex.pdf", scale=1, dpi=400)


p <- ggplot(data = subset(clyme, !is.na(MemberGender)), aes(x = MemberGender, y = age)) 
p <- p + geom_violin(aes(fill = MemberGender)) + geom_boxplot(width = 0.2)
p <- p + geom_jitter(shape=16, alpha = 0.2, size = 0.2, width = 0.05)

# ggsave("LID_results_012721/boxplot_cl_age_by_sex.png", scale=1, dpi=400)


# \\ when ----
library(zoo)
clyme$yearmon <- as.Date(as.yearmon(clyme$DateServiceStarted))

# create dataset
clyme_monthcount <- as.data.frame(table(clyme$yearmon))
colnames(clyme_monthcount) <- c("date_cl", "count_cl")

# class adj
clyme_monthcount$date_cl <- as.Date(clyme_monthcount$date_cl)

clyme_monthcount$month_cl <- month(clyme_monthcount$date_cl)

# most are from 2009 to 2013
table(year(clyme$DateServiceStarted))

# restrict from 2009 to 2013
clyme_mcplot <- subset(clyme_monthcount, date_cl >= ymd("2009-01-01"))

# plot
p <- ggplot(clyme_mcplot, aes(date_cl, count_cl, label = month_cl))
p <- p + geom_point() + geom_line() + geom_label(nudge_x = 30, size = 3, label.padding = unit(0.1, "lines"))
p <- p + theme(plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"))


# ggsave("LID_results_012721/timeseries_cl.png", scale=1, dpi=400)


# when * gender
# create dataset
clyme_monthcount_s <- as.data.frame(table(clyme$yearmon, clyme$MemberGender))
colnames(clyme_monthcount_s) <- c("date_cl", "sex_cl", "count_cl")

# class adj
clyme_monthcount_s$date_cl <- as.Date(clyme_monthcount_s$date_cl)

clyme_monthcount_s$month_cl <- month(clyme_monthcount_s$date_cl)

# restrict from 2009 to 2013
clyme_mcplot_s <- subset(clyme_monthcount_s, date_cl >= ymd("2009-01-01"))

# plot
p <- ggplot(clyme_mcplot_s, aes(date_cl, count_cl, color = sex_cl, label = month_cl))
p <- p + geom_point() + geom_line() + geom_label(nudge_x = 30, size = 3, label.padding = unit(0.1, "lines"))
p <- p + theme(plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"))
grid.arrange(p, (p + scale_fill_Publication() + theme_Publication()), nrow = 1)


# ggsave("LID_results_012721/timeseries_cl_by_sex.pdf", scale=1, dpi=400) # 


# when * age (Not big bimodal issue in clyme)
clyme_monthcount_a <- as.data.frame(table(clyme$yearmon, cut(clyme$age, breaks = c(0, 35, 100))))
colnames(clyme_monthcount_a) <- c("date_cl", "age_cl", "count_cl")


# class adj
clyme_monthcount_a$date_cl <- as.Date(clyme_monthcount_a$date_cl)

clyme_monthcount_a$month_cl <- month(clyme_monthcount_a$date_cl)

# restrict from 2009 to 2013
clyme_mcplot_a <- subset(clyme_monthcount_a, date_cl >= ymd("2009-01-01"))

# plot
p <- ggplot(clyme_mcplot_a, aes(date_cl, count_cl, color = age_cl, label = month_cl))
p <- p + geom_point() + geom_line() + geom_label(nudge_x = 30, size = 3, label.padding = unit(0.1, "lines"))
p <- p + theme(plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"))
grid.arrange(p, (p + scale_fill_Publication() + theme_Publication()), nrow = 1)


# ggsave("LID_results_012721/timeseries_cl_by_age.pdf", scale=1, dpi=400)


# \\ location ----



# ******** -----
# C. PTLDS ---------------------------------------------------------------------------

library(tidyverse); library(lubridate)

# \\ sex ----
table(ptlds$MemberGender)
# F   M 
# 681 550
ptlds_gender <- table(ptlds$MemberGender)
round(ptlds_gender/sum(ptlds_gender), 3)
# F     M 
# 0.553 0.447 

# \\ age ----
quantile(ptlds$age, na.rm = TRUE)
# 0%  25%  50%  75% 100% 
# 6   27   49   59   99 
summary(ptlds$age)
mean(ptlds$age, na.rm = TRUE)
# 45.30626
sd(ptlds$age, na.rm = TRUE)
# 19.12797


## update on age 050219
ptlds$yearmon <- as.Date(as.yearmon(ptlds$DateServiceStarted)) 
ptlds$yearFirst <-  year(ptlds$yearmon)
ptlds$age2 <- ptlds$yearFirst - ptlds$MemberBirthYear
ptlds[which(ptlds$age2 > 100), "age"] <- 100 



# age distribution

p <- ggplot(ptlds, aes(x = age))
p <- p + geom_histogram()


# ggsave("LID_results_012721/hist_pt_age.png", scale=1, dpi=400)


# age * gender distribution
# base freqploy (non stacking default)
p <- ggplot(ptlds, aes(x = age, color = MemberGender)) # fill not stacking, because it's a transparent polygon outline, won't mask the back one
p <- p + geom_freqpoly()
p <- p + scale_x_continuous(breaks = seq(0, 100, by = 10))
grid.arrange(p, (p + scale_fill_Publication() + theme_Publication()), nrow = 1)


ggsave("LID_results_012721/hist_freq_pt_age_by_sex.pdf", scale=1, dpi=400) # supp figure 2B


p <- ggplot(data = subset(ptlds, !is.na(MemberGender)), aes(x = MemberGender, y = age)) 
p <- p + geom_violin(aes(fill = MemberGender)) + geom_boxplot(width = 0.2)
p <- p + geom_jitter(shape=16, alpha = 0.2, size = 0.2, width = 0.05)

# ggsave("LID_results_012721/boxplot_pt_age_by_sex.png", scale=1, dpi=400)




# \\ when ----
library(zoo)
ptlds$yearmon <- as.Date(as.yearmon(ptlds$DateServiceStarted))

# create dataset
ptlds_monthcount <- as.data.frame(table(ptlds$yearmon))
colnames(ptlds_monthcount) <- c("date_cl", "count_cl")

# class adj
ptlds_monthcount$date_cl <- as.Date(ptlds_monthcount$date_cl)

ptlds_monthcount$month_cl <- month(ptlds_monthcount$date_cl)

# most are from 2009 to 2013

table(year(ptlds$DateServiceStarted))

# restrict from 2009 to 2013
ptlds_mcplot <- subset(ptlds_monthcount, date_cl >= ymd("2009-01-01"))

# plot
p <- ggplot(ptlds_mcplot, aes(date_cl, count_cl, label = month_cl))
p <- p + geom_point() + geom_line() + geom_label(nudge_x = 30, size = 3, label.padding = unit(0.1, "lines"))
p <- p + theme(plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"))


# ggsave("LID_results_012721/timeseries_pt.png", scale=1, dpi=400)


# when * gender
# create dataset
ptlds_monthcount_s <- as.data.frame(table(ptlds$yearmon, ptlds$MemberGender))
colnames(ptlds_monthcount_s) <- c("date_cl", "sex_cl", "count_cl")

# class adj
ptlds_monthcount_s$date_cl <- as.Date(ptlds_monthcount_s$date_cl)

ptlds_monthcount_s$month_cl <- month(ptlds_monthcount_s$date_cl)

# restrict from 2009 to 2013
ptlds_mcplot_s <- subset(ptlds_monthcount_s, date_cl >= ymd("2009-01-01"))

# plot
p <- ggplot(ptlds_mcplot_s, aes(date_cl, count_cl, color = sex_cl, label = month_cl))
p <- p + geom_point() + geom_line() + geom_label(nudge_x = 30, size = 6, label.padding = unit(0.1, "lines")) # for paper, not for on screen
p <- p + theme(plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"))
grid.arrange(p, (p + scale_fill_Publication() + theme_Publication()), nrow = 1)

ggsave("LID_results_012721/timeseries_pt_by_sex.pdf", scale=1, dpi=400) # supp 3B


# when * age (Not big bimodal issue in ptlds)
ptlds_monthcount_a <- as.data.frame(table(ptlds$yearmon, cut(ptlds$age, breaks = c(0, 35, 100))))
colnames(ptlds_monthcount_a) <- c("date_cl", "age_cl", "count_cl")


# class adj
ptlds_monthcount_a$date_cl <- as.Date(ptlds_monthcount_a$date_cl)

ptlds_monthcount_a$month_cl <- month(ptlds_monthcount_a$date_cl)

# restrict from 2009 to 2013
ptlds_mcplot_a <- subset(ptlds_monthcount_a, date_cl >= ymd("2009-01-01"))

# plot
p <- ggplot(ptlds_mcplot_a, aes(date_cl, count_cl, color = age_cl, label = month_cl))
p <- p + geom_point() + geom_line() + geom_label(nudge_x = 30, size = 3, label.padding = unit(0.1, "lines"))
p <- p + theme(plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"))
grid.arrange(p, (p + scale_fill_Publication() + theme_Publication()), nrow = 1)

# ggsave("LID_results_012721/timeseries_pt_by_age.pdf", scale=1, dpi=400)


# \\ location ----




