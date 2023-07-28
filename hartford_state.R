#d rehkopf - edited by c jackson for figure 5
#code started: 31 January 2018
#code updated: 13 February 2020
#Hartford State Analysis 1.26

###############################################################################
#               PACKAGES                                                      #
###############################################################################
####### Load packages -----
library(acs)
library(ggplot2)
library(maps)
library(sas7bdat)
library(data.table)
library(haven)
library(foreign)
library(prettyR)
library(plyr)
library(missForest)
library(gdata)
library(corrplot)
library(Hmisc)
library(reshape)
library(psych)
library(rgdal)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(mapproj)
library(Rmisc)
library(car)
library(ggalt)
library(ggpubr)
library(egg)
library(mgcv)
library(epitools)
library(officer)
library(oaxaca)
library(viridis)
library(ggthemes)
library(sf)
library(tidycensus)
library(dplyr)
library(tidyr)
library(glmnet)
library(R.utils)
library(ipumsr)
library(survey)
library(readxl)




###############################################################################
#               State Geo Data                                                #
###############################################################################
####### Load State shapefiles and FIPS codes -----
stateFIPS <- read.csv("./Data/stateFIPS.csv")
state.geo <- map_data("state")
state.geo$state <- state.geo$region
state <- merge(state.geo, stateFIPS, by = "state")
# test plot:
ggplot(state.geo, aes(long, lat, group = group)) + geom_polygon(color = "white")



###############################################################################
#               State population Data                                         #
###############################################################################
####### Load State Population data    2003 - 2016 ----

# 2000-2009 State Population data:
# https://www.census.gov/data/tables/time-series/demo/popest/intercensal-2000-2010-state.html
#
# 2010-2016 State Population data:
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html
#
#  popsize:
clean.statepop <- function(x) {
  y <- x[6:56,] # remove national data
  names(y)[1] <- "State"
  y$State <- gsub("\\.", "", y$State)
  vars <- c("State", intersect(2000:2019, names(y)))
  return(y[, vars])
}
statepop.00.09.raw <- as.data.frame(read_excel("./Data/single_year_2003-2017/census_pension/state_pops/st-est00int-01.xls", skip = 3))
statepop.10.19.raw <- as.data.frame(read_excel("./Data/single_year_2003-2017/census_pension/state_pops/nst-est2019-01.xlsx", skip = 3))
statepop.00.19.raw <- list(statepop.00.09.raw, statepop.10.19.raw)
statepop.00.19.clean <- lapply(statepop.00.19.raw, clean.statepop)
statepop.00.19 <- Reduce(function(...) merge(...), statepop.00.19.clean)
#
# turn the final data.frame into a list by year (for years 2003 - 2016):
yrs <- as.character(2003:2016)
statepop <- vector(mode = "list", length = length(yrs))
names(statepop) <- yrs
for (i in yrs) {
  statepop[[i]] <- statepop.00.19[, c("State", i)]
  names(statepop[[i]])[2] <- "popsize"
}
lapply(statepop, head) # check results


###############################################################################
#               Load State ACS Data                                           #
###############################################################################
# (this takes a hot minute, so we'll only do it once.)
####### Load ACS data                 2003 - 2017 ----
# From IPUMS ACS single year
# https://usa.ipums.org/usa/
acs.ddi <- read_ipums_ddi("./Data/single_year_2003-2017/ipums_acs_2003_2017/usa_00014.xml")
acs.data <- as.data.frame(read_ipums_micro(acs.ddi))
# tweak vars and split by year
acs.byYear <- split(acs.data, acs.data$YEAR)
rm(acs.data) # this thing is huge (~3.5 Gb) and no longer needed


###############################################################################
#               Productivity and Engagement                                   #
###############################################################################
####### Labor Force Participation     2003 - 2017 ----
# From IPUMS ACS single year
# https://usa.ipums.org/usa/

# ACS data (by Year) is created earlier in the program
# and called "acs.byYear"

# Calculate labor force percentages by state/year.
# Store results in a list indexed by year.
lf.percent.fun <- function(x) {
  # Compute percent of people in labor force who above certain ages:
  
  # total by age
  total.over65 <- sum(x$PERWT[x$AGE >= 65])
  total.over70 <- sum(x$PERWT[x$AGE >= 70])
  total.over75 <- sum(x$PERWT[x$AGE >= 75])
  
  # LABFORCE CODES:
  # 0 = N/A (ie: younger than 16)
  # 1 = Not in labor force
  # 2 = In labor force
  
  # total in labor force by age
  inLF.over65 <- sum(x$PERWT[x$LABFORCE == 2 & x$AGE >= 65])
  inLF.over70 <- sum(x$PERWT[x$LABFORCE == 2 & x$AGE >= 70])
  inLF.over75 <- sum(x$PERWT[x$LABFORCE == 2 & x$AGE >= 75])
  
  # Percent of people in age range who are still in labor force:
  mf65lf <- inLF.over65/total.over65
  mf70lf <- inLF.over70/total.over70
  mf75lf <- inLF.over75/total.over75
  
  result <- c(mf65lf, mf70lf, mf75lf)
  names(result) <- paste0("LaborForce", c(65, 70, 75))
  return(result)
}
lf.byState.fun <- function(x) {
  # Apply lf.percent.fun within each State in a given year:
  y <- split(x, x$STATEFIP)
  lf.pct.by.state <- lapply(y, lf.percent.fun)
  lf.pct <- as.data.frame(do.call(rbind, lf.pct.by.state))
  # Merge results with FIPS codes
  lf.pct$FIPS <- names(lf.pct.by.state)
  m <- merge(stateFIPS, lf.pct, by="FIPS")
  return(m)
}
labor.force <- lapply(acs.byYear, lf.byState.fun)



####### Community/Civic participation 2008 - 2011, 2013 ----
# From the CPS Civic Engagement Supplement
# Pre-existing dataset
cps65 <- get(load("./Data/cps65state.Rdata"))
cps75 <- get(load("./Data/cps75state.Rdata"))
cps6575 <- merge(cps65, cps75, by = c("State", "year"))

cps <- split(cps6575, cps6575$year)
civic.code <- function(x) {
  y <- subset(x, select = c(State,
                            orgcom_prop_a65, orgciv_prop_a65,
                            orgcom_prop_a75, orgciv_prop_a75))
  m <- merge(y, stateFIPS, by = "State")
  return(m)
}
civic <- lapply(cps, civic.code)


####### Average Hours Volunteering    2003 - 2015 -----
# From CPS Volunteer Supplement
# https://www.icpsr.umich.edu/icpsrweb/NADAC/search/studies?q=volunteer+supplement
# age 65+ and age 75+

# Load Data
vol <- list()
vol.dir <- "./Data/single_year_2003-2017/nadac_cps_volunteer_2003_2015"
vol.data <- "DS0001"
for (i in 2003:2015) {
  loc <- paste(vol.dir, i, vol.data, sep = "/")
  files <- list.files(loc)
  rda <- grep("\\.rda", files)
  vol[[as.character(i)]] <- get(load(paste(loc, files[rda], sep = "/")))
}

volunteer.code <- function(x, age) {
  if("PRTAGE" %in% names(x)) {AGE <- x$PRTAGE} # 2003, 2004, 2013 - 2015
  if("PEAGE" %in%  names(x)) {AGE <- x$PEAGE}  # 2005 - 2012
  names(x) <- gsub("^PTS", "PES", names(x)) # PES6* turns to PTS6* beginning in 2011 
  cpsVol <- subset(x,
                   AGE >= age, 
                   select=c(PES5A,PES5B,PES5C,PES5D,PES5E,PES5F,PES5G,
                            PES6A,PES6B,PES6C,PES6D,PES6E,PES6F,PES6G,
                            GESTFIPS,PWSSWGT))
  cpsVol[is.na(cpsVol)] <- 0
  cpsVol$volHours <-  (cpsVol$PES5A * cpsVol$PES6A) + 
                      (cpsVol$PES5B * cpsVol$PES6B) +
                      (cpsVol$PES5C * cpsVol$PES6C) + 
                      (cpsVol$PES5D * cpsVol$PES6D) + 
                      (cpsVol$PES5E * cpsVol$PES6E) +
                      (cpsVol$PES5F * cpsVol$PES6F) +
                      (cpsVol$PES5G * cpsVol$PES6G)
  cpsVol$volHours[cpsVol$volHours > 2080] <- 2080 #Capping maximum value at 40 hours a week for 52 weeks a year
  cpsVol$volAny <- ifelse(cpsVol$volHours >= 1, 1, 0)
  cpsVol$FIPS <- as.numeric(substr(cpsVol$GESTFIPS, 2, 3))
  cpsVol$sampleWeight <- cpsVol$PWSSWGT / mean(cpsVol$PWSSWGT)
  cpsVol$volHoursW <- as.vector(cpsVol$volHours) * as.vector(cpsVol$sampleWeight)
  cpsVol$volAnyW <- as.vector(cpsVol$volAny) * as.vector(cpsVol$sampleWeight)
  volSTATE <- aggregate(cpsVol, by = list(cpsVol$FIPS), FUN = mean, na.rm = TRUE)
  return(volSTATE)
}
create.volunteer.fun <- function(x) {
  volSTATEa65 <- volunteer.code(x, age = 65)
  volSTATEa75 <- volunteer.code(x, age = 75)
  volSTATEa6575 <- merge(volSTATEa65, volSTATEa75, by = c("FIPS"), suffixes = c("_a65", "_a75"))
  vol <- merge(volSTATEa6575, stateFIPS, by="FIPS")
  return(vol)
}
volunteer <- lapply(vol, create.volunteer.fun)



###############################################################################
#               Security                                                      #
###############################################################################
####### Pension Wealth                2003 - 2016 -----
# pension data:
# https://www.census.gov/programs-surveys/aspp/data/tables.All.html
#
# Load and clean all the pension data:
get.pension.data <- function() {
  # Functions used in cleaning pension data files:
  rm.st.loc <- function(x) {
  st.loc <- grep("state|local", tolower(x$State))
  x[-st.loc,]
}
  clean.pension.12.16 <- function(x) {
  nms <- x[,1]
  y <- as.data.frame(t(x))
  names(y) <- nms
  to.remove <- grep("\\.\\.\\.", row.names(y))
  z <- y[-grep("\\.\\.\\.|United States", row.names(y)),
         c("Total Contributions", "Total Membership")]
  names(z) <- c("contributions", "membership")
  z$State <- row.names(z)
  row.names(z) <- NULL
  return(z[,c("State", "contributions", "membership")])
} 
  # Deal with membership spreadsheets from 2003 - 2011
  pen.mem.2003 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/membership/2003ret05.xls",      range = "A17:C67",  col_names = c("State", "nsys", "membership"))
  pen.mem.2004 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/membership/2004ret05a-xls.xls", range = "A16:C66",  col_names = c("State", "nsys", "membership"))
  pen.mem.2005 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/membership/2005ret05a-xls.xls", range = "A15:C65",  col_names = c("State", "nsys", "membership"))
  pen.mem.2006 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/membership/2006ret05a-xls.xls", range = "A16:C66",  col_names = c("State", "nsys", "membership"))
  pen.mem.2007 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/membership/2007ret05.xls",      range = "A15:C167", col_names = c("State", "nsys", "membership"))
  pen.mem.2008 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/membership/2008ret05.xls",      range = "A15:C167", col_names = c("State", "nsys", "membership"))
  pen.mem.2009 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/membership/2009ret05.xls",      range = "A16:C168", col_names = c("State", "nsys", "membership"))
  pen.mem.2010 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/membership/2010ret05.xls",      range = "A15:C167", col_names = c("State", "nsys", "membership"))
  pen.mem.2011 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/membership/2011ret05.xls",      range = "A16:C168", col_names = c("State", "nsys", "membership"))
  pen.mem.03.06.raw <- list(pen.mem.2003, pen.mem.2004, pen.mem.2005, pen.mem.2006)
  pen.mem.07.11.raw <- list(pen.mem.2007, pen.mem.2008, pen.mem.2009, pen.mem.2010, pen.mem.2011)
  pen.mem.03.06 <- lapply(pen.mem.03.06.raw, function(x) x[,c(1,3)])
  pen.mem.07.11 <- lapply(pen.mem.07.11.raw, function(x) rm.st.loc(x[,c(1,3)]))
  pen.mem.03.11 <- lapply(c(pen.mem.03.06, pen.mem.07.11), as.data.frame)
  # Deal with revenue spreadsheets from 2003 - 2011
  pen.rev.2003 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/revenue/2003ret02.xls",      range = "A11:B163", col_names = c("State", "contributions"))
  pen.rev.2004 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/revenue/2004ret02a-xls.xls", range = "A9:B161",  col_names = c("State", "contributions"))
  pen.rev.2005 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/revenue/2005ret02a-xls.xls", range = "A10:B162", col_names = c("State", "contributions"))
  pen.rev.2006 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/revenue/2006ret02a-xls.xls", range = "A10:B162", col_names = c("State", "contributions"))
  pen.rev.2007 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/revenue/2007ret02.xls",      range = "A11:C163", col_names = c("State", "earn", "contributions"))
  pen.rev.2008 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/revenue/2008ret02.xls",      range = "A11:C163", col_names = c("State", "earn", "contributions"))
  pen.rev.2009 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/revenue/2009ret02.xls",      range = "A11:C163", col_names = c("State", "earn", "contributions"))
  pen.rev.2010 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/revenue/2010ret02.xls",      range = "A11:C163", col_names = c("State", "earn", "contributions"))
  pen.rev.2011 <- read_excel("./Data/single_year_2003-2017/census_pension/2003-2011/revenue/2011ret02.xls",      range = "A11:C163", col_names = c("State", "earn", "contributions"))
  pen.rev.03.06.raw <- list(pen.rev.2003, pen.rev.2004, pen.rev.2005, pen.rev.2006)
  pen.rev.07.11.raw <- list(pen.rev.2007, pen.rev.2008, pen.rev.2009, pen.rev.2010, pen.rev.2011)
  pen.rev.03.06 <- lapply(pen.rev.03.06.raw, function(x) rm.st.loc(x))
  pen.rev.07.11 <- lapply(pen.rev.07.11.raw, function(x) rm.st.loc(x[,c(1,3)]))
  pen.rev.03.11 <- lapply(c(pen.rev.03.06, pen.rev.07.11), as.data.frame)
  # Merge membership and revenue data from 2003 - 2011
  pen.03.11 <- mapply(function(a, b) merge(a, b, by="State"), pen.rev.03.11, pen.mem.03.11, SIMPLIFY = FALSE)
  names(pen.03.11) <- 2003:2011
  # Deal with spreadsheets from 2012 - 2016 (they contain both membership and
  # revenue data starting in 2012.)
  pen2012 <- as.data.frame(read_excel("./Data/single_year_2003-2017/census_pension/2012-2016/2012asppsummary.xlsx"))
  pen2013 <- as.data.frame(read_excel("./Data/single_year_2003-2017/census_pension/2012-2016/2013asppsummary.xlsx"))
  pen2014 <- as.data.frame(read_excel("./Data/single_year_2003-2017/census_pension/2012-2016/2014asppsummary.xlsx"))
  pen2015 <- as.data.frame(read_excel("./Data/single_year_2003-2017/census_pension/2012-2016/2015asppsummary.xlsx"))
  pen2016 <- as.data.frame(read_excel("./Data/single_year_2003-2017/census_pension/2012-2016/2016asppsummary.xlsx"))
  pen.12.16.raw <- list(pen2012, pen2013, pen2014, pen2015, pen2016)
  pen.12.16 <- lapply(pen.12.16.raw, clean.pension.12.16)
  names(pen.12.16) <- 2012:2016
  # Combine 2003 - 2011 data with the 2012 - 2016 data:
  pension.only <- c(pen.03.11, pen.12.16)
  
  # merge pension data and popsize (statepop defined earlier in code):
  pension.data <- mapply(function(a, b) merge(a, b), pension.only, statepop, SIMPLIFY = FALSE)
  return(pension.data)
}
pension.data <- get.pension.data()
# Compute necessary pension variables:
pension.code <- function(x) {
  x$pensionPC <- as.numeric(x$contributions)/as.numeric(x$popsize)
  pensiont <- subset(x, select = c(State, pensionPC))
  pension <- merge(stateFIPS, pensiont, by = "State")
  return(pension)
}
pension <- lapply(pension.data, pension.code)
#####



####### State GDP                     2003 - 2016 -----
# 2003 - 2016
# Using data that was already downloaded:
get.gdp.data <- function() {
  stateGDP.raw <- read.csv("./Data/STATE_GDP.csv")
  nms.gdp <- stateGDP.raw$YEAR
  stateGDP <- as.data.frame(t(stateGDP.raw[, -1]))
  names(stateGDP) <- nms.gdp
  stateGDP$ST <- substr(row.names(stateGDP), 1, 2)
  
  yrs <- as.character(2003:2016)
  gdp.list <- vector(mode = "list", length = length(yrs))
  names(gdp.list) <- yrs
  for (i in yrs) {
    gdp.list[[i]] <- stateGDP[, c("ST", i)]
    names(gdp.list[[i]])[2] <- "gdp"
  }
  
  gdp <- lapply(gdp.list, function(x) merge(stateFIPS, x, by="ST"))
  return(gdp)
}
gdp <- get.gdp.data()
#####


####### Violent/Property Crime        2003 - 2014 ------
# 2003 - 2014 (it only goes up to 2014 on website)
# https://www.ucrdatatool.gov/Search/Crime/State/StatebyState.cfm
#
crime.files <- list.files("./Data/single_year_2003-2017/ucr_crime_2003_2014/", full.names = TRUE)
crime.raw <- list()
for (i in crime.files) {
  crime.raw[[i]] <- read.csv(i, skip = 9, nrows = 51)
}
names(crime.raw) <- 2003:2014
clean.crime <- function(x) {
  x$popsize <- x$Population
  x$vcrime <- x$Violent.crime.total
  x$pcrime <- x$Property.crime.total
  x$vcrimerate <- x$Violent.Crime.rate
  x$pcrimerate <- x$Property.crime.rate
  # Subset
  y <- subset(x, select = c(State, vcrimerate, pcrimerate))
  z <- merge(y, stateFIPS, by = "State")
  return(z)
}
crime <- lapply(crime.raw, clean.crime)
#####



####### Poverty                       2003 - 2017 -----
# Percent in poverty 2003 - 2017
# From IPUMS ACS single year
# https://usa.ipums.org/usa/

# ACS data (by Year) is created earlier in the program
# and called "acs.byYear"

# Calculate poverty rates by state/year.
# Store results in a list indexed by year.
poverty.rate.fun <- function(x) {
  # Compute percent of people in poverty
  # above certain ages:
  
  # total by age
  total.over65 <- sum(x$PERWT[x$AGE >= 65])
  total.over75 <- sum(x$PERWT[x$AGE >= 75])
  
  # POVERTY :
  # POVERTY is a 3-digit numeric code expressing each family's 
  # total income for the previous year as a percentage of the 
  # poverty thresholds established by the Social Security 
  # Administration in 1964 and subsequently revised in 1980, 
  # adjusted for inflation (See Poverty Definition Page).
  
  # total in labor force by age
  inPOV.over65 <- sum(x$PERWT[x$POVERTY <= 199 & x$AGE >= 65])
  inPOV.over75 <- sum(x$PERWT[x$POVERTY <= 199 & x$AGE >= 75])
  
  # Percent of people in age range who are still in labor force:
  mf65pov <- inPOV.over65/total.over65
  mf75pov <- inPOV.over75/total.over75
  
  result <- c(mf65pov, mf75pov)
  names(result) <- paste0("poverty", c(65, 75))
  return(result)
}
poverty.byState.fun <- function(x) {
  # Apply lf.percent.fun within each State in a given year:
  y <- split(x, x$STATEFIP)
  pov.pct.by.state <- lapply(y, poverty.rate.fun)
  pov.pct <- as.data.frame(do.call(rbind, pov.pct.by.state))
  # Merge results with FIPS codes
  pov.pct$FIPS <- names(pov.pct.by.state)
  m <- merge(stateFIPS, pov.pct, by="FIPS")
  return(m)
}
poverty <- lapply(acs.byYear, poverty.byState.fun)



####### Food Security                 2003 - 2017 ----
# From IPUMS CPS single year
# https://cps.ipums.org/cps/index.shtml
fs.ddi <- read_ipums_ddi("./Data/single_year_2003-2017/ipums_cps_food_security_2003_2017/cps_00005.xml")
fs.data <- as.data.frame(read_ipums_micro(fs.ddi))
# tweak vars and split by year
fs.byYear <- split(fs.data, fs.data$YEAR)
#rm(fs.data) # this thing is huge (~3.5 Gb) and no longer needed
foodsec.prop.fun <- function(x) {
  # Compute percent of people above certain ages that have low food security:
  
  # FSHWTSCALE is the food security status weight.
  # This weight should be used when analyzing the food security scale data: 
  # FSRASCH, FSRASCHA, FSRASCHC, FSRASCHM, FSRASCHMA, FSRASCHMC, FSRAWSC95, 
  # FSRAWSCR, FSRAWSCRA, FSRAWSCRC, FSRAWSCRM, FSRAWSCRMA, FSRAWSCRMC, FSSTATUS, 
  # FSSTATUS95, FSSTATUSA, FSSTATUSC, FSSTATUSD, FSSTATUSM, FSSTATUSMA, 
  # FSSTATUSMC, FSSTATUSMD. 
  # In most years, it is identical to the supplement 
  # weight, FSSSUPPWTH. In 1998, 1999, and 2007, some rotation groups were 
  # excluded from the food security scales because they were asked test questions 
  # that differed substantially from the main food security questionnaire. 
  # In these years, FSHWTSCALE adjusts for the exclusion of these households 
  # from the food security scale.
  
  # total by age
  total.over65 <- sum(x$FSHWTSCALE[x$AGE >= 65])
  total.over75 <- sum(x$FSHWTSCALE[x$AGE >= 75])
  
  # FSSTATUS
  # 1 = Food Secure
  # 2 = Low Food Secure
  # 3 = Very Low Food Secure
  # 98/99 = Missing / NA
  
  # total in low food security by age
  lowfs.over65 <- sum(x$FSHWTSCALE[x$FSSTATUS == 2 & x$AGE >= 65])
  lowfs.over75 <- sum(x$FSHWTSCALE[x$FSSTATUS == 2 & x$AGE >= 75])
  
  # Percent of people in age range who have low food security:
  fs65 <- lowfs.over65/total.over65
  fs75 <- lowfs.over75/total.over75
  
  result <- c(fs65, fs75)
  names(result) <- paste0("prop_lowfs", c(65, 75))
  return(result)
}
foodsec.byState.fun <- function(x) {
  y <- split(x, x$STATEFIP)
  fs.pct.by.state <- lapply(y, foodsec.prop.fun)
  fs.pct <- as.data.frame(do.call(rbind, fs.pct.by.state))
  fs.pct$FIPS <- names(fs.pct.by.state)
  m <- merge(stateFIPS, fs.pct, by="FIPS")
  return(m)
}
food.security <- lapply(fs.byYear, foodsec.byState.fun)



###############################################################################
#               Equity                                                        #
###############################################################################
####### State Income Inequality        2003 - 2015 ----
incInq <- read.csv("./Data/Frank_Gini_2015.csv")
income.ineq <- list()
for (i in 2003:2015) {
  yr <- as.character(i)
  a <- subset(incInq, incInq$Year == i & incInq$st >= 1)
  m <- merge(a, stateFIPS, by = "State")
  income.ineq[[yr]] <- m
}




####### Ed tertiary & HS comp rate    2003 - 2016 ----
# From IPUMS ACS single year
# https://usa.ipums.org/usa/

# ACS data (by Year) is created earlier in the program
# and called "acs.byYear"

# Calculate poverty rates by state/year.
# Store results in a list indexed by year.
ed.rate.fun <- function(x) {
  # Compute education rates above 65:
  # total by age
  total.over65 <- sum(x$PERWT[x$AGE >= 65])
  
  # EDUCD Codes:
  #   000	N/A or no schooling	
  #   001	N/A	
  #   002	No schooling completed	
  #   010	Nursery school to grade 4	
  #   011	Nursery school, preschool	
  #   012	Kindergarten	
  #   013	Grade 1, 2, 3, or 4	
  #   014	Grade 1	
  #   015	Grade 2	
  #   016	Grade 3	
  #   017	Grade 4	
  #   020	Grade 5, 6, 7, or 8	
  #   021	Grade 5 or 6	
  #   022	Grade 5	
  #   023	Grade 6	
  #   024	Grade 7 or 8	
  #   025	Grade 7	
  #   026	Grade 8	
  #   030	Grade 9	
  #   040	Grade 10	
  #   050	Grade 11	
  #   060	Grade 12	
  #   061	12th grade, no diploma
  #   062	High school graduate or GED	
  #   063	Regular high school diploma	
  #   064	GED or alternative credential	
  #   065	Some college, but less than 1 year	
  #   070	1 year of college	
  #   071	1 or more years of college credit, no degree	
  #   080	2 years of college
  #   081	Associate's degree, type not specified
  #   082	Associate's degree, occupational program
  #   083	Associate's degree, academic program
  #   090	3 years of college
  #   100	4 years of college
  #   101	Bachelor's degree
  #   110	5+ years of college	
  #   111	6 years of college (6+ in 1960-1970)
  #   112	7 years of college
  #   113	8+ years of college
  #   114	Master's degree	
  #   115	Professional degree beyond a bachelor's degree
  #   116	Doctoral degree	
  #   999	Missing	
  
  # We want HS graduate and above (ie: HS grads)
  # and BS and above (ie: college grads)
  hsGrad.over65  <- sum(x$PERWT[x$EDUCD >= 62  & x$EDUCD < 999 & x$AGE >= 65])
  colGrad.over65 <- sum(x$PERWT[x$EDUCD >= 101 & x$EDUCD < 999 & x$AGE >= 65])
  
  # Percent of people in age range who are graduates:
  mf65hs  <- hsGrad.over65/total.over65
  mf65col <- colGrad.over65/total.over65
  
  result <- c(mf65hs, mf65col)
  names(result) <- c("eduHSmf", "eduCOLmf")
  return(result)
}
ed.byState.fun <- function(x) {
  # Apply lf.percent.fun within each State in a given year:
  y <- split(x, x$STATEFIP)
  ed.rates.by.state <- lapply(y, ed.rate.fun)
  ed.rates <- as.data.frame(do.call(rbind, ed.rates.by.state))
  # Merge results with FIPS codes
  ed.rates$FIPS <- names(ed.rates.by.state)
  m <- merge(stateFIPS, ed.rates, by="FIPS")
  return(m)
}
education <- lapply(acs.byYear, ed.byState.fun)


###############################################################################
#               Cohesion                                                      #
###############################################################################
####### Dinner w/HH, helping nhbrs    2008 - 2011, 2013 -----
cohesion.raw <- split(cps6575, cps6575$year)
cohesion.code <- function(x) {
  y <- subset(x, select = c(State,
                            famdin_prop1_a65,social_prop1_a65, favors_prop1_a65,
                            famdin_prop1_a75,social_prop1_a75, favors_prop1_a75))
  m <- merge(y, stateFIPS, by = "State")
  return(m)
}
cohesion <- lapply(cohesion.raw, cohesion.code)
#####


###############################################################################
#               Well-Being                                                    #
###############################################################################
######## BRFSS: Phys/Mental Health    2003 - 2017 ----
# - Read in 2003 to 2017 BRFSS data with demographics and well-being measures
brfss2003 <- sasxport.get("../brfss/CDBRFS03.xpt")
brfss2003sub<-subset(brfss2003,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.finalwt,income2,x.educag,x.racegr2,smokeday,drnk2ge5,exerany2))
brfss2003sub$sampweight <- round((brfss2003sub$x.finalwt/mean(brfss2003sub$x.finalwt)),1)
brfss2003sub$x.rfsmok3<-NA; brfss2003sub$x.rfsmok3[brfss2003sub$smokeday==1 | brfss2003sub$smokeday==2]<-2; brfss2003sub$x.rfsmok3[brfss2003sub$smokeday==3]<-1
brfss2003sub$drnk3ge5<-brfss2003sub$drnk2ge5
brfss2003sub = subset(brfss2003sub, select = -c(x.finalwt,smokeday,drnk2ge5))
rm(brfss2003)

brfss2004 <- sasxport.get("../brfss/CDBRFS04.xpt")
brfss2004sub<-subset(brfss2004,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.finalwt,income2,x.educag,x.racegr2,smokeday,drnk2ge5,exerany2))
brfss2004sub$sampweight <- round((brfss2004sub$x.finalwt/mean(brfss2004sub$x.finalwt)),1)
brfss2004sub$x.rfsmok3<-NA; brfss2004sub$x.rfsmok3[brfss2004sub$smokeday==1 | brfss2004sub$smokeday==2]<-2; brfss2004sub$x.rfsmok3[brfss2004sub$smokeday==3]<-1
brfss2004sub$drnk3ge5<-brfss2004sub$drnk2ge5
brfss2004sub = subset(brfss2004sub, select = -c(x.finalwt,smokeday,drnk2ge5))
rm(brfss2004)

brfss2005 <- sasxport.get("../brfss/CDBRFS05.xpt")
brfss2005sub<-subset(brfss2005,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.finalwt,income2,x.educag,x.racegr2,x.rfsmok3,drnk2ge5,exerany2))
brfss2005sub$sampweight <- round((brfss2005sub$x.finalwt/mean(brfss2005sub$x.finalwt)),1)
brfss2005sub$drnk3ge5<-brfss2005sub$drnk2ge5
brfss2005sub = subset(brfss2005sub, select = -c(x.finalwt,drnk2ge5))
rm(brfss2005)

brfss2006 <- sasxport.get("../brfss/CDBRFS06.xpt")
brfss2006sub<-subset(brfss2006,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.finalwt,income2,x.educag,x.racegr2,x.rfsmok3,drnk3ge5,exerany2))
brfss2006sub$sampweight <- round((brfss2006sub$x.finalwt/mean(brfss2006sub$x.finalwt)),1)
brfss2006sub = subset(brfss2006sub, select = -c(x.finalwt))
rm(brfss2006)

brfss2007 <- sasxport.get("../brfss/CDBRFS07.xpt")
brfss2007sub<-subset(brfss2007,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.finalwt,income2,x.educag,x.racegr2,x.rfsmok3,drnk3ge5,exerany2))
brfss2007sub$sampweight <- round((brfss2007sub$x.finalwt/mean(brfss2007sub$x.finalwt)),1)
brfss2007sub = subset(brfss2007sub, select = -c(x.finalwt))
rm(brfss2007)

brfss2008 <- sasxport.get("../brfss/CDBRFS08.xpt")
brfss2008sub<-subset(brfss2008,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.finalwt,income2,x.educag,x.racegr2,x.rfsmok3,drnk3ge5,exerany2))
brfss2008sub$sampweight <- round((brfss2008sub$x.finalwt/mean(brfss2008sub$x.finalwt)),1)
brfss2008sub = subset(brfss2008sub, select = -c(x.finalwt))
rm(brfss2008)

brfss2009 <- sasxport.get("../brfss/CDBRFS09.xpt")
brfss2009sub<-subset(brfss2009,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.finalwt,income2,x.educag,x.racegr2,x.rfsmok3,drnk3ge5,exerany2))
brfss2009sub$sampweight <- round((brfss2009sub$x.finalwt/mean(brfss2009sub$x.finalwt)),1)
brfss2009sub = subset(brfss2009sub, select = -c(x.finalwt))
rm(brfss2009)

brfss2010 <- sasxport.get("../brfss/CDBRFS10.xpt")
brfss2010sub<-subset(brfss2010,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.finalwt,income2,x.educag,x.racegr2,x.rfsmok3,drnk3ge5,exerany2))
brfss2010sub$sampweight <- round((brfss2010sub$x.finalwt/mean(brfss2010sub$x.finalwt)),1)
brfss2010sub = subset(brfss2010sub, select = -c(x.finalwt))
rm(brfss2010)

brfss2011 <- sasxport.get("../brfss/LLCP2011.xpt")
brfss2011sub<-subset(brfss2011,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.llcpwt,income2,x.educag,x.racegr2,x.rfsmok3,drnk3ge5,exerany2))
brfss2011sub$sampweight <- round((brfss2011sub$x.llcpwt/mean(brfss2011sub$x.llcpwt)),1)
brfss2011sub = subset(brfss2011sub, select = -c(x.llcpwt))
rm(brfss2011)

brfss2012 <- sasxport.get("../brfss/LLCP2012.xpt")
brfss2012sub<-subset(brfss2012,age>=65,select=c(x.state,genhlth,physhlth,menthlth,age,sex,iyear,x.llcpwt,income2,x.educag,x.racegr2,x.rfsmok3,drnk3ge5,exerany2))
brfss2012sub$sampweight <- round((brfss2012sub$x.llcpwt/mean(brfss2012sub$x.llcpwt)),1)
brfss2012sub = subset(brfss2012sub, select = -c(x.llcpwt))
rm(brfss2012)

brfss2013 <- sasxport.get("../brfss/LLCP2013.xpt")
brfss2013sub<-subset(brfss2013,x.age65yr==2,select=c(x.state,genhlth,physhlth,menthlth,x.ageg5yr,sex,iyear,x.llcpwt,income2,x.educag,x.race.g1,x.rfsmok3,drnk3ge5,exerany2))
brfss2013sub$sampweight <- round((brfss2013sub$x.llcpwt/mean(brfss2013sub$x.llcpwt)),1)
brfss2013sub$x.racegr2<-NA; brfss2013sub$x.racegr2[brfss2013sub$x.race.g1==1]<-1; brfss2013sub$x.racegr2[brfss2013sub$x.race.g1==2]<-2;brfss2013sub$x.racegr2[brfss2013sub$x.race.g1==3]<-5;brfss2013sub$x.racegr2[brfss2013sub$x.race.g1==4]<-3;brfss2013sub$x.racegr2[brfss2013sub$x.race.g1==5]<-4
brfss2013sub = subset(brfss2013sub, select = -c(x.llcpwt,x.race.g1))
rm(brfss2013)

brfss2014 <- sasxport.get("../brfss/LLCP2014.xpt")
brfss2014sub<-subset(brfss2014,x.age65yr==2,select=c(x.state,genhlth,physhlth,menthlth,x.ageg5yr,sex,iyear,x.llcpwt,income2,x.educag,x.race.g1,x.rfsmok3,drnk3ge5,exerany2))
brfss2014sub$sampweight <- round((brfss2014sub$x.llcpwt/mean(brfss2014sub$x.llcpwt)),1)
brfss2014sub$x.racegr2<-NA; brfss2014sub$x.racegr2[brfss2014sub$x.race.g1==1]<-1; brfss2014sub$x.racegr2[brfss2014sub$x.race.g1==2]<-2;brfss2014sub$x.racegr2[brfss2014sub$x.race.g1==3]<-5;brfss2014sub$x.racegr2[brfss2014sub$x.race.g1==4]<-3;brfss2014sub$x.racegr2[brfss2014sub$x.race.g1==5]<-4
brfss2014sub = subset(brfss2014sub, select = -c(x.llcpwt,x.race.g1))
rm(brfss2014)

brfss2015 <- sasxport.get("../brfss/LLCP2015.xpt")
brfss2015sub<-subset(brfss2015,x.age65yr==2,select=c(x.state,genhlth,physhlth,menthlth,x.ageg5yr,sex,iyear,x.llcpwt,income2,x.educag,x.race.g1,x.rfsmok3,drnk3ge5,exerany2))
brfss2015sub$sampweight <- round((brfss2015sub$x.llcpwt/mean(brfss2015sub$x.llcpwt)),1)
brfss2015sub$x.racegr2<-NA; brfss2015sub$x.racegr2[brfss2015sub$x.race.g1==1]<-1; brfss2015sub$x.racegr2[brfss2015sub$x.race.g1==2]<-2;brfss2015sub$x.racegr2[brfss2015sub$x.race.g1==3]<-5;brfss2015sub$x.racegr2[brfss2015sub$x.race.g1==4]<-3;brfss2015sub$x.racegr2[brfss2015sub$x.race.g1==5]<-4
brfss2015sub = subset(brfss2015sub, select = -c(x.llcpwt,x.race.g1))
rm(brfss2015)

brfss2016 <- sasxport.get("../brfss/LLCP2016.xpt")
brfss2016sub<-subset(brfss2016,x.age65yr==2,select=c(x.state,genhlth,physhlth,menthlth,x.ageg5yr,sex,iyear,x.llcpwt,income2,x.educag,x.race.g1,x.rfsmok3,drnk3ge5,exerany2))
brfss2016sub$sampweight <- round((brfss2016sub$x.llcpwt/mean(brfss2016sub$x.llcpwt)),1)
brfss2016sub$x.racegr2<-NA; brfss2016sub$x.racegr2[brfss2016sub$x.race.g1==1]<-1; brfss2016sub$x.racegr2[brfss2016sub$x.race.g1==2]<-2;brfss2016sub$x.racegr2[brfss2016sub$x.race.g1==3]<-5;brfss2016sub$x.racegr2[brfss2016sub$x.race.g1==4]<-3;brfss2016sub$x.racegr2[brfss2016sub$x.race.g1==5]<-4
brfss2016sub = subset(brfss2016sub, select = -c(x.llcpwt,x.race.g1))
rm(brfss2016)

brfss2017 <- sasxport.get("../brfss/LLCP2017.xpt")
brfss2017sub<-subset(brfss2017,x.age65yr==2,select=c(x.state,genhlth,physhlth,menthlth,x.ageg5yr,sex,iyear,x.llcpwt,income2,x.educag,x.race.g1,x.rfsmok3,drnk3ge5,exerany2))
brfss2017sub$sampweight <- round((brfss2017sub$x.llcpwt/mean(brfss2017sub$x.llcpwt)),1)
brfss2017sub$x.racegr2<-NA; brfss2017sub$x.racegr2[brfss2017sub$x.race.g1==1]<-1; brfss2017sub$x.racegr2[brfss2017sub$x.race.g1==2]<-2;brfss2017sub$x.racegr2[brfss2017sub$x.race.g1==3]<-5;brfss2017sub$x.racegr2[brfss2017sub$x.race.g1==4]<-3;brfss2017sub$x.racegr2[brfss2017sub$x.race.g1==5]<-4
brfss2017sub = subset(brfss2017sub, select = -c(x.llcpwt,x.race.g1))
rm(brfss2017)

brfss0312<-rbind(brfss2003sub,brfss2004sub,brfss2005sub,brfss2006sub,brfss2007sub,brfss2008sub,brfss2009sub,brfss2010sub,brfss2011sub,brfss2012sub)
brfss0312$age5year<-NA
brfss0312$age5year[brfss0312$age>=18 & brfss0312$age<=24]<-1
brfss0312$age5year[brfss0312$age>=25 & brfss0312$age<=29]<-2
brfss0312$age5year[brfss0312$age>=30 & brfss0312$age<=34]<-3
brfss0312$age5year[brfss0312$age>=35 & brfss0312$age<=39]<-4
brfss0312$age5year[brfss0312$age>=40 & brfss0312$age<=44]<-5
brfss0312$age5year[brfss0312$age>=45 & brfss0312$age<=49]<-6
brfss0312$age5year[brfss0312$age>=50 & brfss0312$age<=54]<-7
brfss0312$age5year[brfss0312$age>=55 & brfss0312$age<=59]<-8
brfss0312$age5year[brfss0312$age>=60 & brfss0312$age<=64]<-9
brfss0312$age5year[brfss0312$age>=65 & brfss0312$age<=69]<-10
brfss0312$age5year[brfss0312$age>=70 & brfss0312$age<=74]<-11
brfss0312$age5year[brfss0312$age>=75 & brfss0312$age<=79]<-12
brfss0312$age5year[brfss0312$age>=80]<-13
brfss0312b<-subset(brfss0312,select=-c(age))

brfss1317<-rbind(brfss2013sub,brfss2014sub,brfss2015sub,brfss2016sub,brfss2017sub)
brfss1317$x.ageg5yr[brfss1317$x.ageg5yr==14]<-NA
brfss1317$age5year<-brfss1317$x.ageg5yr
brfss1317<-subset(brfss1317,select=-c(x.ageg5yr))
brfss0317<-subset(rbind(brfss0312b,brfss1317),x.state<=60)

brfss0317$genhlth[brfss0317$genhlth>5]<-NA
brfss0317$physhlth[brfss0317$physhlth==77]<-NA
brfss0317$physhlth[brfss0317$physhlth==99]<-NA
brfss0317$physhlth[brfss0317$physhlth==88]<-0
brfss0317$menthlth[brfss0317$menthlth==77]<-NA
brfss0317$menthlth[brfss0317$menthlth==99]<-NA
brfss0317$menthlth[brfss0317$menthlth==88]<-0
table(brfss0317$iyear)
brfss0317$iyear<-ifelse(brfss0317$iyear==16,15,brfss0317$iyear)
brfss0317$iyear<-as.numeric(brfss0317$iyear) 
brfss0317$genhlth<-as.numeric(brfss0317$genhlth)

summary(brfss0317)
describe(brfss0317$genhlth)
describe(brfss0317$physhlth)
describe(brfss0317$menthlth)
brfss0317 <- as.data.frame.matrix(brfss0317)

stateFIPS<-read.csv("./Data/stateFIPS.csv")
brfss0317<-merge(brfss0317,stateFIPS,by.x="x.state",by.y="FIPS")
brfss0317$year<-brfss0317$iyear+2002
brfss0317$year[brfss0317$year==2018]<-2017

# - Turn BRFSS into a health list by year
brfss.byYear <- split(brfss0317, brfss0317$year)
health.rate.fun <- function(x) {
  # Data already subset to age 65+ (ie: age5year >= 10)
  ph65 <- mean(x$physhlth, na.rm = TRUE)
  mh65 <- mean(x$menthlth, na.rm = TRUE)

  ph75 <- mean(x$physhlth[x$age5year >= 12], na.rm = TRUE)
  mh75 <- mean(x$menthlth[x$age5year >= 12], na.rm = TRUE)
  
  results <- c(ph65, mh65, ph75, mh75)
  names(results) <- c("physhlth65", "menthlth65", "physhlth75", "menthlth75")
  return(results)
}
health.byState.fun <- function(x) {
  y <- split(x, x$x.state)
  health.rates.by.state <- lapply(y, health.rate.fun)
  health.rates <- as.data.frame(do.call(rbind, health.rates.by.state))
  # merge with FIPS codes
  health.rates$state <- names(health.rates.by.state)
  m <- merge(stateFIPS, health.rates, by.x = "FIPS", by.y = "state")
  return(m)
}
health <- lapply(brfss.byYear, health.byState.fun)




######## Mortality (CDC Compressed)   1999 - 2016 ----
# https://wonder.cdc.gov/
# See readme file in ./Data/single_year_2003-2017/cdc_mortality_1999_2016/
# for details on how to create dataset I used.
mort.raw <- read.delim("./Data/single_year_2003-2017/cdc_mortality_1999_2016/Compressed Mortality, 1999-2016.txt", stringsAsFactors = FALSE)
mort.subset.fun <- function(x) {
  vars <- c("State", "Year", "State.Code", "Age.Group", "Age.Group.Code", "Deaths", "Population", "Crude.Rate")
  age.group <- x$Age.Group.Code %in% c("65-74", "75-84", "85+")
  years <- x$Year %in% 2003:2016
  y <- x[age.group & years, vars]
  y$Deaths     <- as.numeric(y$Deaths)
  y$Population <- as.numeric(y$Population)
  y$Crude.Rate <- as.numeric(y$Crude.Rate)
  return(y)
}
mort.sub <- mort.subset.fun(mort.raw)
mort.byYear <- split(mort.sub, mort.sub$Year)


mortality.code <- function(x) {
  # Using here for all years
  stdPop65 <- c((34264 + 31773), (26999 + 17842), 15508)
  wide <- reshape(x[,-2], idvar = "State", timevar = "Age.Group", direction = "wide")
  
  asMort65 <- as.data.frame(matrix(nrow = 51, ncol = 4))
  asMort75 <- as.data.frame(matrix(nrow = 51, ncol = 4))
  
  # count variables
  cv <- c("Deaths.65-74 years", "Deaths.75-84 years", "Deaths.85+ years")
  # population variables
  pv <- c("Population.65-74 years", "Population.75-84 years", "Population.85+ years")
  
  for (i in 1:51) {
    asMort65[i,] <- ageadjust.direct(wide[i, cv],     wide[i, pv],     rate = NULL, stdPop65)
    asMort75[i,] <- ageadjust.direct(wide[i, cv[-1]], wide[i, pv[-1]], rate = NULL, stdPop65[-1])
  }
  
  nms <- c("crude.rate", "adj.rate", "lci", "uci")
  names(asMort65) <- nms
  names(asMort75) <- nms
  
  asMort <- as.data.frame(cbind(asMort65$adj.rate, asMort75$adj.rate))
  names(asMort) <- paste0("mortality", c(65,75))
  asMort$State <- wide$State
  return(asMort)
}
mortality.fun <- function(x) {
  mort.rates <- mortality.code(x)
  m <- merge(stateFIPS, mort.rates, by="State")
  return(m)
}
mortality <- lapply(mort.byYear, mortality.fun)




###############################################################################
#               Taking Stock of Where We Are                                  #
###############################################################################
# At this point we have the following objects spanning the following years:
# These objects are lists, with elements corresponding to years.

# labor.force      03  04  05  06  07  08  09  10  11  12  13  14  15  16  17
# civic             .   .   .   .   .  08  09  10  11   .  13   .   .   .   .
# volunteer        03  04  05  06  07  08  09  10  11  12  13  14  15   .   .
# pension          03  04  05  06  07  08  09  10  11  12  13  14  15  16   .
# gdp              03  04  05  06  07  08  09  10  11  12  13  14  15  16   .
# crime            03  04  05  06  07  08  09  10  11  12  13  14   .   .   .
# poverty          03  04  05  06  07  08  09  10  11  12  13  14  15  16  17
# food.security    03  04  05  06  07  08  09  10  11  12  13  14  15  16  17
# income.ineq      03  04  05  06  07  08  09  10  11  12  13  14  15   .   .
# education        03  04  05  06  07  08  09  10  11  12  13  14  15  16  17
# cohesion          .   .   .   .   .  08  09  10  11   .  13   .   .   .   .
# health           03  04  05  06  07  08  09  10  11  12  13  14  15  16  17
# mortality        03  04  05  06  07  08  09  10  11  12  13  14  15  16   .  



###############################################################################
#               What To Do With The Missing Years                             #
###############################################################################
# In the above table, some yaers are missing for some datasets.
# For now, we'll just use the closest available year as a stand-in.
# If something else should be done, we can update the following code
# with whatever is decided. But for now:
LIST <- as.list(rep(NA, 15))
names(LIST) <- 2003:2017

if (length(civic) == 5)        {
  civic <- c(LIST[1:5],  # 2003 - 2007
             civic[1:4], # 2008 - 2011
             LIST[10],   # 2012
             civic[5],   # 2013
             LIST[12:15])# 2014 - 2017
}
if (length(volunteer) == 13)   {volunteer <- c(volunteer, LIST[14:15])}
if (length(pension) == 14)     {pension <- c(pension, LIST[15])}
if (length(gdp) == 14)         {gdp <- c(gdp, LIST[15])}
if (length(crime) == 12)       {crime <- c(crime, LIST[13:15])}
if (length(income.ineq) == 13) {income.ineq <- c(income.ineq, LIST[14:15])}
if (length(cohesion) == 5)     {
  cohesion <- c(LIST[1:5],     # 2003 - 2007
                cohesion[1:4], # 2008 - 2011
                LIST[10],      # 2012
                cohesion[5],   # 2013
                LIST[12:15])   # 2014 - 2017
}
if (length(mortality) == 14)   {mortality <- c(mortality, LIST[15])}

civic[["2003"]] <- civic[["2008"]]
civic[["2004"]] <- civic[["2008"]]
civic[["2005"]] <- civic[["2008"]]
civic[["2006"]] <- civic[["2008"]]
civic[["2007"]] <- civic[["2008"]]
civic[["2012"]] <- civic[["2011"]]
civic[["2014"]] <- civic[["2013"]]
civic[["2015"]] <- civic[["2013"]]
civic[["2016"]] <- civic[["2013"]]
civic[["2017"]] <- civic[["2013"]]

volunteer[["2016"]] <- volunteer[["2015"]]
volunteer[["2017"]] <- volunteer[["2015"]]

pension[["2017"]] <- pension[["2016"]]

gdp[["2017"]] <- gdp[["2016"]]

crime[["2015"]] <- crime[["2014"]]
crime[["2016"]] <- crime[["2014"]]
crime[["2017"]] <- crime[["2014"]]

income.ineq[["2016"]] <- income.ineq[["2015"]]
income.ineq[["2017"]] <- income.ineq[["2015"]]

cohesion[["2003"]] <- cohesion[["2008"]]
cohesion[["2004"]] <- cohesion[["2008"]]
cohesion[["2005"]] <- cohesion[["2008"]]
cohesion[["2006"]] <- cohesion[["2008"]]
cohesion[["2007"]] <- cohesion[["2008"]]
cohesion[["2012"]] <- cohesion[["2011"]]
cohesion[["2014"]] <- cohesion[["2013"]]
cohesion[["2015"]] <- cohesion[["2013"]]
cohesion[["2016"]] <- cohesion[["2013"]]
cohesion[["2017"]] <- cohesion[["2013"]]

mortality[["2017"]] <- mortality[["2016"]]

# Now all the lists should have years 2003 - 2017 with data.


###############################################################################
#                               Merging all Data                              #
###############################################################################

# 'Reduce(function(...) merge(..., by = "ST"), list(x, y, z))'
# is the code needed to merge multiple datasets at once.
# It's the more concise version of this:
#   m0 <- merge(x, y, by = "ST")
#   m1 <- merge(m0, z, by = "ST")

# mapply allows you to apply a function to elements of separate lists.
# so, in psuedo-code, 'mapply(merge, volunteer, civic)' would merge
# the first element of volunteer with the first element of civic. Then
# the second element of volunteer with the second element of civic. And
# so on until the nth element of each.

# Here I'm combining the two functions. 'Reduce' merges across three datasets
# at once: labor.force, volunteer, civic.
# And mapply to do it by year (each successive element of each list: 2003 - 2017).
# So the results for pae (in pseudo-code) are:
# pae[[1]]  = merge( labor.force[[1]], volunteer[[1]], civic[[1]] ) # 2003
# pae[[2]]  = merge( labor.force[[2]], volunteer[[2]], civic[[2]] ) # 2004
# ...
# pae[[15]] = merge( labor.force[[15]], volunteer[[15]], civic[[15]] ) # 2017
merge.vars <- c("FIPS", "Region", "Division", "ST", "State", "STATE", "state")

pae <- mapply(function(a,b,c) {Reduce(function(...) merge(..., by = merge.vars), list(a,b,c))},
              a = labor.force, b = volunteer, c = civic, SIMPLIFY = FALSE)
sec <- mapply(function(a,b,c,d,e) {Reduce(function(...) merge(..., by=merge.vars), list(a,b,c,d,e))},
              a = gdp, b = crime, c = pension, d = poverty, e = food.security, SIMPLIFY = FALSE)
equ <- mapply(function(a,b) {Reduce(function(...) merge(..., by=merge.vars), list(a,b))},
              a = income.ineq, b = education, SIMPLIFY = FALSE)
coh <- cohesion
wbe <- mapply(function(a,b) {Reduce(function(...) merge(..., by=merge.vars), list(a,b))},
              a = health, b = mortality, SIMPLIFY = FALSE)
####
domain.unadj <- mapply(function(a,b,c,d,e) {Reduce(function(...) merge(..., by=merge.vars), list(a,b,c,d,e))},
                       a = pae, b = sec, c = equ, d = coh, e = wbe, SIMPLIFY = FALSE)




###############################################################################
#                           state rankings                                    #
###############################################################################

# We wound up preferring the "white_num" version. Formalize with a function:
bw.corr <- function(x, filename, width = 7.11, labcex=0.7, ncex = 0.7,
                    order = "hclust") {
  flnm <- paste0("~/Desktop/", filename, ".pdf")
  flnm.crop <- paste0("~/Desktop/", filename, "_cropped.pdf")
  pdf(file = flnm, width = width) # figure ID changes in final version of paper
  corrplot(cor(x), outline = T, method="color",
           order=order, type="upper",
           # rect.lwd = 5, cl.pos = "n",
           tl.cex = labcex, cl.cex = labcex,
           addCoef.col = "white", 
           number.digits = 1, number.cex = ncex)
  dev.off()
  system(paste("pdfcrop", flnm, flnm.crop))
}
################################################################################




##########################################################################
##########################################################################


#Goal post adjusted
gp.adj <- function(x, numerator.max = T) {
  if (numerator.max == T) {m <- max(x) - x}
  if (numerator.max == F) {m <- x - min(x)}
  n <- max(x) - min(x)
  o <- round(m/n * 100)
  p <- abs(o - 100)
  return(p)
}
goal.post.adjusted <- function(x) {
  #Productivity and Engagement (Labor Force Participation, Community Org, Civic Org, Volunteering)
  x$labor65R <- gp.adj(x$LaborForce65)
  x$labor75R <- gp.adj(x$LaborForce75)
  x$vol65R   <- gp.adj(x$volHoursW_a65)
  x$vol75R   <- gp.adj(x$volHoursW_a75)
  x$civic65R <- gp.adj(x$orgciv_prop_a65)
  x$civic75R <- gp.adj(x$orgciv_prop_a75)
  x$comm65R  <- gp.adj(x$orgcom_prop_a65)
  x$comm75R  <- gp.adj(x$orgcom_prop_a75)
  #Security (State GDP, violent crime, property crime, poverty, food security)
  x$gdpR     <- gp.adj(x$gdp,          numerator.max = F)
  x$violCR   <- gp.adj(x$vcrimerate,   numerator.max = F)
  x$propCR   <- gp.adj(x$pcrimerate,   numerator.max = F)
  x$pensionR <- gp.adj(x$pensionPC,    numerator.max = F)
  x$pov65R   <- gp.adj(x$poverty65,    numerator.max = F)
  x$pov75R   <- gp.adj(x$poverty75,    numerator.max = F)
  x$food65R  <- gp.adj(x$prop_lowfs65, numerator.max = F)
  x$food75R  <- gp.adj(x$prop_lowfs75, numerator.max = F)
  #Equity (Gini, education tertiary, hs completion rate)
  x$incInqR <- gp.adj(x$Gini,          numerator.max = F)
  x$eduHSR  <- gp.adj(x$eduHSmf)
  x$eduCOLR <- gp.adj(x$eduCOLmf)
  #Cohesion (dinner with household, talking with neighbors, favors for neighbors)
  x$din65R   <- gp.adj(x$famdin_prop1_a65)
  x$din75R   <- gp.adj(x$famdin_prop1_a75)
  x$talk65R  <- gp.adj(x$social_prop1_a65)
  x$talk75R  <- gp.adj(x$social_prop1_a75)
  x$favor65R <- gp.adj(x$favors_prop1_a65)
  x$favor75R <- gp.adj(x$favors_prop1_a75)
  #Wellbeing (LE, physical health, mental health)
  x$le65R   <- gp.adj(x$mortality65, numerator.max = F)
  x$le75R   <- gp.adj(x$mortality75, numerator.max = F)
  x$phys65R <- gp.adj(x$physhlth65,  numerator.max = F)
  x$phys75R <- gp.adj(x$physhlth75,  numerator.max = F)
  x$ment65R <- gp.adj(x$menthlth65,  numerator.max = F)
  x$ment75R <- gp.adj(x$menthlth75,  numerator.max = F)
  
  
  #Creating weighted state ranking
  x$productivity65R <-(x$labor65R*0.45 + x$vol65R*0.15 + x$civic65R*0.15 + x$comm65R*0.25)
  x$productivity75R <-(x$labor75R*0.45 + x$vol75R*0.15 + x$civic75R*0.15 + x$comm75R*0.25)
  x$security65R     <-(x$gdpR*0.15 + x$violCR*0.15 + x$propCR*0.15 + x$pensionR*0.15 +x$food65R*0.15 + x$pov65R*0.25)
  x$security75R     <-(x$gdpR*0.15 + x$violCR*0.15 + x$propCR*0.15 + x$pensionR*0.15 +x$food75R*0.15 + x$pov75R*0.25)
  x$equity65R       <-(x$incInqR*0.50 + x$eduHSR*0.25 + x$eduCOLR*0.25)
  x$equity75R       <-(x$incInqR*0.50 + x$eduHSR*0.25 + x$eduCOLR*0.25)
  x$cohesion65R     <-(x$din65R*0.50 + x$talk65R*0.25 + x$favor65R*0.25)
  x$cohesion75R     <-(x$din75R*0.50 + x$talk75R*0.25 + x$favor75R*0.25)
  x$wellbeing65R    <-(x$le65R*0.5 + x$phys65R*0.25 + x$ment65R*0.25)
  x$wellbeing75R    <-(x$le75R*0.5 + x$phys75R*0.25 + x$ment75R*0.25)
  # the following weights add up to 101:
  x$state65R        <-(x$productivity65R*0.22 + x$security65R*0.19 + x$equity65R*0.18 + x$cohesion65R*0.17 + x$wellbeing65R*0.25)
  x$state75R        <-(x$productivity75R*0.22 + x$security75R*0.19 + x$equity75R*0.18 + x$cohesion75R*0.17 + x$wellbeing75R*0.25)
  
  #Creating unweighted state ranking
  x$productivity65Ruw <-(x$labor65R + x$vol65R + x$civic65R + x$comm65R) / 4
  x$productivity75Ruw <-(x$labor75R + x$vol75R + x$civic75R + x$comm75R) / 4
  x$security65Ruw     <-(x$gdpR + x$violCR + x$propCR + x$pensionR + x$food65R + x$pov65R) / 6
  x$security75Ruw     <-(x$gdpR + x$violCR + x$propCR + x$pensionR + x$food75R + x$pov75R) / 6
  x$equity65Ruw       <-(x$incInqR + x$eduHSR + x$eduCOLR) / 3
  x$equity75Ruw       <-(x$incInqR + x$eduHSR + x$eduCOLR) / 3
  x$cohesion65Ruw     <-(x$din65R + x$talk65R + x$favor65R) / 3
  x$cohesion75Ruw     <-(x$din75R + x$talk75R + x$favor75R) / 3
  x$wellbeing65Ruw    <-(x$le65R + x$phys65R + x$ment65R) / 3
  x$wellbeing75Ruw    <-(x$le75R + x$phys75R + x$ment75R) / 3
  x$state65Ruw        <-(x$productivity65Ruw + x$security65Ruw + x$equity65Ruw + x$cohesion65Ruw + x$wellbeing65Ruw) / 5
  x$state75Ruw        <-(x$productivity75Ruw + x$security75Ruw + x$equity75Ruw + x$cohesion75Ruw + x$wellbeing75Ruw) / 5
  
  #Creating drop one domain state rankings
  x$state65RsPro <- (x$security65R*0.19     + x$equity65R*0.18   + x$cohesion65R*0.17 + x$wellbeing65R*0.25) * (1/(0.19 + 0.18 + 0.17 + 0.25))
  x$state65RsSec <- (x$productivity65R*0.22 + x$equity65R*0.18   + x$cohesion65R*0.17 + x$wellbeing65R*0.25) * (1/(0.22 + 0.18 + 0.17 + 0.25))
  x$state65RsEqu <- (x$productivity65R*0.22 + x$security65R*0.19 + x$cohesion65R*0.17 + x$wellbeing65R*0.25) * (1/(0.22 + 0.19 + 0.17 + 0.25))
  x$state65RsCoh <- (x$productivity65R*0.22 + x$security65R*0.19 + x$equity65R*0.18   + x$wellbeing65R*0.25) * (1/(0.22 + 0.19 + 0.18 + 0.25))
  x$state65RsWel <- (x$productivity65R*0.22 + x$security65R*0.19 + x$equity65R*0.18   + x$cohesion65R*0.17)  * (1/(0.22 + 0.19 + 0.18 + 0.17))
  
  return(x)
}
domain <- lapply(domain.unadj, goal.post.adjusted)






###PLOTS#############################################################################################################


##########################################################################
##########################################################################
# Updated figures S3 and S4. (renamed ranks) On Feb 8 2021,
# Figures S3 and S4 (above) be redone to edit group names:
var.nms <- c("volHoursW_a65","orgcom_prop_a65","orgciv_prop_a65",
             "LaborForce65","poverty65","pensionPC",
             "gdp","vcrimerate","pcrimerate","prop_lowfs65",
             "Gini","eduHSmf","eduCOLmf","famdin_prop1_a65",
             "social_prop1_a65","favors_prop1_a65","physhlth65",
             "menthlth65","mortality65")
var.labs <- c("Volunteer Hours", "Community Organization", "Civic Organization",
              "Labor Force Participation", "Poverty", "Pension Wealth", 
              "Gross Domestic Product", "Violent Crime", "Property Crime", "Food Insecurity",
              "Gini", "High School Degree", "College Degree", "Eat with Household",
              "Talk with Neighbors", "Favors for Neighbors", "Physical Health",
              "Mental Health", "Mortality Rate")
s3.dat <- domain[["2003"]][,var.nms]
names(s3.dat) <- var.labs
# looking at plots it seems like we need fig width of ~6.5".
# Specifying width here is a bit tricky since it leaves enough horizontal space
# to accommodate longest variable name. (This is why there is so much white space
# to left of "Physical Health"...there is just enough space there to accomodate
# "Labor Force Participation"). So I fiddled with width until the width of the 
# actual figure was about 6.5".
pdf(file = "~/Desktop/s3_gpadj.pdf", width = 7.15)
corrplot(cor(s3.dat),type="upper",order="hclust", 
         tl.cex=0.7, cl.cex=0.7)
dev.off()
# to crop the extra whitespace, use pdfcrop function from mactex distribution
# note: you need mactex for this:
system("pdfcrop ~/Desktop/s3_gpadj.pdf ~/Desktop/s3_gpadjcrop.pdf")

s4.dat <- domain[["2017"]][,var.nms]
names(s4.dat) <- var.labs
pdf(file = "~/Desktop/figure1_gpadj.pdf", width = 7.11) # figure ID changes in final version of paper
corrplot(cor(s4.dat),type="upper",order="hclust",
         tl.cex=0.7, cl.cex=0.7)
dev.off()
system("pdfcrop ~/Desktop/figure1_gpadj.pdf ~/Desktop/figure1_gpadjcrop.pdf")

bw.corr(s3.dat, "s3_gpadj_bw")
bw.corr(s4.dat, "figure1_gpadj_bw")


################################################################################
################################################################################
# Updated figures S5 and S6. (renamed ranks) On Feb 8 2021, requested
# Figures S5 and S6 (above) be redone to edit group names:
var.nms <- c("productivity65R","security65R","equity65R",
             "cohesion65R","wellbeing65R")
var.labs <- c("Productivity", "Security", "Equity",
              "Cohesion", "Well-Being")
s5.dat <- domain[["2003"]][,var.nms]
names(s5.dat) <- var.labs
# looking at plots it seems like we need fig width of ~6.5".
# Specifying width here is a bit tricky since it leaves enough horizontal space
# to accommodate longest variable name. (This is why there is so much white space
# to left of "Physical Health"...there is just enough space there to accomodate
# "Labor Force Participation"). So I fiddled with width until the width of the 
# actual figure was about 6.5".
pdf(file = "~/Desktop/s4.pdf", width = 5)
corrplot(cor(s5.dat),type="upper", 
         tl.cex=0.9, cl.cex=0.9)
dev.off()
# to crop the extra whitespace, use pdfcrop function from mactex distribution
# note: you need mactex for this:
system("pdfcrop ~/Desktop/s4.pdf ~/Desktop/s4crop.pdf")

s6.dat <- domain[["2017"]][,var.nms]
names(s6.dat) <- var.labs
pdf(file = "~/Desktop/s5.pdf", width = 5) # figure ID changes in final version of paper
corrplot(cor(s6.dat),type="upper",
         tl.cex=0.9, cl.cex=0.9)
dev.off()
system("pdfcrop ~/Desktop/s5.pdf ~/Desktop/s5crop.pdf")

##
bw.corr(s5.dat, "s4_bw", width = 5, labcex = 0.9, ncex = 1, order="original")
bw.corr(s6.dat, "s5_bw", width = 5, labcex = 0.9, ncex = 1, order="original")
##########################################################################
##########################################################################

##########################################################################
##########################################################################
# Updated figure 1, changed color scheme. On Feb 8 2021, request to
# change of color scheme to satisfy reviewer.
us.state=geo.make(state="*")
state.df=map_data("state")
state.df <- reshape::rename(state.df, c(region="state"))
stateDAT=merge(state.df, stateFIPS, by=c("state"))
domainT2 <- domain[["2017"]]
stateDATA<-merge(stateDAT,domainT2, by=c("FIPS"))
stateDATAb<-subset(stateDATA,select=c(
  "state65R","state65Ruw","state75R","state65RsPro","state65RsSec",
  "state65RsEqu","state65RsCoh","state65RsWel","productivity65R","security65R",
  "equity65R","cohesion65R","wellbeing65R","FIPS","long","lat","group"))
fill.gradient <- scale_fill_gradient(low="#FED976", high="#B10026")
border.col <- "darkgrey"

ggplot(stateDATAb, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = state65R), colour = border.col) + 
  fill.gradient + 
  labs(fill = "Aging Index") + 
  theme_map()
ggsave("~/Desktop/figure2.pdf", width = 6, height = 4, units ="in")
system("pdfcrop ~/Desktop/figure2.pdf ~/Desktop/figure2crop.pdf")

##################################################################
stateDATA_t1 <- merge(stateFIPS, domain[["2003"]], by = c("FIPS"))
stateDATA_t2 <- merge(stateFIPS, domain[["2017"]], by = c("FIPS"))

milbank2003 <- unique(stateDATA_t1[, c("FIPS", "ST.x", "State.x", "state65R")])
milbank2017 <- unique(stateDATA_t2[, c("FIPS", "ST.x", "State.x", "state65R")])
names(milbank2003) <- c("fips", "ST", "State", "index65")
names(milbank2017) <- c("fips", "ST", "State", "index65")
milbank2003$year <- 2003
milbank2017$year <- 2017
milbank2003$fips <- formatC(milbank2003$fips, width = 2, format = "d", flag = "0")
milbank2017$fips <- formatC(milbank2017$fips, width = 2, format = "d", flag = "0")
# plot maps to (a) see if they're different and (b) make sure
# the 2017 map shows same results as above.
library(usmap)
plot_usmap(data = milbank2003[, c("fips", "index65")], values="index65") +
  scale_fill_gradient(low="#FED976", high="#B10026")
plot_usmap(data = milbank2017[, c("fips", "index65")], values="index65") +
  scale_fill_gradient(low="#FED976", high="#B10026")
# save data
write.csv(milbank2003, "~/Desktop/fig2_2003.csv", na="", row.names=F)
write.csv(milbank2017, "~/Desktop/fig2_2017.csv", na="", row.names=F)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

##########################################################################
##########################################################################


###################################################################
# Updated and graphed on Aug. 19, 2021
# Reviewer wanted the x-axis title change to init caps.
# When re-creating graph, order of states changes a bit from
# what was currently being used. Suspect this graph was never
# re-done with the new data.
ggplot(domainT2, aes(x=reorder(State, state65R), 
                       y=state65R, label=State)) +
  geom_point(stat='identity', size=1)  +
  #labs(title="Overall State Aging Rank 2017") +
  labs(y="Weighted Average Across Domains", x="State") +
  coord_flip() # + theme(axis.text=element_text(size=7))
# This works best if changing axis.text size to 7:
# ggsave("~/Desktop/figure3.pdf", width = 5.5, height=6)
ggsave("~/Desktop/figure3.pdf", width = 8.5, height=11)
system("pdfcrop ~/Desktop/figure3.pdf ~/Desktop/figure3crop.pdf")
##################################################################

##########################################################################
##########################################################################
# Updated figures 3a - 3e, changed color scheme. On Feb 8 2021, request to
# change of color scheme to satisfy reviewer.
ggplot(stateDATAb, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = productivity65R), colour = border.col) 
  fill.gradient + 
  labs(fill = "Productivity") + 
  theme_map()
# Figure number changes in updated manuscript:
ggsave("~/Desktop/figure4a.pdf", width = 6, height = 4, units ="in")
system("pdfcrop ~/Desktop/figure4a.pdf ~/Desktop/figure4acrop.pdf")


ggplot(stateDATAb, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = security65R), colour = border.col) + 
  fill.gradient +
  labs(fill = "Security") + 
  theme_map()
# Figure number changes in updated manuscript:
ggsave("~/Desktop/figure4b.pdf", width = 6, height = 4, units ="in")
system("pdfcrop ~/Desktop/figure4b.pdf ~/Desktop/figure4bcrop.pdf")


ggplot(stateDATAb, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = equity65R), colour = border.col) + 
  fill.gradient + 
  labs(fill = "Equity") + 
  theme_map()
# Figure number changes in updated manuscript:
ggsave("~/Desktop/figure4c.pdf", width = 6, height = 4, units ="in")
system("pdfcrop ~/Desktop/figure4c.pdf ~/Desktop/figure4ccrop.pdf")


ggplot(stateDATAb, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = cohesion65R), colour = border.col) + 
  fill.gradient + 
  labs(fill = "Cohesion") + 
  theme_map()
# Figure number changes in updated manuscript:
ggsave("~/Desktop/figure4d.pdf", width = 6, height = 4, units ="in")
system("pdfcrop ~/Desktop/figure4d.pdf ~/Desktop/figure4dcrop.pdf")


ggplot(stateDATAb, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = wellbeing65R), colour = border.col) + 
  fill.gradient + 
  labs(fill = "Well-Being") + 
  theme_map()
# Figure number changes in updated manuscript:
ggsave("~/Desktop/figure4e.pdf", width = 6, height = 4, units ="in")
system("pdfcrop ~/Desktop/figure4e.pdf ~/Desktop/figure4ecrop.pdf")
##########################################################################
##########################################################################



###################################################################
# Updated and graphed on Aug. 19, 2021
# Reviewer wanted the x-axis title change to init caps.
# figure 5a
ggplot(domainT2, aes(x=reorder(State, productivity65R), 
                       y=productivity65R, label=State)) +
  geom_point(stat='identity', size=1)  +
  labs(y="Weighted Rank of Domain", x="State") +
  coord_flip()
ggsave("~/Desktop/figure5a.pdf", width = 8.5, height=11)
system("pdfcrop ~/Desktop/figure5a.pdf ~/Desktop/figure5acrop.pdf")

# figure 5b
ggplot(domainT2, aes(x=reorder(State, security65R), 
                     y=security65R, label=State)) +
  geom_point(stat='identity', size=1)  +
  labs(y="Weighted Rank of Domain", x="State") +
  coord_flip() 
ggsave("~/Desktop/figure5b.pdf", width = 8.5, height=11)
system("pdfcrop ~/Desktop/figure5b.pdf ~/Desktop/figure5bcrop.pdf")

# figure 5c
ggplot(domainT2, aes(x=reorder(State, equity65R), 
                     y=equity65R, label=State)) +
  geom_point(stat='identity', size=1)  +
  labs(y="Weighted Rank of Domain", x="State") +
  coord_flip() 
ggsave("~/Desktop/figure5c.pdf", width = 8.5, height=11)
system("pdfcrop ~/Desktop/figure5c.pdf ~/Desktop/figure5ccrop.pdf")

# figure 5d
ggplot(domainT2, aes(x=reorder(State, cohesion65R), 
                     y=cohesion65R, label=State)) +
  geom_point(stat='identity', size=1)  +
  labs(y="Weighted Rank of Domain", x="State") +
  coord_flip() 
ggsave("~/Desktop/figure5d.pdf", width = 8.5, height=11)
system("pdfcrop ~/Desktop/figure5d.pdf ~/Desktop/figure5dcrop.pdf")

# figure 5e
ggplot(domainT2, aes(x=reorder(State, wellbeing65R), 
                     y=wellbeing65R, label=State)) +
  geom_point(stat='identity', size=1)  +
  labs(y="Weighted Rank of Domain", x="State") +
  coord_flip() 
ggsave("~/Desktop/figure5e.pdf", width = 8.5, height=11)
system("pdfcrop ~/Desktop/figure5e.pdf ~/Desktop/figure5ecrop.pdf")



################################################################################
# Begin Deliverables
################################################################################
# 1) plot of state rank slopes for each state

# domain has the data by year. We need the data by state.
# Create "year" variable within each year's dataframe
for (i in seq_along(domain)) {
  domain[[i]]$year <- as.numeric(names(domain)[i])
}
# Split domain on State instead of Year
year2state.fun <- function(x) {
  y <- do.call(rbind, x)
  z <- split(y, y$State)
  return(z)
}
domain.byState <- year2state.fun(domain)

# Regress StateRank on Year for each state:
sr.ols.list <- lapply(domain.byState, function(x) {
  model <- lm(state65R ~ year, data = x)
})

# Find slopes with CIs
sr.slopes.list <- lapply(sr.ols.list, function(x) {
  coefs <- summary(x)$coefficients
  slope <- coefs["year", "Estimate"]
  sd <- coefs["year", "Std. Error"]
  lci <- slope - 1.96 * sd
  uci <- slope + 1.96 * sd
  df <- data.frame(year.slope = slope, slope, lci, uci)
  return(df)
})
# Create a dataframe of those slopes/CIs
sr.slopes <- do.call(rbind, sr.slopes.list)
sr.slopes$State <- row.names(sr.slopes)
# Order by decreasing slope
ordered.slopes <- sr.slopes[order(sr.slopes$year.slope),]
ordered.slopes$State <- factor(ordered.slopes$State, levels=ordered.slopes$State)
# PLOT
ggplot(ordered.slopes, aes(x=year.slope, y=as.factor(State))) +
  geom_errorbarh(aes(xmin=lci, xmax=uci), height = .5, size = .5, color = "orange") +
  scale_x_continuous() + 
  geom_point(stat='identity', fill="black", size=1) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank()) +
  labs(x=NULL,  y=NULL, title="State Rank Slopes", subtitle="2003 (orange dot) to 2017 (black dot)") 
ggsave("~/Desktop/stateSlopes.pdf", width=8.5, height=11)

################################################################################
# Panel plot of Rank by Year for each State with regression line:
################################################################################
# 2) panel plot of yearly ranks for each state with regression line.
ggplot(do.call(rbind, domain.byState), aes(x = year, y = state65R)) + 
  geom_point(size = 0.1) +
  # If you set se = TRUE below, you get error bars on the graph
  # but I think these graphs are too small to show that well.
  stat_smooth(method = "lm", col = "red", size = .2, se = FALSE) +
  scale_y_continuous(breaks = c(30,50,70), labels = c(30,50,70)) +
  facet_wrap( ~ ST, nrow = 10) + theme(legend.position = "none") +
  #labs(x=NULL,  y=NULL, title="Change in State Rank Over Time") 
  labs(x=NULL,  y=NULL, title=element_blank())
ggsave("~/Desktop/s9.pdf", width=8, height=8)
system("pdfcrop ~/Desktop/s9.pdf ~/Desktop/s9crop.pdf")


# Redoing plots for Figure 7
gulf <- domain.byState[c("Alabama", "Arkansas", "Louisiana", "Mississippi")]
ggplot(do.call(rbind, gulf), aes(x = year, y = state65R)) + 
  geom_point(size = 1) +
  # If you set se = TRUE below, you get error bars on the graph
  # but I think these graphs are too small to show that well.
  stat_smooth(method = "lm", col = "red", size = 1, se = T) +
  scale_y_continuous(breaks = seq(25,50,by=5), 
                     labels = seq(25,50,by=5)) +
  facet_wrap( ~ State, nrow = 2) + theme(legend.position = "none") +
  #labs(x=NULL,  y=NULL, title="Change in State Rank Over Time") 
  labs(x=NULL,  y=NULL, title=element_blank())
ggsave("~/Desktop/figure7.pdf", width=6, height=6)
system("pdfcrop ~/Desktop/figure7.pdf ~/Desktop/figure7crop.pdf")


################################################################################
# End Deliverables
################################################################################



#Figure S6: weighted vs unweighted 2017
gdpDotplot <- ggplot(domainT2, aes(x=state65R, xend=state65Ruw,
                                   y=reorder(State, state65R)))
gdpDotplot <- gdpDotplot + geom_dumbbell(color="orange", size=0.75) +
  scale_x_continuous() +
  geom_point(stat='identity', fill="black", size=1)  +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank()) +
  labs(x=NULL,  y=NULL) 
gdpDotplot
ggsave("~/Desktop/figureS6.pdf", width=8.5, height=11)
system("pdfcrop ~/Desktop/figureS6.pdf ~/Desktop/figureS6crop.pdf")

#Figure S7: age 65+ vs age 75+
gdpDotplot <- ggplot(domainT2, aes(x=state65R, xend=state75R, 
                                   y=reorder(State, state65R)))
gdpDotplot <- gdpDotplot + geom_dumbbell(color="orange", size=0.75) +
  scale_x_continuous() +
  geom_point(stat='identity', fill="black", size=1)  +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank()) +
  labs(x=NULL,  y=NULL)
gdpDotplot
ggsave("~/Desktop/figureS7.pdf", width=8.5, height=11)
system("pdfcrop ~/Desktop/figureS7.pdf ~/Desktop/figureS7crop.pdf")

###Figure S8: Dotpolots of domains in 2017
gdpDotplot <- ggplot(domainT2, aes(x=state65R, 
                                   y=reorder(State, state65R)))
gdpDotplot <- gdpDotplot + geom_point(aes(x=state65RsPro,y=reorder(State, state65R)),colour="orange")
  gdpDotplot <- gdpDotplot + geom_point(aes(x=state65RsSec,y=reorder(State, state65R)),colour="orange")
  gdpDotplot <- gdpDotplot + geom_point(aes(x=state65RsEqu,y=reorder(State, state65R)),colour="orange")
  gdpDotplot <- gdpDotplot + geom_point(aes(x=state65RsCoh,y=reorder(State, state65R)),colour="orange")
  gdpDotplot <- gdpDotplot + geom_point(aes(x=state65RsWel,y=reorder(State, state65R)),colour="orange") +
  scale_x_continuous() +
  geom_point(stat='identity', fill="orange")  +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank()) +
  labs(x=NULL,  y=NULL)
gdpDotplot
ggsave("~/Desktop/figureS8.pdf", width=8.5, height=11)
system("pdfcrop ~/Desktop/figureS8.pdf ~/Desktop/figureS8crop.pdf")
###################################################################



# #############################################################################################
# ###STATE DRIVERS DATA: 2003-5 and 2015-2017##################################################
# #############################################################################################

####-----> Functions for State Drivers ----

# regression slope extractor function
slope.extract <- function(x, var="year") {
  a <- summary(x)
  b <- a$coefficients
  return(b[var, "Estimate"])
}

# State regression function:
state.regression <- function(x, lm.var, rename=lm.var) {
  if("state" %in% tolower(names(x))) {
    state.i <- which(tolower(names(x)) == "state")
    names(x)[state.i] <- "State"}
  if("year" %in% tolower(names(x))) {
    year.i <- which(tolower(names(x)) == "year")
    names(x)[year.i] <- "year"}
  
  byState <- split(x, x$State)
  .f <- reformulate("year", lm.var)
  byState.lm <- lapply(byState, function(x) lm(.f, data=x))
  byState.slope <- lapply(byState.lm, slope.extract)
  
  slopes <- unlist(byState.slope)
  df <- data.frame(names(byState.slope),
                   unlist(byState.slope))
  names(df) <- c("State", paste0(rename, ".slope"))
  df[[paste0(rename,".slope.scaled")]] <- scale(df[[paste0(rename, ".slope")]])
  row.names(df) <- NULL
  df
}

# Hosp Data        -----
load("./Data/single_year_2003-2017/hosp.Rdata")
table(hosp$State, hosp$year)

hosp.clean <- unique(hosp[, c("State", "year", "hosp")])
table(hosp.clean$State, hosp.clean$year)
HOSP <- state.regression(hosp.clean, lm.var="hosp")


# Nursfac Data     -----
load("./Data/single_year_2003-2017/nursfac.Rdata")
# Nursefac seems good to go. (Year 2005 isn't repeated)
table(nursfac$State, nursfac$year)
nursfac$nursfac.pc <- nursfac$nursfac/nursfac$popsize
NURSFAC <- state.regression(nursfac, lm.var="nursfac.pc", rename="nursfac")
####-----> driver.pop dataset -----
# Nursfac dataset comes with it's own popsize data that appears complete across
# all years/states. We'll use that for these datasets when needed even though
# there was an updated version of popsize created in the beginning of this
# document.
driver.pop <- as.data.frame(nursfac[, c("State", "year", "popsize")])

# CMS Data         ----
load("./Data/single_year_2003-2017/cms.Rdata")
table(cms$State, cms$year)
# cms data seems to have same problem as hosp with data from 2005 being repeated.
cms.clean <- unique(cms[, c("State", "year", "mcare", "mcaid")])
table(cms.clean$State, cms.clean$year)
# We'll use the population sizes from the nursfac dataset
cms.clean.pop <- merge(cms.clean, driver.pop)
cms.clean.pop$mcare.pc <- cms.clean.pop$mcare/cms.clean.pop$popsize
cms.clean.pop$mcaid.pc <- cms.clean.pop$mcaid/cms.clean.pop$popsize
CMS.mcare <- state.regression(cms.clean.pop, lm.var="mcare.pc", rename="mcare")
CMS.mcaid <- state.regression(cms.clean.pop, lm.var="mcaid.pc", rename="mcaid")


# Haustock Data    ----
load("./Data/single_year_2003-2017/haustock.Rdata")
# Check for State/year wackiness:
table(haustock$State, haustock$year)
HAUSTOCK <- state.regression(haustock, lm.var="units_after2000", rename="haustock")


# Taxrates Data    ------
load("./Data/taxrates.Rdata")
table(taxrates$State, taxrates$year)
TAXRATES.low20 <- state.regression(taxrates, lm.var="low20")
TAXRATES.top15 <- state.regression(taxrates, lm.var="top15")

# Credit Data      ------
load("./Data/credit.Rdata")
table(credit$state, credit$year)
credit$rating <- NA
credit$rating[credit$credit=="BBB"] <- 1
credit$rating[credit$credit=="A-"]  <- 2
credit$rating[credit$credit=="A"]   <- 3
credit$rating[credit$credit=="A+"]  <- 4
credit$rating[credit$credit=="AA-"] <- 5
credit$rating[credit$credit=="AA"]  <- 6
credit$rating[credit$credit=="AA+"] <- 7
credit$rating[credit$credit=="AAA"] <- 8
    # The equivalent to "zero change" for SD would just be 7s:
    credit[credit$state == "South Dakota",]
    sum(is.na(credit$rating))
    credit$rating[is.na(credit$rating)] <- 7
    credit[credit$state == "South Dakota",]
# Adding Null Washington D.C.
    unique(credit$state) # yep...no DC
    dc <- data.frame(state = "District of Columbia",
                     year = c(2004, 2005, 2014:2016),
                     credit = NA,
                     rating = 0)
    credit.dc <- rbind(credit, dc)
CREDIT <- state.regression(credit.dc, lm.var="rating", rename="credit")
# Check that rating.slope for DC is 0 and slope for SD is close to 0+:
CREDIT[CREDIT$State %in% c("District of Columbia", "South Dakota"),]


# Snap Data      ------
load("./Data/snap.Rdata")
table(snap$State, snap$Year)
SNAP <- state.regression(snap, lm.var="i_benefit", rename="snap")





# Duals Data     ------
load("./Data/duals.Rdata")
table(duals$State, duals$year)
duals$duals.pc <- duals$duals/duals$popsize
DUALS <- state.regression(duals, lm.var="duals.pc", rename="duals")


# Seniors Data      ------
load("./Data/seniors.Rdata")
table(seniors$State, seniors$year)
HOUSING <- state.regression(seniors, lm.var="pct_age62plus", rename="housing")




# Eduspend Data      -------
load("./Data/eduspend.Rdata")
table(eduspend$State, eduspend$year)
EDUSPEND <- state.regression(eduspend, lm.var="eduspend")


# Docs Data      -------
load("./Data/docs.Rdata")
table(docs$State, docs$year)
# no data for OR for 2004 and 2005?
docs$ndocs.pc <- docs$ndocs/docs$popsize
NDOCS <- state.regression(docs, lm.var="ndocs.pc", rename="ndocs")

# Partisan Data     ------
# Partisan (NB: Nebraska and D.C. set to no change since missing)
partisan<-read.csv("./Data/Partisan_2003-2019.csv")
partisan.long <- melt(partisan, id.vars="State", measure.vars=paste0("X",2003:2019))
partisan.long$year <- as.numeric(substring(partisan.long$variable, 2))
PARTISAN <- state.regression(partisan.long, lm.var="value", rename="partisan")



# EITC Data       ------
eitc2004<-read.csv("./state_eitc_2004.csv")
eitc2015<-read.csv("./state_eitc_2015.csv")
eitc.raw<-read.csv("./Data/single_year_2003-2017/eitc/state_eitc_rates_LONG.csv")
state.year <- data.frame(State = unique(docs$State),
                         Year = rep(2003:2016, each = length(unique(docs$State))))
eitc <- merge(state.year, eitc.raw, all.x = T)
eitc$Credit[is.na(eitc$Credit)] <- 0 # 0 eitc if you're not in eitc raw list.

# Test original data with updated data:
test2004 <- merge(eitc2004, eitc[eitc$Year == 2004,])
all(round(test2004$Credit, 2) == test2004$EITCT1)
test2004[which(round(test2004$Credit, 2) != test2004$EITCT1), ]
#
test2015 <- merge(eitc2015, eitc[eitc$Year == 2015,])
all(round(test2015$Credit, 2) == test2015$EITCT2)
test2015[which(round(test2015$Credit, 2) != test2015$EITCT2), ]

EITC <- state.regression(eitc, lm.var="Credit", rename="eitc")


# Census of Spending Data --------
load("./Data/census.Rdata")
table(census$state, census$year)
cen <- census[, c("state", "year", "popsize", "pubw_s", "police_s", 
                  "libs_s", "transit_s", "prec_s", "natr_s", "retire_s")]
cen$public  <-    cen$pubw_s/cen$popsize
cen$police  <-  cen$police_s/cen$popsize
cen$libs    <-    cen$libs_s/cen$popsize
cen$transit <- cen$transit_s/cen$popsize; cen$transit[is.na(cen$transit)] <- 0
cen$prec    <-    cen$prec_s/cen$popsize
cen$natr    <-    cen$natr_s/cen$popsize
cen$retire  <-  cen$retire_s/cen$popsize
CENSUS.public  <- state.regression(cen, lm.var="public")
CENSUS.police  <- state.regression(cen, lm.var="police")
CENSUS.libs    <- state.regression(cen, lm.var="libs")
CENSUS.transit <- state.regression(cen, lm.var="transit")
CENSUS.prec    <- state.regression(cen, lm.var="prec")
CENSUS.natr    <- state.regression(cen, lm.var="natr")
CENSUS.retire  <- state.regression(cen, lm.var="retire")


##############################################################################################
#               Taking Stock of Where We Are                                                 #
##############################################################################################
# At this point we have the following datasets with regression slopes
# calculated from the following years:

# hosp                |    03  04  05  06  07  08  09  10  11  12  13  14  15  16
# nursfac             |    03  04  05  06  07  08  09  10  11  12  13  14  15  16
# cms                 |    03  04  05  06  07  08  09  10  11  12  13  14   .   .
# haustock            |     .   .  05   .   .   .   .  10  11  12  13  14  15  16
# taxrates            | 02  .   .   .   .  07   .   .   .   .   .  13   .  15   .
# credit              |     .  04  05   .   .   .   .   .   .   .   .  14  15  16
# snap                |    03  04  05   .   .   .   .   .   .   .   .  14  15  16
# duals               |     .   .   .  06   .   .   .   .   .   .   .  14  15  16
# seniors (housing)   |     .  04  05   .   .   .   .   .   .   .   .  14  15  16
# eduspend            |    03  04  05   .   .   .   .   .   .   .   .  14  15  16
# docs                |    03  04  05   .   .   .   .   .   .   .   .  14  15  16
# partisan            |    03  04  05  06  07  08  09  10  11  12  13  14  15  16  [17  18  19]  <- unused
# eitc                |    03  04  05  06  07  08  09  10  11  12  13  14  15  16  
# census o' spending  |     .  04  05   .   .   .   .   .   .   .   .  14  15   .    
#############################################################################################

# merge all drivers together
drivers.all <- list(HOSP, NURSFAC, CMS.mcare, CMS.mcaid, HAUSTOCK, TAXRATES.low20,
                    TAXRATES.top15, CREDIT, SNAP, DUALS,
                    HOUSING, EDUSPEND, NDOCS, PARTISAN, EITC, CENSUS.public,
                    CENSUS.police, CENSUS.libs, CENSUS.transit, CENSUS.prec,
                    CENSUS.natr, CENSUS.retire)

drivers.sub <- lapply(drivers.all, function(x) {
  names(x) <- gsub("\\.slope\\.scaled", "", names(x))
  y <- x[, -grep("slope", names(x))]
  y})

drivers <- Reduce(function(...) merge(...), drivers.sub)

# #############################################################################################
# ###STATE DRIVERS ANALYSIS####################################################################
# #############################################################################################

##########################################################################
##########################################################################
# Updated figure S11. (renamed ranks) On Feb 8 2021, request for
# Figure S11 (above) be redone to edit group names:
var.nms <- c("hosp", "nursfac", "mcare", "mcaid", 
             "haustock", "low20", "top15", 
             "credit", "snap", "duals", "housing", 
             "eduspend", "ndocs", "partisan", "eitc", 
             "public", "police", "libs",
             "transit", "prec", "natr", 
             "retire")
var.labs <- c("Hospitals", "Nursing facilites", "Medicare spending", "Medicaid spending",
              "New housing", "Proportion taxes paid by bottom", "Proportion taxes paid by top",
              "Credit rating", "SNAP spending", "Dual eligible", "Low income housing",
              "Education spending", "Physicians", "Partisan", "Earned Income Tax Credit rate",
              "Public welfare spending", "Police spending", "Library spending",
              "Transit spending", "Parks spending", "Natural res. spending",
              "Retirement spending")
s11.dat <- drivers[,var.nms]
names(s11.dat) <- var.labs
# Specifying width here is a bit tricky since it leaves enough horizontal space
# to accommodate longest variable name. So I fiddled with width until the width of the 
# actual figure was about 6.5".
pdf(file = "~/Desktop/s10.pdf", width = 7)
corrplot(cor(s11.dat),type="upper", order="hclust",
         tl.cex=0.7, cl.cex=0.7)
dev.off()
# to crop the extra whitespace, use pdfcrop function from mactex distribution
# note: you need mactex for this:
system("pdfcrop ~/Desktop/s10.pdf ~/Desktop/s10crop.pdf")

##
# this figure was renamed to s10 in final paper:
bw.corr(s11.dat, "s10_bw", ncex = 0.5)
##########################################################################
##########################################################################



# ###Figure 6: regression models of change, overall and for each domain
state65R.slopes <- data.frame(State = names(sr.ols.list),
                              stateChange = unlist(lapply(sr.ols.list, slope.extract)))
dim(drivers)
drivers65R <- merge(drivers, state65R.slopes)
dim(drivers65R)

d <- list()
d.preds <- setdiff(names(drivers65R), c("State", "stateChange"))
for (i in d.preds) {
  # note: In the above GLM models, there is no mcare or eitc. 
  # these were added back in for this analysis.
  .f <- reformulate(i, "stateChange")
  d[[i]] <- glm(.f, data=drivers65R)
}


# #coefficients
e.cci.list <- lapply(d, function(x) {
  e.coef <- x$coef[2]
  e.LL <- confint(x, level=(0.95))[2,1]
  e.UL <- confint(x, level=(0.95))[2,2]
  data.frame(e.coef=e.coef, e.LL=e.LL, e.UL=e.UL)
})
e.cci <- do.call(rbind, e.cci.list)

predictorNames <- row.names(e.cci)
predictorNames <- gsub("hosp",     "Hospitals", predictorNames)
predictorNames <- gsub("nursfac",  "Nursing facilities", predictorNames)
predictorNames <- gsub("mcaid",    "Medicaid spending", predictorNames)
predictorNames <- gsub("mcare",    "Medicare spending", predictorNames)
predictorNames <- gsub("haustock", "New housing", predictorNames)
predictorNames <- gsub("low20",    "Proportion taxes paid bottom", predictorNames)
predictorNames <- gsub("top15",    "Proportion taxes paid by top", predictorNames)
predictorNames <- gsub("credit",   "Credit rating", predictorNames)
predictorNames <- gsub("snap",     "SNAP spending", predictorNames)
predictorNames <- gsub("duals",    "Dual eligible", predictorNames)
predictorNames <- gsub("^housing", "Low income housing", predictorNames)
predictorNames <- gsub("eduspend", "Education spending", predictorNames)
predictorNames <- gsub("eitc",     "Earned Income Tax Credit rate", predictorNames)
predictorNames <- gsub("ndocs",    "Physicians", predictorNames)
predictorNames <- gsub("partisan", "Partisan", predictorNames)
predictorNames <- gsub("public",   "Public welfare spending", predictorNames)
predictorNames <- gsub("police",   "Police spending", predictorNames)
predictorNames <- gsub("libs",     "Library spending", predictorNames)
predictorNames <- gsub("transit",  "Transit spending", predictorNames)
predictorNames <- gsub("prec",     "Parks spending", predictorNames)
predictorNames <- gsub("natr",     "Natural res. spending", predictorNames)
predictorNames <- gsub("retire",   "Retirement spending", predictorNames)


ggplot(e.cci, aes(x = predictorNames, y = e.coef, ymin = e.LL, ymax = e.UL))+
  geom_pointrange(size=0.3) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip()+
  scale_y_continuous(name = "Change in Rank Percentile with 1 SD change in Driver") +
  scale_x_discrete(name = "Drivers") +
  theme(axis.text.y  = element_text(color = "black"),
        axis.text.x  = element_text(color = "black"),
        axis.title = element_text(size = 8),
        plot.title = element_text(size=10))
ggsave("~/Desktop/figure8.pdf", height = 5.5, width=5)
system("pdfcrop ~/Desktop/figure8.pdf ~/Desktop/figure8crop.pdf")