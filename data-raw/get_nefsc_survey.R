 #Set up for finding survey proportions

#This script was adapted from code written by Sean Lucey at the NEFSC. Note that you
#will need the latest survdat file to get this to run successfully.

data.dir <- here::here("data-raw")
#-------------------------------------------------------------------------
#Required packages
#devtools::install_github("slucey/RSurvey/Survdat")

library(data.table); library(rgdal); library(Survdat)
library(dplyr);library(sf);library(tidyr)
#-------------------------------------------------------------------------------
nefsc_survey_rdata <- 'Survdat.RData'
#Load raw data and get "strata" aka EPUs here
load(file.path(data.dir, nefsc_survey_rdata ))

strata <- ecodata::epu_sf %>% as("Spatial")

#Generate area table
strat.area <- Survdat::getarea(strata, 'EPU')

#Post stratify data
survdat.EPU <- Survdat::poststrat(survdat, strata, 'EPU')
setnames(survdat.EPU, 'newstrata', 'EPU')

#Subset by season/ strata set
fall   <- survdat.EPU[SEASON == 'FALL',   ]
spring <- survdat.EPU[SEASON == 'SPRING', ]

#Run stratification prep
fall.prep   <- Survdat::stratprep(fall,   strat.area, strat.col = 'EPU', area.col = 'Area')
spring.prep <- Survdat::stratprep(spring, strat.area, strat.col = 'EPU', area.col = 'Area')

#Calculate mean weight/tow by aggregate groups
#n tows
n.tows.fall  <- unique(fall.prep[,   list(YEAR, EPU, ntows)])
n.tows.spring <- unique(spring.prep[, list(YEAR, EPU, ntows)])

#drop length data
data.table::setkey(fall.prep, YEAR, EPU, STATION, STRATUM, SVSPP, CATCHSEX)
fall.prep <- unique(fall.prep, by = key(fall.prep))
fall.prep[, c('LENGTH', 'NUMLEN') := NULL]

data.table::setkey(spring.prep, YEAR, EPU, STATION, STRATUM, SVSPP, CATCHSEX)
spring.prep <- unique(spring.prep, by = key(spring.prep))
spring.prep[, c('LENGTH', 'NUMLEN') := NULL]

#Merge Sexed species
data.table::setkey(fall.prep, YEAR, EPU, STATION, STRATUM, SVSPP)
fall.prep <- fall.prep[, sum(BIOMASS, na.rm = T), by = key(fall.prep)]

data.table::setkey(spring.prep, YEAR, EPU, STATION, STRATUM, SVSPP)
spring.prep <- spring.prep[, sum(BIOMASS, na.rm = T), by = key(spring.prep)]

#Sum biomass within an EPU
fall.sum   <- fall.prep[,   sum(V1), by = c('YEAR', 'EPU', 'SVSPP')]
spring.sum <- spring.prep[, sum(V1), by = c('YEAR', 'EPU', 'SVSPP')]

#Merge sum with station count
fall.sum   <- merge(fall.sum,   n.tows.fall, by = c('YEAR', 'EPU'))
spring.sum <- merge(spring.sum, n.tows.spring, by = c('YEAR', 'EPU'))

#Calculate mean weight per tow
fall.sum[, kg.per.tow := V1 / ntows]
fall.mean <- fall.sum[, list(YEAR, EPU, SVSPP, kg.per.tow)]

spring.sum[, kg.per.tow := V1 / ntows]
spring.mean <- spring.sum[, list(YEAR, EPU, SVSPP, kg.per.tow)]

#get species groupings
groups <- ecodata::species_groupings %>%
  dplyr::select(group = SOE_18, SVSPP,
                comname = COMNAME) %>%
  dplyr::filter(!is.na(SVSPP)) %>%
  as.data.table()

#Aggregate by conceptual model groupings
fall   <- merge(fall.mean,   unique(groups[, list(SVSPP, group, comname)]),
                by = 'SVSPP', all.x = T)
spring <- merge(spring.mean, unique(groups[, list(SVSPP, group, comname)]),
                by = 'SVSPP', all.x = T)

#Fix NA group to other
fall[  is.na(group), group := 'Other']
spring[is.na(group), group := 'Other']

#Before aggregating, save data with species ids
spring$season <- "spring"
fall$season <- "fall"

survey_biomass <- rbind(spring, fall) %>%
  dplyr::filter(!is.na(SVSPP))

managed <- ecodata::species_groupings %>%
  dplyr::select(comname = COMNAME, Fed_Managed)

nefsc_survey_disaggregated <-
  survey_biomass %>%
  dplyr::group_by(EPU, group, season, YEAR) %>%
  dplyr::mutate(Total = sum(kg.per.tow)) %>%
  dplyr::mutate(Prop = kg.per.tow/Total) %>%
  dplyr::filter(!group %in% c("Apex Predator","Other"),
         EPU != "SS") %>%
  dplyr::rename(Time = YEAR) %>%
  as.data.frame() %>%
  dplyr::left_join(.,managed,by = c("comname") ) %>%
  dplyr::distinct() %>%
  tidyr::complete(Time = full_seq(min(.$Time):max(.$Time),1),
           tidyr::nesting(EPU, Fed_Managed,season, group, comname, SVSPP)) %>%
  dplyr::rename(Management = Fed_Managed,
                `Feeding guild` = group,
                Season = season,
                Proportion = Prop)

usethis::use_data(nefsc_survey_disaggregated, overwrite = TRUE)

## GET AGGREGATED SURVEY DATA--------------------------------------------------------

fall.agg   <- fall[,   sum(kg.per.tow), by = c('YEAR', 'EPU', 'group')]
spring.agg <- spring[, sum(kg.per.tow), by = c('YEAR', 'EPU', 'group')]

# #Total
fall.agg[, Total := sum(V1), by = c('YEAR', 'EPU', 'group')]
spring.agg[, Total := sum(V1), by = c('YEAR', 'EPU', 'group')]

#Get in correct long format for SOE
#By feeding guild
fall.tot <- copy(fall.agg)
fall.tot[, Var := paste(group, 'Fall Biomass Index')]
setnames(fall.tot, c('YEAR', 'EPU', 'V1'), c('Time', 'Region', 'Value'))
fall.tot[, Units  := 'kg tow^-1']
fall.tot <- unique(fall.tot)
fall.tot <- mutate(fall.tot, Season = "Fall")

spring.tot <- copy(spring.agg)
spring.tot[, Var := paste(group, 'Spring Biomass Index')]
setnames(spring.tot, c('YEAR', 'EPU', 'V1'), c('Time', 'Region', 'Value'))
spring.tot[, Units  := 'kg tow^-1']
spring.tot <- unique(spring.tot)
spring.tot <- mutate(spring.tot, Season = "Spring")

agg_survey <- rbind(spring.tot, fall.tot)

#Merge into one data set
nefsc_survey <- agg_survey %>%
  dplyr::select(Time, EPU = Region, Var, Units, Value)  %>%
  dplyr::distinct() %>%
  tidyr::complete(Time = full_seq(min(.$Time):max(.$Time),1),
           tidyr::nesting(EPU,Var))


# metadata ----
attr(nefsc_survey, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/survdat.html"
attr(nefsc_survey, "data_files")   <- list(
  nefsc_survey_rdata = nefsc_survey_rdata)
attr(nefsc_survey, "data_steward") <- c(
  "Sean Lucey <sean.lucey@noaa.gov>")
usethis::use_data(nefsc_survey, overwrite = T)
