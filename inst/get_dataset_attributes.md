get\_dataset\_attributes
================

``` r
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  devtools, dplyr, here, knitr, purrr, tidyr)
load_all(here())

ds_ls <- data(package = "ecodata") %>% .$results %>% .[,3]
not_null <- function(x) !is.null(x)
datasets <- tibble(
  name = ds_ls) %>% 
  mutate(
    data = map(name, function(name){
      d <- try(get(name), silent = T)
      if ("try-error" %in% class(d))
        return(NA)
      d}),
    data_steward = map(data, function(data){
      attr(data, "data_steward")}),
    tech_doc_url = map(data, function(data){
      attr(data, "tech-doc_url")}),
    data_files = map(data, function(data){
      attr(data, "data_files") %>% 
        unlist()}),
    has_data = map_lgl(data, function(data){
      if (length(data) == 1 && is.na(data[[1]]))
        return(F)
      T}),
    has_steward = map_lgl(data_steward, not_null),
    has_doc     = map_lgl(tech_doc_url, not_null),
    has_files   = map_lgl(data_files  , not_null))

datasets_todo <- datasets %>% 
  select(name, starts_with("has")) %>% 
  rowwise() %>% 
  mutate(
    has_missing = any(!has_data, !has_steward, !has_doc, !has_files)) %>% 
  filter(has_missing) %>% 
  select(-has_missing)

datasets_done <- datasets %>% 
  anti_join(datasets_todo, by = "name") %>% 
  select(name, starts_with("has"))

dataset_stewards <- datasets %>% 
  select(name, data_steward) %>% 
  unnest(data_steward)

dataset_techdocurls <- datasets %>% 
  select(name, tech_doc_url) %>% 
  unnest(tech_doc_url)

dataset_datafiles <- datasets %>% 
  select(name, data_files) %>% 
  unnest(data_files)
```

## datasets

### done

``` r
kable(datasets_done)
```

| name                            | has\_data | has\_steward | has\_doc | has\_files |
| :------------------------------ | :-------- | :----------- | :------- | :--------- |
| aggregate\_biomass              | TRUE      | TRUE         | TRUE     | TRUE       |
| ch\_bay\_sal                    | TRUE      | TRUE         | TRUE     | TRUE       |
| chl\_pp                         | TRUE      | TRUE         | TRUE     | TRUE       |
| hp\_density                     | TRUE      | TRUE         | TRUE     | TRUE       |
| phyto\_size                     | TRUE      | TRUE         | TRUE     | TRUE       |
| productivity\_anomaly           | TRUE      | TRUE         | TRUE     | TRUE       |
| seasonal\_oisst\_anom           | TRUE      | TRUE         | TRUE     | TRUE       |
| seasonal\_sst\_anomaly\_gridded | TRUE      | TRUE         | TRUE     | TRUE       |

### todo

``` r
kable(datasets_todo)
```

| name                         | has\_data | has\_steward | has\_doc | has\_files |
| :--------------------------- | :-------- | :----------- | :------- | :--------- |
| aquaculture                  | TRUE      | FALSE        | FALSE    | FALSE      |
| bennet                       | TRUE      | FALSE        | FALSE    | FALSE      |
| blue\_runner                 | TRUE      | FALSE        | FALSE    | FALSE      |
| bottom\_temp\_glorys         | TRUE      | FALSE        | FALSE    | FALSE      |
| bottom\_temp                 | TRUE      | FALSE        | FALSE    | FALSE      |
| CalanusStage                 | TRUE      | FALSE        | FALSE    | FALSE      |
| ches\_bay\_wq                | TRUE      | FALSE        | FALSE    | FALSE      |
| coast                        | TRUE      | FALSE        | FALSE    | FALSE      |
| cold\_pool\_sf               | TRUE      | FALSE        | FALSE    | FALSE      |
| cold\_pool                   | TRUE      | FALSE        | FALSE    | FALSE      |
| comdat                       | TRUE      | FALSE        | FALSE    | FALSE      |
| commercial\_div              | TRUE      | FALSE        | FALSE    | FALSE      |
| energy\_density              | TRUE      | FALSE        | FALSE    | FALSE      |
| engagement                   | TRUE      | FALSE        | FALSE    | FALSE      |
| epu\_sf                      | TRUE      | FALSE        | FALSE    | FALSE      |
| forage\_anomaly              | TRUE      | FALSE        | FALSE    | FALSE      |
| grayseal                     | TRUE      | FALSE        | FALSE    | FALSE      |
| gsi\_old                     | TRUE      | FALSE        | FALSE    | FALSE      |
| gsi                          | TRUE      | FALSE        | FALSE    | FALSE      |
| habitat\_occupancy           | TRUE      | FALSE        | FALSE    | FALSE      |
| habitat\_vulnerability       | TRUE      | FALSE        | FALSE    | FALSE      |
| harborporpoise               | TRUE      | FALSE        | FALSE    | FALSE      |
| heatwave\_anom\_gridded      | TRUE      | FALSE        | FALSE    | FALSE      |
| heatwave\_peak\_date         | TRUE      | FALSE        | FALSE    | FALSE      |
| heatwave\_year               | TRUE      | FALSE        | FALSE    | FALSE      |
| heatwave                     | TRUE      | FALSE        | FALSE    | FALSE      |
| hms\_cpue                    | TRUE      | FALSE        | FALSE    | FALSE      |
| hms\_landings                | TRUE      | FALSE        | FALSE    | FALSE      |
| ichthyo\_diversity           | TRUE      | FALSE        | FALSE    | FALSE      |
| long\_term\_sst              | TRUE      | FALSE        | FALSE    | FALSE      |
| mab\_inshore\_survey         | TRUE      | FALSE        | FALSE    | FALSE      |
| mass\_inshore\_survey        | TRUE      | FALSE        | FALSE    | FALSE      |
| nao                          | TRUE      | FALSE        | FALSE    | FALSE      |
| narw                         | TRUE      | FALSE        | FALSE    | FALSE      |
| ne\_inshore\_survey\_species | TRUE      | FALSE        | FALSE    | FALSE      |
| ne\_inshore\_survey          | TRUE      | FALSE        | FALSE    | FALSE      |
| ne\_wind                     | TRUE      | FALSE        | FALSE    | FALSE      |
| nefsc\_survey\_disaggregated | TRUE      | FALSE        | FALSE    | FALSE      |
| nefsc\_survey                | TRUE      | FALSE        | FALSE    | FALSE      |
| new\_england                 | TRUE      | FALSE        | FALSE    | FALSE      |
| observed\_sharks             | TRUE      | FALSE        | FALSE    | FALSE      |
| ppr                          | TRUE      | FALSE        | FALSE    | FALSE      |
| rec\_hms                     | TRUE      | FALSE        | FALSE    | FALSE      |
| recdat                       | TRUE      | FALSE        | FALSE    | FALSE      |
| seabird\_MAB                 | FALSE     | FALSE        | FALSE    | FALSE      |
| seabird\_ne                  | TRUE      | FALSE        | FALSE    | FALSE      |
| slopewater                   | TRUE      | FALSE        | FALSE    | FALSE      |
| species\_dist                | TRUE      | FALSE        | FALSE    | FALSE      |
| species\_groupings           | TRUE      | FALSE        | FALSE    | FALSE      |
| stock\_status                | TRUE      | FALSE        | FALSE    | FALSE      |
| stom\_fullness               | TRUE      | FALSE        | FALSE    | FALSE      |
| stratification               | TRUE      | FALSE        | FALSE    | FALSE      |
| wcr                          | TRUE      | FALSE        | FALSE    | FALSE      |
| wind\_dev\_speed             | TRUE      | FALSE        | FALSE    | FALSE      |
| wind\_occupancy              | TRUE      | FALSE        | FALSE    | FALSE      |
| wind\_revenue                | TRUE      | FALSE        | FALSE    | FALSE      |
| zoo\_abund                   | TRUE      | FALSE        | FALSE    | FALSE      |
| zoo\_diversity               | TRUE      | FALSE        | FALSE    | FALSE      |
| zoo\_oi                      | TRUE      | FALSE        | FALSE    | FALSE      |
| zoo\_sli\_anom               | TRUE      | FALSE        | FALSE    | FALSE      |
| zoo\_strat\_abun             | TRUE      | FALSE        | FALSE    | FALSE      |

## attributes

### stewards

``` r
kable(dataset_stewards)
```

| name                            | data\_steward                                  |
| :------------------------------ | :--------------------------------------------- |
| aggregate\_biomass              | Sean Lucey <sean.lucey@noaa.gov>               |
| ch\_bay\_sal                    | Charles Pellerin <charles.pellerin@noaa.gov>   |
| ch\_bay\_sal                    | Bruce Vogt \<<bruce.vogt@noaa.gov>             |
| chl\_pp                         | Kimberly Hyde <kimberly.hyde@noaa.gov>         |
| hp\_density                     | Chris Orphanides <chris.orphanides@noaa.gov>   |
| phyto\_size                     | Kimberly Hyde <kimberly.hyde@noaa.gov>         |
| productivity\_anomaly           | Kimberly Bastille <kimberly.bastille@noaa.gov> |
| seasonal\_oisst\_anom           | Kimberly Bastille <kimberly.bastille@noaa.gov> |
| seasonal\_sst\_anomaly\_gridded | Kimberly Bastille <kimberly.bastille@noaa.gov> |

### docs

``` r
kable(dataset_techdocurls)
```

| name                            | tech\_doc\_url                                                          |
| :------------------------------ | :---------------------------------------------------------------------- |
| aggregate\_biomass              | <https://noaa-edab.github.io/tech-doc/aggroups.html>                    |
| ch\_bay\_sal                    | <https://noaa-edab.github.io/tech-doc/chesapeake-bay-salinity.html>     |
| chl\_pp                         | <https://noaa-edab.github.io/tech-doc/chl-pp.html>                      |
| hp\_density                     | <https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html>     |
| phyto\_size                     | <https://noaa-edab.github.io/tech-doc/chl-pp.html>                      |
| productivity\_anomaly           | <https://noaa-edab.github.io/tech-doc/fish-productivity-indicator.html> |
| seasonal\_oisst\_anom           | <https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html>      |
| seasonal\_sst\_anomaly\_gridded | <https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html>      |

### files

``` r
kable(dataset_datafiles)
```

| name                            | data\_files                                                                                     |
| :------------------------------ | :---------------------------------------------------------------------------------------------- |
| aggregate\_biomass              | Aggregate\_Survey\_biomass\_20 (5).RData                                                        |
| ch\_bay\_sal                    | SR\_Salinity.csv                                                                                |
| chl\_pp                         | 1998\_2020-OCCCI\_MODISA-CHLOR\_A-STATS\_ANOMS-NES\_EPU\_NOESTUARIES-SOE\_V2021-SOE\_FORMAT.csv |
| chl\_pp                         | 1998\_2020-OCCCI\_MODISA-PPD-STATS\_ANOMS-NES\_EPU\_NOESTUARIES-SOE\_V2021-SOE\_FORMAT.csv      |
| hp\_density                     | Harbor\_Porpoise - Fall AMAPPS mapping data from website.csv                                    |
| phyto\_size                     | 1998\_2020-OCCCI-PHYSIZE-STATS-NES\_EPU\_NOESTUARIES-V2021-SOE\_FORMAT.csv                      |
| productivity\_anomaly           | dat\_spec\_rec\_forSOE.Rdata                                                                    |
| productivity\_anomaly           | dat\_spec\_rec\_epu\_forSOE.Rdata                                                               |
| seasonal\_oisst\_anom           | sst.day.mean.ltm.1982-2010.nc                                                                   |
| seasonal\_sst\_anomaly\_gridded | sst.day.mean.2020.nc                                                                            |
| seasonal\_sst\_anomaly\_gridded | sst.day.mean.ltm.1982-2010.nc                                                                   |
