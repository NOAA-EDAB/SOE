#### Chesapeake bay salinity

library(tidyverse)
library(readr)

raw.dir <- here::here("data-raw")

ch_bay_temp_csv<-"GR_WaterTemp_Data.csv"
get_ch_bay_temp <- function(save_clean = F){

  ch_bay_temp<-read_csv(file.path(raw.dir,ch_bay_temp_csv), col_names = FALSE) %>%
    janitor::row_to_names(1) %>%
    dplyr::rename("Year2020" = "2020 Daily",
                  "YearLTA" = "2010-2019 avg",
                  "minLTA" = "2010-2019 min",
                  "maxLTA" = "2010-2019 max") %>%
    tidyr::pivot_longer(c("Year2020", "YearLTA", "minLTA", "maxLTA"), names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Units = c("C"),
                  Value = as.numeric(Value))  %>%
    dplyr::rename(Time = Date) %>%
    tidyr::separate(Time, c("Time", "Trash"), sep = " ") %>%
    dplyr::select(-Trash) %>%
    dplyr::mutate(Time = lubridate::mdy(Time))


  # metadata ----
  attr(ch_bay_temp, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chesapeake-bay-salinity.html"
  attr(ch_bay_temp, "data_files")   <- list(
    ch_bay_temp_csv = ch_bay_temp_csv)
  attr(ch_bay_temp, "data_steward") <- c(
    "Charles Pellerin <charles.pellerin@noaa.gov>",
    "Bruce Vogt <bruce.vogt@noaa.gov")

  if (save_clean){
    usethis::use_data(ch_bay_temp, overwrite = T)
  } else {
    return(ch_bay_temp)
  }
}
get_ch_bay_temp(save_clean = T)
