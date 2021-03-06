#Chesapeake bay water quality attainment indicator

library(dplyr)
library(tidyr)


raw.dir <- here::here("data-raw")

ches_bay_wq_csv<-"Attainment_indicator.csv"

get_ches_bay_wq <- function(save_clean =F){
  ches_bay_wq <- read.csv(file.path(raw.dir, ches_bay_wq_csv)) %>%
  dplyr::select(Time = Year.1, Value = Total) %>%
  dplyr::mutate(Var = "chesapeake bay water quality attainment",
           Units = "estimated attainment, percent",
           EPU = "MAB")

  if (save_clean){
    usethis::use_data(ches_bay_wq, overwrite = T)
  } else {
    return(ches_bay_wq)
  }
  # metadata ----
  attr(ches_bay_wq, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/chesapeake-bay-water-quality-standards-attainment.html"
  attr(ches_bay_wq, "data_files")   <- list(
    ches_bay_wq_csv = ches_bay_wq_csv)
  attr(ches_bay_wq, "data_steward") <- c(
    "Qian Zhang <qian.zhang@chesapeakebay.net>")
}

get_ches_bay_wq(save_clean = T)
