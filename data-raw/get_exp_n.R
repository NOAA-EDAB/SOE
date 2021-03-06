

library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
exp_n_rdata <- "Expected_number_of_species_21 (1).RData"
get_exp_n <- function(save_clean = F){

  load(file.path(raw.dir, exp_n_rdata))



  exp_n2<-ESn.epu %>% filter(str_detect(Var,  'Species - Bigelow|Species - Albatross')) %>%
    tidyr::separate(Var, c("Season", "EPU", "trash1","trash2", "trash3", "trash4", "Var")) %>%
    dplyr::select(-trash1, -trash2, -trash3, -trash4)

  exp_n<-ESn.epu %>% filter(str_detect(Var,  'Standard Deviation')) %>%
    tidyr::separate(Var, c("Season", "EPU", "trash1", "trash2", "trash3","trash4",
                           "trash5", "trash6","Var")) %>%
    dplyr::mutate(Var=recode(Var,
                             `Albatross`="AlbatrossSD",
                             `Bigelow`="BigelowSD")) %>%
    dplyr::select(-trash1, -trash2, -trash3, -trash4, -trash5, -trash6) %>%
    rbind(exp_n2)



  if (save_clean){
    usethis::use_data(exp_n, overwrite = T)
  } else {
    return(exp_n)
  }
  # metadata ----
  attr(exp_n, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc"
  attr(exp_n, "data_files")   <- list(
    exp_n_rdata = exp_n_rdata)
  attr(exp_n, "data_steward") <- c(
    "Sean Lucey <sean.lucey@noaa.gov>")
}
get_exp_n(save_clean = T)
