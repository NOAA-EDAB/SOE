# Processing for Coast file

# Coast df from rnaturalearth and rnaturalearthhires
#

library(dplyr)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthhires)

raw.dir <- here::here("data-raw")

get_coast <- function(save_clean = F){

  #CRS
  crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  #Coastline shapefile
  coast <- rnaturalearth::ne_countries(scale = 10,
                        continent = "North America",
                        returnclass = "sf") %>%
    sf::st_transform(crs = crs)

  if (save_clean){
    usethis::use_data(coast, overwrite = T)
  } else {
    return(coast)
  }
}
get_coast(save_clean = T)
