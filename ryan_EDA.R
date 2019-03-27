library(data.table)
library(readxl)
library(tidyverse)
library(tigris)
library(ggmap)
library(sf)
library(sp)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(Hmisc)
library(htmltools)
library(tidycensus)

# census api key
readRenviron("~/.Renviron")

# Set the tigris_use_cache option
options(tigris_use_cache = TRUE)

# Get data from tigris as simple features
options(tigris_class = "sf")

# set the working directory to where the scripts are
root_dir <- "/media/sf_VBox_Shared_Folder/DHS/ryan/"
data_dir <- "/media/sf_VBox_Shared_Folder/DHS/ryan/data/"

setwd(root_dir)

# read in the floods data. The xlsx file is put in a data directory under the current working directory
floods <- readxl::read_xlsx(paste0(data_dir, "/FL_Midwest_US_ListofAffectedAreas_25Mar2019_v1.xlsx")
                            , sheet = "US")

# convert to data.table and do some processing
floods <- as.data.table(floods)

# create new duplicate column to do the merge with the zctas data
floods[, ZCTA5CE10 := `ZIP Code`]

# get the zctas data using tigris
zdata  <- zctas(cb = TRUE, state = floods[, unique(State)], class = "sf")

# make sure data is of sf class
class(zdata)

# some data processing
zdata_2 <- zdata %>% mutate(ZCTA5CE10 = as.integer(ZCTA5CE10))

# subset the zctas data to only include areas within our flood data
zdata_3 <- zdata_2[zdata_2$ZCTA5CE10 %in% floods$`ZIP Code`, ]
# zdata_3 <- left_join(zdata_2, floods, by = c("ZCTA5CE10" = "ZIP Code"))

# download the latest AHPS file from NOAA:
download.file(url = "https://water.weather.gov/ahps/download.php?data=tgz_obs"
              , destfile = paste0(data_dir, "/AHPS.tgz"))
# get the shape file data
# untar the file to a directory
untar(tarfile = paste0(data_dir, "/AHPS.tgz")
      , exdir = paste0(data_dir, "/AHPS"))

# read the directory using ogr
ahps_dt <- readOGR(paste0(data_dir, "/AHPS"))

# make sure the object is an sf object
ahps_dt <- st_as_sf(ahps_dt)

# convert object crs to match that of the zdata object
st_crs(ahps_dt) <- st_crs(zdata_3)

# subset the ahps_dt file we just downloaded to only include areas within our zdata_3 object
ahps_dt_subset <- ahps_dt[zdata_3, ]

# now let's download the population data for our areas
# to do that we will use the tidycensus library
# we have to download the data for all zctas in the US
zpop_data <- get_acs(geography = "zcta"
                     , variables = c(population = "B01003_001"),
                     geometry = TRUE)

# extract areas in the zpop_data data that are in the zdata_3 object
zpop_data_sub <- zpop_data[zpop_data$GEOID %in% zdata_3$GEOID10, ]

# make some quick leaflet maps
factPal <- colorFactor(palette = "viridis"
                       , levels = ahps_dt_subset$Status)
ryan_map <- zdata_3 %>% 
  leaflet() %>% 
  addTiles() %>% 
  # addPolygons(color = "gray"
  #             , weight = "1"
  #             , fillColor="red"
  #             , fillOpacity = 0.25) %>%
  addPolygons(data = zpop_data_sub
              , color = "gray"
              , weight = "1"
              , fillColor="red"
              , fillOpacity = 0.25) %>%
  addCircleMarkers(data = ahps_dt_subset
             , radius = 2
             , popup = paste0('<strong>', "Waterbody: ", ahps_dt_subset$Waterbody, '</strong>'
                              , '<br/>'
                              , "ZCTA Population: ", zpop_data_sub$estimate
                              , '<br/>'
                              ,"Status: ", ahps_dt_subset$Status
                              , '<br/>'
                              , "Observation Time: ", ahps_dt_subset$ObsTime
                              , '<br/>'
                              , "Observed: ", ahps_dt_subset$Observed
                              , '<br/>'
                              , paste0('<a href=', ahps_dt_subset$URL,'>NOAA URL</a>'))
             , color = factPal(ahps_dt_subset$Status)
             , group = "gauges"
  ) %>% 
  addResetMapButton() %>% 
  addLegend(pal = factPal
            , values = ahps_dt_subset$Status
            , position = "bottomleft"
            , opacity = 1
            , title = "Status")

ryan_map <- ryan_map %>%
  addSearchFeatures(targetGroups = "gauges"
                    , options = searchFeaturesOptions(
                      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )
                    )


# ryan_map %>% fitBounds(lng1 = -100.5, lat1 = 40.111, lng2 = -93.97, lat2 = 44.3)
ryan_map
