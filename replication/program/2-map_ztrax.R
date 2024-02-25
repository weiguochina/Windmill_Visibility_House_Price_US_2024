# ========================================================================
# Last Update: 2024/02
# Project: Windmills and House Transactions
# Aim: Generate the final data with measures on windmill visibility
# Note: This progrem has to start in OSGEO4W Shell following below steps:
#        1 Start OSGEO4W Shell
#        2 Type "cd C:\Program Files\RStudio\bin", then Enter
#        3 Type "RStudio", then it goes to the RStudio session
# Author: Wei Guo (wei.guo@cmcc.it)
# ========================================================================

rm(list=ls())


library(sp)
library(raster)
library(scales)
library(Rcpp)
library(sf)
library(rgl)
library(rgdal)
library(rgrass7)
library(link2GI)
library(tictoc)
library(dplyr)
library(stars)
library(data.table)
library(rgeos)
library(fst)
library(geojsonsf)
library(haven)
library(progressr)
library(purrr)
library(tidyverse)
library(future)
library(future.apply)



# find current directory
PATH <<- dirname(dirname(rstudioapi::getSourceEditorContext()$path))


# define the working folders
code_location <<- paste0(PATH, "/program/")
data_folder <<- paste0(PATH, "/data/")
dems_folder <<- paste0(data_folder,"dems/")
grass_home <<- paste0(data_folder)
viewshed_folder <<- paste0(data_folder,"/viewshed_grid/")
windmill_folder <<- paste0(data_folder,"/windmills/")
ztrax_folder <<- paste0(data_folder,"/ztrax/")

# define the output folder
output_folder <<- paste0(data_folder,"/final_data/")

# find the place where GRASS is installed
grass_installation = findGRASS() 

# minimize the warning display
options("rgdal_show_exportToProj4_warnings"="none") 


# set handlers setting
handlers(handler_progress(format="[:bar] :percent :eta :message"), show_after = 0.5)
handlers(global = T)


# define the CRS codes
epsg4326 <<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
epsg3857 <<- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
epsg102005 <<- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs"

# load initial setting of GRASS
setwd(code_location)
source(file.path(getwd(), 'initialize_rgrass7-setup-win-osgeo4w.R'))

# load core programs
setwd(code_location)
source(file.path(getwd(), 'add_vis_wm.R'))
source(file.path(getwd(), 'add_nearby_wm.R'))
source(file.path(getwd(), 'add_vis_wm_to_trans.R'))
source(file.path(getwd(), 'add_nearby_wm_to_trans.R'))
source(file.path(getwd(), 'ztrax_windmill_map.R'))


# set the GRASS directory
grass_location <- "grass_3857"
grass_mapset <- "PERMANENT"

# initialize GRASS
initGRASS(gisBase = grass_installation,
          home = grass_home, 
          gisDbase = grass_home,
          location = grass_location,
          mapset = grass_mapset,
          override=TRUE
)


# check if the proj is epsg:3857
execGRASS("g.proj", flags = "p")

# set the region to match the patched dems
execGRASS("g.region",
          flags = c("overwrite", "quiet"),
          vector = "location_usa_patched@PERMANENT"
)


# export the grid vector to R and keep only those with windmills inside
setwd(viewshed_folder)
use_sf()
grid_100km <- readVECT("grid_100km")
grid_100km <- subset(grid_100km, count > 0)


# find the 50km buffers of all grids
grid_buffer_50km <- grid_100km %>% 
  st_transform(epsg102005) %>%
  st_buffer(50000) %>%
  st_transform(epsg3857)


# read the windmill data
setwd(windmill_folder)
windmills_sf <- read_sf("uswtdb_v4_1.shp") %>%
  st_transform(crs = epsg3857)
windmills <- windmills_sf %>%  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  as.data.table()
windmills_out <- windmills_sf %>%
  st_transform(epsg4326) %>%
  dplyr::select(-xlong, -ylat)
windmills_out <- windmills_out %>%
  mutate(X = unlist(map(windmills_out$geometry,1)),
         Y = unlist(map(windmills_out$geometry,2)))
windmills_out <- windmills_out %>%  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  as.data.table()
write_dta(windmills_out, paste0(windmill_folder,"windmills.dta"))
setkey(windmills, "case_id")



# load the state fips of which NAL house transactions are available
state_pads <- unique(gsub("^(.*)_ztrax.fst", "\\1", 
                          list.files(ztrax_folder))) %>%
  as.numeric() %>% na.omit()
state_pads <- state_pads[state_pads>0]


# define useful columns to read
cols <- c("SalesPriceAmount",
          "DocumentDate",
          "LoanAmount",
          "ImportParcelID",
          "TransId",
          "PropertyFullStreetAddress",
          "X", "Y",
          "STATEFP", "COUNTYFP", "TRACTCE", "GEOID","ZCTA5CE10",
          "TaxAmount", "TaxYear",
          "NoOfBuildings",
          "LotSizeAcres",
          "LotSizeSquareFeet",
          "LotSizeFrontageFeet",
          "LotSizeDepthFeet",
          "NoOfUnits",
          "PropertyLandUseStndCode",
          "BuildingOrImprovementNumber",
          "YearBuilt", "YearRemodeled",
          "NoOfStories", "TotalRooms", "TotalBedrooms", 
          "TotalKitchens", "TotalCalculatedBathCount") 


# run the core functions for all states
lapply(state_pads, ztrax_windmill_map, cols = cols, max_vis = 10000)

