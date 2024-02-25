# ========================================================================
# Last Update: 2024/02
# Project: Windmills and House Transactions
# Aim: Find the viewsheds of each windmill
# Note: This program has to start in OSGEO4W Shell following below steps:
#        1 Start OSGEO4W Shell
#        2 Type "cd C:\Program Files\RStudio\bin", then Enter
#        3 Type "RStudio", then it goes to the RStudio session
# Author: Wei Guo (wei.guo@cmcc.it)
# ========================================================================

rm(list=ls())

# devtools::install_github("rsbivand/rgrass7")
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("BiocParallel")
# install.packages("rgdal", repos="http://R-Forge.R-project.org")
library(sp)
library(raster)
library(scales)
library(rasterVis)
library(ellipsis)
library(Rcpp)
library(sf)
library(rgl)
library(rgdal)
library(rgrass7)
library(link2GI)
library(mapview)
library(tictoc)
library(dplyr)
library(stars)
library(data.table)
library(future)
library(future.apply)
library(rgeos)
library(doFuture)
library(BiocParallel)
library(foreach)


# find the current directory
PATH <<- dirname(dirname(rstudioapi::getSourceEditorContext()$path))


# define the working folders
code_location <<- paste0(PATH, "/program/")
data_folder <<- paste0(PATH, "/data/")
dems_folder <<- paste0(data_folder,"dems/")
grass_home <<- paste0(data_folder)
windmill_folder <<- paste0(data_folder,"/windmills/")
# assign a temporary folder which need to have more than 50 gigabyte free storage
temp_folfer <<- tempdir()
# define the output folder
viewshed_folder <<- paste0(data_folder,"/viewshed_grid/")

# # find the directory where GRASS is installed
grass_installation = findGRASS() 
# grass_installation <<- "C:/OSGeo4W64/apps/grass/grass78"

options("rgdal_show_exportToProj4_warnings"="none") 


# define the useful CRS codes
epsg4326 <<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
epsg3857 <<- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"


# load initial setting of GRASS
setwd(code_location)
source(file.path(getwd(), 'initialize_rgrass7-setup-win-osgeo4w.R'))

# load the viewshed calculation function
setwd(code_location)
source(file.path(getwd(), 'viewshed_grass.R'))

# ========================================================================
# Step 0 (RUN ONLY ONCE!): prepare the dems and windmill data in GRASS

# 0.1 load the dems data to GRASS location "/grass_dem_unproj4326"

# set directory
grass_location <- "grass_dem_unproj4326"
grass_mapset <- "PERMANENT"

# initialize GRASS
initGRASS(gisBase = grass_installation,
          home = grass_home, 
          gisDbase = grass_home,
          location = grass_location,
          mapset = grass_mapset,
          override=TRUE
)

# check proj is XY location (unprojected)
execGRASS("g.proj", flags = "p")

# read the dems data
setwd(data_folder)
demlist <- list.files(path = dems_folder, pattern = ".tif$",full.names = T)
demlist_s <- list.files(path = dems_folder, pattern = ".tif$",full.names = F)

# load the dem rasters in GRASS
lapply(seq_len(length(demlist)), function(i)
  execGRASS("r.in.gdal", 
            flags = "overwrite",
            input = demlist[i],
            output = gsub(".tif", "", demlist_s[i])
  )
)
rm(demlist)

# set region to match the boundary of the dem rasters
execGRASS("g.region",
          flags = c("s", "overwrite"),
          raster = paste(gsub(".tif", "@PERMANENT", demlist_s), sep=",")
)

# save the region to a vector location_usa
execGRASS("v.in.region", 
          flags = "overwrite",
          output = "location_usa"
)




# 0.2 read the windmill shapefile in GRASS location "/windmill_4326"

# set directory
grass_location <- "windmill_4326"
grass_mapset <- "PERMANENT"

# initialize GRASS
initGRASS(gisBase = grass_installation,
          home = grass_home, 
          gisDbase = grass_home,
          location = grass_location,
          mapset = grass_mapset,
          override=TRUE
)

# check if the projection system is XY (unprojected)
execGRASS("g.proj", flags = "p")

# find and load the windmill shapefile
windmilllist <- list.files(path = windmill_folder, pattern = ".shp$",full.names = T)
windmilllist_s <- list.files(path = windmill_folder, pattern = ".shp$",full.names = F)
execGRASS("v.in.ogr",
          flags = "overwrite",
          input = windmilllist,
          layer = gsub(".shp", "", windmilllist_s)
)
rm(windmilllist)


# set region to match the boundary of the dem rasters
execGRASS("g.region",
          flags = c("s", "overwrite"),
          vector = gsub(".shp", "@PERMANENT", windmilllist_s)
)
 
# save the region to a vector windmill_location_usa
execGRASS("v.in.region", 
          flags = "overwrite",
          output = "windmill_location_usa"
)


# 0.3 reproject to epsg::3857 in GRASS, save under location "/grass_3857"

# set directory
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


# check proj is XY location (unprojected)
execGRASS("g.proj", flags = "p")


# reproject the dems box
execGRASS("v.proj",
          flags = "overwrite",
          location = "grass_dem_unproj4326",
          mapset = "PERMANENT",
          input = "location_usa"
)

# set the reprojected dems box as the current region, set resolution as 90m for efficiency
execGRASS("g.region",
          flags = c("s", "overwrite"),
          vector = "location_usa@PERMANENT",
          res = 90
)

# reproject the dem rasters
lapply(seq_len(length(demlist_s)), function(i)
  execGRASS("r.proj",
            flags = "overwrite",
            location = "grass_dem_unproj4326",
            mapset = "PERMANENT",
            input = gsub(".tif", "", demlist_s[i]),
            method = "lanczos",
            memory = 50000, # allow the memory usage up to 50G
            resolution = 90
  )
)

# patch the dem rasters
execGRASS("r.patch",
          flags = "overwrite",
          input = paste0(gsub(".tif", "@PERMANENT", demlist_s), sep=","),
          output = "usa_patched"
)


# reproject the windmill box
execGRASS("v.proj",
          flags = "overwrite",
          location = "windmill_4326",
          mapset = "PERMANENT",
          input = "windmill_location_usa"
)

# set the reprojected box as the current region
execGRASS("g.region",
          flags = c("s", "overwrite"),
          vector = "windmill_location_usa@PERMANENT",
          res = 90
)


# reproject the windmill data
execGRASS("v.proj",
          flags = "overwrite",
          location = "windmill_4326",
          mapset = "PERMANENT",
          input = gsub(".shp", "", windmilllist_s)
)

# set the region to match the boundary of the dem rasters
execGRASS("g.region",
          flags = c("s", "overwrite"),
          raster = paste(gsub(".tif", "@PERMANENT", demlist_s), sep=",")
)

# save the region to vector location_usa_patched
execGRASS("v.in.region", 
          flags = "overwrite",
          output = "location_usa_patched"
)

# remove the windmill data outside the dems boundary (lat > 60 degree)
execGRASS("v.clip",
          flags = c("r","overwrite"),
          input = gsub(".shp", "@PERMANENT", windmilllist_s),
          clip = "location_usa_patched@PERMANENT",
          output = "windmills_in_dems"
)



# ========================================================================
# Step 1: prepare the dems and windmill data in GRASS

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


# check proj is epsg:3857
execGRASS("g.proj", flags = "p")

# set the region to match the patched dem
execGRASS("g.region",
          flags = c("overwrite", "quiet"),
          vector = "location_usa_patched@PERMANENT"
)


# # run the grid generation only once!
# # make square grids of 100km * 100km
# execGRASS("v.mkgrid",
#           flags = c("overwrite"),
#           map = "grid_100km",
#           box = c(100000,100000)
#           )
# 
# 
# # calculate the count of windmills in each grid
# execGRASS("v.vect.stats",
#           points = "windmills_in_dems@PERMANENT",
#           areas = "grid_100km@PERMANENT",
#           count_column = "count"
#           )

# export the grid vector to R and keep only those with any windmills inside
use_sf()
grid_100km <- readVECT("grid_100km")
grid_100km <- subset(grid_100km, count > 0)

# find the minimum height of all windmills
windmilllist_s <- list.files(path = windmill_folder, pattern = ".shp$",full.names = T)
windmill_minhh <- read_sf(windmilllist_s, quiet = T) %>% 
  subset(t_hh>0) %>% 
  as.data.frame() %>%
  select(t_hh) %>%
  unlist() %>%
  as.numeric() %>%
  min(na.rm=T)

gc()

# loop over the grids
for(i in seq_len(nrow(grid_100km))){

  tic()
  
  # move the grid to GRASS
  writeVECT(grid_100km[i,], "itm_grid",
            v.in.ogr_flags=c("overwrite", "quiet"))
  
  # find windmills inside the grid
  execGRASS("v.select",
            flags = c("overwrite", "quiet"),
            ainput = "windmills_in_dems@PERMANENT",
            atype = "point",
            binput = "itm_grid@PERMANENT",
            btype = "line,area",
            output = "itm_windmills",
            operator = "intersects")
  
  
  # export the inside windmills
  use_sf()
  itm_windmills <- readVECT("itm_windmills")
  
  
  # generate a 100km buffer around the grid
  execGRASS("v.buffer",
            flags = c("s", "overwrite", "quiet"),
            input = "itm_grid@PERMANENT",
            output = "itm_buffer",
            distance = 100000
  )
  
  
  # set the 100km buffer as region
  execGRASS("g.region",
            flags = c("overwrite", "quiet"),
            vector = "itm_buffer"
  )
  
  
  
  # start paralleling with n multisessions
  nparal <- 28 # the number of workers is n(tasks)/2 up to 28
  # do sequential only if there is one inside windmill
  if(nrow(itm_windmills)==1){ plan(sequential) } else{
    registerDoFuture()
    plan(multisession, workers = min(floor(nrow(itm_windmills)), nparal))
    register(DoparParam(stop.on.error=FALSE), default = TRUE)
  }
  
  # run the viewshed calculation function
  viewshed_index <- future_lapply(seq_len(nrow(itm_windmills)), 
                                  viewshed_grass,
                                  windmill_sf = itm_windmills, 
                                  min_height = windmill_minhh
  )


  
  # combine the viewsheds in the grid
  itm_viewshed_list <- future_lapply(itm_windmills$case_id, function(j)
    read_stars(paste0(viewshed_folder,"viewshed_case",j,".tif")) %>%
      st_as_sf(as_points = F, merge = T) %>%
      select(geometry) %>%
      mutate(case_id = j,
             geometry = st_cast(geometry, "POLYGON")) %>%
      st_as_sf()
  )
  itm_viewshed <- rbindlist(itm_viewshed_list) %>%
    st_as_sf()
  
  
  # unlink the viewshed tifs
  do.call(unlink, list(
    list.files(paste0(viewshed_folder),
               pattern = ".tif$",
               full.names = TRUE)
  ))
  do.call(unlink, list(
    list.files(paste0(viewshed_folder),
               pattern = ".xml$",
               full.names = TRUE)
  ))
  
  
  
  # return to serial sequence
  plan(sequential)
  
  # write the viewshed to a shapefile
  st_write(itm_viewshed, paste0(viewshed_folder,"viewshed_grid",i,".shp"), append = F)
  
  
  execGRASS("g.remove", 
            flags=c("f","quiet"),
            type = "raster",
            pattern = "viewshed*")
  
  # remove all intermediate outputs
  do.call(unlink, list(
    list.files(paste0(grass_home, grass_location,"/PERMANENT/.tmp/unknown/"), full.names = TRUE)
  ))
  gc()
  
  # print the grid index and the count of windmills inside
  print(
    paste0(i, ": ", nrow(itm_windmills), ", ", nrow(itm_viewshed) )
  )
  remove(itm_viewshed, itm_windmills, itm_viewshed_list, v_list, viewshed_index)
  toc()
  
  
  # delete temp files
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
}
