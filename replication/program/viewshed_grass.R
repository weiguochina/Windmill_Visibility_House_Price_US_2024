# function for the viewshed calculation in GRASS
viewshed_grass <- function(j, windmill_sf, min_height){
  
  j_windmill <- windmill_sf[j,]
  case <- j_windmill$case_id %>% as.character()
  
  if(is.na(j_windmill$t_hh) | j_windmill$t_hh <= 0){
    execGRASS("r.viewshed", 
              flags=c("c","r","b","overwrite", "quiet"),
              input = "usa_patched@PERMANENT",
              output = paste0("viewshed",j),
              coordinates = st_coordinates(j_windmill),
              observer_elevation = min_height,
              target_elevation = 1.75,# assume the observer's (property's) height is 1.75m
              max_distance = 10000, # assume the maximum visibility distance is 10km
              memory = 5000 # allow memory usage for up to 5Gb 
    )
  }else{
    execGRASS("r.viewshed", 
              flags=c("c","r","b","overwrite", "quiet"),
              input = "usa_patched@PERMANENT",
              output = paste0("viewshed",j),
              coordinates = st_coordinates(j_windmill),
              observer_elevation = j_windmill$t_hh,
              target_elevation = 1.75,# assume the observer's (property's) height is 1.75m
              max_distance = 10000, # assume the maximum visibility distance is 10km
              memory = 5000 # allow memory usage for up to 5Gb 
    )
  }
  
  
  # keep only visible rasters
  execGRASS("r.null",
            map = paste0("viewshed",j,"@PERMANENT"),
            setnull = "0"
  )
  
  # transform the viewshed raster to vector
  execGRASS("r.out.gdal",
            flags = c("overwrite","f"),
            input = paste0("viewshed",j,"@PERMANENT"),
            output = paste0(viewshed_folder,"viewshed_case",case,".tif"),
            format = "GTiff"
  )
  
  return(case)
}

