# the function uses add_vis_wm to add windmill info to a bunch of trans (trans_chunk)
add_vis_wm_to_trans <- function(trans_chunk,i,x,grid_buffer_50km,grid_overlayed,windmills_sf){
  
  ## Start adding the windmill visibility variables, including:
  ## the number of windmills visible to the property when the transaction happens, their case ids
  ## the total number of windmill visible to the property up to date, and their case ids
  
  # overlay the transaction in the chunk with buffered grids to know what grids s to load
  grid_trans_chunk <- st_intersects(grid_buffer_50km, trans_chunk)
  grid_overlayed_chunk <- seq_len(nrow(grid_buffer_50km))[
    lapply(grid_trans_chunk, length) %>% unlist() >0
  ]
  
  print(paste0("state ",i, " chunk ",seq_len(length(grid_overlayed))[which(grid_overlayed==x)],
               ": ",length(grid_overlayed_chunk)," grid shapefiles to load  at ",Sys.time()))
  
  # find the year range
  year_range <- unique(year(trans_chunk$DocumentDate)) %>% na.omit()
  
  # return NA if there is no viewsheds
  if(length(grid_overlayed_chunk)==0 | length(year_range)==0){
    rm(grid_overlayed_chunk,trans_chunk)
    return(NULL)
  } 
  
  
  # read in viewsheds of all windmills
  # add bounding box to each viewshed polygon
  setwd(viewshed_folder)
  viewshed_all <- lapply(grid_overlayed_chunk, function(xx){
    count <- st_layers(paste0("viewshed_grid",xx,".shp"), do_count = TRUE)[["features"]]
    if(count>1000){
      plan(multisession, 
           workers = min(ceiling(count/100000),8), 
           gc = T)
      sp <- future_lapply(seq(0, count, by = 1000), function(offset){
        if(offset==count){return(NULL)}
        query <- paste0("SELECT * FROM ", paste0("viewshed_grid",xx)," LIMIT ", as.integer(1000), " OFFSET ", as.integer(offset))
        sp_offset <- st_read(paste0(viewshed_folder,"viewshed_grid",xx,".shp"),
                             query = query,
                             type = 3,
                             quiet = T
        )
        return(sp_offset)
      })%>%
        rbindlist() %>%
        st_as_sf()
      plan(sequential)
      names(sp)[names(sp)=="_ogr_geometry_"] = "geometry"
      st_geometry(sp) = "geometry"
      sp_box <- sp %>%
        mutate(bbox = map(geometry, st_bbox))
      sp <- cbind(sp, setDT(transpose(sp_box$bbox))) 
      names(sp)[2:5] <- c( "xmin","ymin","xmax","ymax" )
    }else{
      sp <- st_read(paste0(viewshed_folder,"viewshed_grid",xx,".shp"),
                    type = 3,
                    quiet = T
      )
      sp_box <- sp %>%
        mutate(bbox = map(geometry, st_bbox))
      sp <- cbind(sp, setDT(transpose(sp_box$bbox))) 
    }
    return(sp) 
  })
  plan(sequential)
  viewshed_box <- viewshed_all %>% 
    rbindlist()  %>%
    st_as_sf() 
  rm(grid_overlayed_chunk, grid_trans_chunk,viewshed_all)
  gc()
  
  # subset to windmills whose 10km buffer overlays the transaction in chunk
  windmills_chunk <- subset(windmills_sf, case_id %in% viewshed_box$case_id)
  
  
  # split the transaction into a list splitted by 10km-by-10km cells
  # chunk_grids_geom <- st_make_grid(trans_chunk, cellsize = c(10000,10000)) 
  # chunk_grids <- st_sf(geom = chunk_grids_geom)
  trans_chunk <- as.data.table(trans_chunk)
  # trans_chunk[, grid:= st_intersects(st_as_sf(trans_chunk),chunk_grids) %>% unlist()]
  trans_chunk[, grid:=year(as.Date(DocumentDate))]
  trans_chunk_list <- split(trans_chunk,by=c("grid"))
  # rm(chunk_grids_geom,chunk_grids)
  print(paste0("start adding viewsheds to ", nrow(trans_chunk)," transactions at ",Sys.time()))
  
  
  p <- progressor(along=as.numeric(names(trans_chunk_list)))
  
  wm_vis_list <- lapply(trans_chunk_list, add_vis_wm,
                        viewshed_box = viewshed_box,
                        windmills_chunk = windmills_chunk,
                        p=p)
  
  plan(sequential)
  trans_vis_wm <- rbindlist(wm_vis_list) %>% as.data.table()
  rm(wm_vis_list)
  print(paste0("Finish adding viewsheds to ", nrow(trans_chunk)," transactions at ",Sys.time()))
  rm(viewshed_box,windmills_chunk)
  
  gc()
  return(trans_vis_wm)
}

