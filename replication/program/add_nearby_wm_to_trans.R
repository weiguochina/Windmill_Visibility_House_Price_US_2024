# the function uses add_nearby_wm to add windmill info a bunch of trans (trans_chunk)
add_nearby_wm_to_trans <- function(trans_chunk,i,x,windmill_buffer,windmills_sf){
  
  ## Start adding the windmill variables within 50km, including:
  ## the number of windmills visible to the property when the transaction happens, their case ids
  ## the total number of windmill visible to the property up to date, and their case ids
  # add windmill info of those within 50km regardless of transaction time
  
  # split the transaction into a list splitted by 10km-by-10km cells
  # chunk_grids_geom <- st_make_grid(trans_chunk, cellsize = c(10000,10000)) 
  # chunk_grids <- st_sf(geom = chunk_grids_geom)
  trans_chunk <- as.data.table(trans_chunk)
  # trans_chunk[, grid:= st_intersects(st_as_sf(trans_chunk),chunk_grids) %>% unlist()]
  trans_chunk[, grid:=year(as.Date(DocumentDate))]
  trans_chunk_list <- split(trans_chunk,by=c("grid"))
  
  
  p <- progressor(along=as.numeric(names(trans_chunk_list)))
  
  
  wm_prxm_list <- lapply(trans_chunk_list, add_nearby_wm, 
                         windmills_sf = windmills_sf,
                         windmill_buffer = windmill_buffer,
                         p=p)
  
  plan(sequential)
  trans_prxm_wm <- rbindlist(wm_prxm_list) %>% as.data.table()
  rm(wm_prxm_list)
  
  print(paste0("Finish adding nearby windmills to ", nrow(trans_chunk)," transactions at ",Sys.time()))
  return(trans_prxm_wm)
}

