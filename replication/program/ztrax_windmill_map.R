# the function to add windmill visibility variables, using state fips as input
ztrax_windmill_map <- function(i, cols, max_vis ){
  
  # read in the nal transaction data of state i
  setwd(ztrax_folder)
  naltrans <- read_fst( paste0(i,"_ztrax.fst"),
                        columns = cols,
                        as.data.table = T)
  
  # convert the coordinates into spatial
  trans_sf <- naltrans[,list(X,Y,TransId,ImportParcelID,DocumentDate)] %>%
    subset(!is.na(X) & Y>0 & !is.na(DocumentDate)) %>%
    st_as_sf(coords = c("X","Y"),
             crs = epsg4326) %>%
    st_transform(crs = epsg3857)
  
  rm(naltrans)
  gc()
  
  # find the area and bounding box of areas within max_vis distance to each windmill
  windmills_sf <- st_transform(windmills_sf,epsg102005)
  windmill_buffer <- st_buffer(windmills_sf, max_vis )%>% 
    mutate(bbox = map(geometry, st_bbox))
  windmill_buffer <- cbind(windmill_buffer, setDT(transpose(windmill_buffer$bbox))) 
  windmills_sf <- cbind(windmills_sf, setDT(transpose(windmill_buffer$bbox))) 
  
  # group transactions into chunks by the grids, so in each chunk there would read in no more than 5 grid shapefiles
  grid_trans <- st_intersects(grid_100km, trans_sf)
  grid_overlayed <- seq_len(nrow(grid_100km))[
    lapply(grid_trans, length) %>% unlist() >0
  ]
  
  print(paste0("state ",i," has ", length(unique(unlist(grid_trans))), " in total " ,nrow(trans_sf),
               " transactions possibly visible to windmills, split into ", length(grid_overlayed), " chunks (grids) at ",Sys.time()))
  
  # define intermediate data directory
  subDir <-  paste0(data_folder,"final_data/",i,"/")
  if (!dir.exists(subDir)){
    dir.create(file.path(subDir))
  } 
  # find grids that have been completed
  finished_grid <- list.files(path = subDir,
                              pattern = ".fst",
                              full.names=FALSE) %>%
    gsub(pattern = ".fst", replacement = "") %>%
    as.numeric()
  # only work on the grids yet to be done
  grid_overlayed <- grid_overlayed[!grid_overlayed%in%finished_grid]
  
  lapply(rev(grid_overlayed), function(x){
    
    # find transactions in chunk x
    trans_chunk <- trans_sf[grid_trans[[x]],] 
    
    # add windmill info to trans_chunk
    trans_vis_wm <- add_vis_wm_to_trans(trans_chunk,
                                        i,x,grid_buffer_50km,grid_overlayed,windmills_sf)
    trans_prxm_wm <- add_nearby_wm_to_trans(trans_chunk,
                                            i,x,windmill_buffer,windmills_sf)
    
    # start merging the data
    setkeyv(trans_prxm_wm, c("TransId","ImportParcelID","DocumentDate"))
    setkeyv(trans_vis_wm, c("TransId","ImportParcelID","DocumentDate"))
    trans_prxm_wm <- trans_vis_wm[trans_prxm_wm]
    
    # save the intermediate output
    write_fst(trans_prxm_wm, paste(subDir,as.numeric(x),".fst"))
    rm(trans_chunk,trans_prxm_wm,trans_vis_wm)
    gc()  
    return(NULL)
  })
  
  print(paste0("state ",i," has done with all chunks at ",Sys.time()))
  plan(sequential)
  filenames <- list.files(path = subDir,
                          full.names = TRUE)  
  trans_wm <- lapply(filenames,function(x){ read_fst(x)}) %>%
    rbindlist()
  
  
  # read in the nal transaction data of state i
  setwd(ztrax_folder)
  naltrans <- read_fst( paste0(i,"_ztrax.fst"),
                        columns = cols,
                        as.data.table = T)
  
  # start merging the data all together
  setkeyv(naltrans, c("TransId","ImportParcelID","DocumentDate"))
  setkeyv(trans_wm, c("TransId","ImportParcelID","DocumentDate"))
  naltrans <- trans_wm[naltrans]
  
  
  # replace missing sales price with loan/.8
  naltrans[is.na(SalesPriceAmount) | SalesPriceAmount==0, SalesPriceAmount:=as.numeric(LoanAmount)/0.8]
  naltrans <- naltrans %>% subset(!is.na(X) & Y>0 ) 
  
  # export the final data
  write_fst(naltrans, paste0(data_folder,"trans_",i,".fst"))
  
  print(paste0("state ",i," all done at ", Sys.time()))
  
  rm(naltrans, trans_wm)
}
