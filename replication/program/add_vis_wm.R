# the function to add info of visible windmills to one transaction (of index in trans_chunk)
add_vis_wm <-  function(trans,viewshed_box,windmills_chunk,p){
  
  Sys.sleep(0.1)
  p(sprintf("x=%g", year(trans$`DocumentDate`)[1]))
  
  trans <- st_as_sf(trans)
  
  # subset to viewshed_boxs whose bounding box contains the trans_chunk
  viewshed_box_x <- viewshed_box %>%
    subset( xmin <= st_bbox(trans)[3] &
              xmax >= st_bbox(trans)[1] & 
              ymin <= st_bbox(trans)[4] & 
              ymax >= st_bbox(trans)[2])  
  
  #create an unique id in trans
  trans <- as.data.table(trans)
  trans[, id:=1:.N]
  
  ########################################################################
  # For visible operational windmills_chunks in the trans_chunk year
  
  # find operational windmills_chunks in the trans_chunk year
  viewshed_box_t <- viewshed_box_x %>%
    subset(case_id %in% subset(windmills_chunk, p_year<=year(trans$`DocumentDate`)[1])$case_id) %>%
    st_as_sf()
  
  # find operational windmills_chunks in the trans_chunk year
  if(nrow(viewshed_box_t)==0){
    flag = 0
  }else{
    
    flag = 1
    
    # find the intersect index between trans and viewshed_box_t
    trans[, int := c( sf::st_intersects( geometry, viewshed_box_t ))]
    
    # split the transactions that intersect with multiple polygons into multiple rows
    trans_split <- trans[ , list( int = unlist(int) ) , by = list(id) ]
    trans[,int:=NULL]
    setkey(trans_split,"id")
    setkey(trans,"id")
    trans_wm <- merge(trans_split,trans,all.x=T)
    trans_wm <- trans_wm %>%
      st_as_sf() %>%
      st_transform(epsg102005) %>%
      as.data.table()
    
    # check if any transaction intersect with viewshed polygon
    if(nrow(trans_wm)==0){ 
      
      flag = 0
      rm(trans_split, trans_wm)
      
    }else{
      
      # add case id
      trans_wm[,case := viewshed_box_t$case_id[int]]
      trans_wm[, int := NULL]
      trans_wm <- unique(trans_wm, by=c("case","id"))
      # add windmill location
      windmills_sub <- setDT(windmills_chunk)[case_id%in%trans_wm$case,
                                              list(case = case_id,
                                                   wm_geom = geometry)]
      setkey(trans_wm,"case")
      setkey(windmills_sub,"case")
      trans_wm <- merge(trans_wm,windmills_sub,by="case",all.x=T)
      # add distance to windmill
      trans_wm_1 <- as.data.table(st_coordinates(trans_wm$geometry))
      trans_wm_2 <- as.data.table(st_coordinates(trans_wm$wm_geom))
      names(trans_wm_2) <- c("X_wm","Y_wm")
      trans_wm <- cbind(trans_wm,trans_wm_1,trans_wm_2)
      rm(trans_wm_1,trans_wm_2)
      trans_wm[,dist := sqrt((X-X_wm)^2+(Y-Y_wm)^2)]
      # find the closest windmill for each tran
      trans_dist <- setDT(trans_wm)[, .SD[which.min(dist)], by=list(id)]
      trans_dist <- setDT(trans_dist)[,
                                      list(id = id, dist_vis_wm = dist, case_dist_vis_wm = case  )]
      # find the number of wms in visibility
      trans_n <- setDT(trans_wm)[, .(n_vis_wm = uniqueN(case),
                                     case_vis_wm = paste(unique(case), collapse = ", ")), by = list(id)]
      
      # add the windmills_chunk visibility info to trans_sf
      trans_t <- trans %>%
        select(-geometry,-grid) %>%
        as.data.table()
      setkey(trans_t,"id")
      setkey(trans_n,"id")
      setkey(trans_dist,"id")
      trans_t <- merge(trans_t, trans_n, all.x=T)
      trans_t <- merge(trans_t, trans_dist, all.x=T)
      
      rm(trans_dist, trans_n, windmills_sub, trans_wm, trans_split)
    }
    
  }
  
  if(flag==0){
    trans_t <- trans %>%
      as.data.frame() %>%
      select(-geometry, -grid) %>%
      mutate(
        n_vis_wm = NA,
        case_vis_wm = NA,
        dist_vis_wm =  NA, 
        case_dist_vis_wm = NA
      ) %>%
      as.data.table()
  }
  
  ########################################################################
  # For all visible windmills_chunks  regardless of operational status   
  
  if(nrow(viewshed_box_x)==0){
    flag = 0
  }else{
    
    flag = 1
    
    # find the intersect index between trans and viewshed_box
    trans[, int := c( sf::st_intersects( geometry, viewshed_box_x ))]
    
    # split the transactions that intersect with multiple polygons into multiple rows
    trans_split <- trans[ , list( int = unlist(int) ) , by = list(id) ]
    trans[,int:=NULL]
    setkey(trans_split,"id")
    setkey(trans,"id")
    trans_wm <- merge(trans_split,trans,all.x=T)
    trans_wm <- trans_wm %>%
      st_as_sf() %>%
      st_transform(epsg102005) %>%
      as.data.table()
    
    # check if any trans intersect with windmill buffers
    if(nrow(trans_wm)==0){
      flag=0
      rm(trans_wm,trans_split)
    }else{
      # add case id
      trans_wm[,case := viewshed_box_x$case_id[int]]
      trans_wm[, int := NULL]
      trans_wm <- unique(trans_wm, by=c("case","id"))
      # add windmill location
      windmills_sub <- setDT(windmills_chunk)[case_id%in%trans_wm$case,
                                              list(case = case_id,
                                                   wm_geom = geometry)]
      setkey(trans_wm,"case")
      setkey(windmills_sub,"case")
      trans_wm <- merge(trans_wm,windmills_sub,by="case",all.x=T)
      
      # add distance to windmill
      trans_wm_1 <- as.data.table(st_coordinates(trans_wm$geometry))
      trans_wm_2 <- as.data.table(st_coordinates(trans_wm$wm_geom))
      names(trans_wm_2) <- c("X_wm","Y_wm")
      trans_wm <- cbind(trans_wm,trans_wm_1,trans_wm_2)
      rm(trans_wm_1,trans_wm_2)
      trans_wm[,dist := sqrt((X-X_wm)^2+(Y-Y_wm)^2)]
      # find the closest windmill for each tran
      trans_dist <- setDT(trans_wm)[, .SD[which.min(dist)], by=list(id)]
      trans_dist <- setDT(trans_dist)[,
                                      list(id = id, dist_vis_wm_tot = dist, case_dist_vis_wm_tot = case  )]
      # find the number of wms in visibility
      trans_n <- setDT(trans_wm)[, .(n_vis_wm_tot = uniqueN(case),
                                     case_vis_wm_tot = paste(unique(case), collapse = ", ")), 
                                 by = list(id)]
      
      
      # add the windmills_chunk visibility info to trans_sf
      setkey(trans_t,"id")
      setkey(trans_n,"id")
      setkey(trans_dist,"id")
      trans_t <- merge(trans_t, trans_n, all.x=T)
      trans_t <- merge(trans_t, trans_dist, all.x=T)
      
      rm(trans_dist, trans_n, windmills_sub, trans_wm, trans_split)
    }
    
  } 
  if(flag==0){
    # add the windmills_chunk visibility info to trans_sf
    trans_t <- trans_t %>%
      as.data.frame() %>%
      mutate(
        n_vis_wm_tot = NA,
        case_vis_wm_tot = NA,
        dist_vis_wm_tot =  NA, 
        case_dist_vis_wm_tot = NA
      ) %>%
      as.data.table()
  }
  
  rm(trans, viewshed_box_x, viewshed_box_t,flag)
  # return the trans_chunks with windmills_chunk visibility info
  trans_t[,id:=NULL]
  return(trans_t)
}


