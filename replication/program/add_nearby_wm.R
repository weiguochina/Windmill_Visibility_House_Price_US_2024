# the function to add info of nearby windmillsto one transaction (of index in trans_chunk)
add_nearby_wm <-  function(trans,windmills_sf,windmill_buffer,p){
  
  Sys.sleep(0.1)
  p(sprintf("x=%g", year(trans$`DocumentDate`)[1]))
  
  trans <- st_as_sf(trans) %>%
    st_transform(epsg102005)
  
  # subset to windmills whose bounding box contains the trans_chunk
  windmill_buffer_x <- windmill_buffer %>%
    subset( xmin <= st_bbox(trans)[3] &
              xmax >= st_bbox(trans)[1] & 
              ymin <= st_bbox(trans)[4] & 
              ymax >= st_bbox(trans)[2])  
  windmills_sf_x <- windmills_sf %>%
    subset( xmin <= st_bbox(trans)[3] &
              xmax >= st_bbox(trans)[1] & 
              ymin <= st_bbox(trans)[4] & 
              ymax >= st_bbox(trans)[2])  
  
  #create an unique id in trans
  trans <- as.data.table(trans)
  trans[, id:=1:.N]
  
  ########################################################################
  # For nearby operational windmills in the trans_chunk year
  
  # find operational windmills_chunks in the trans_chunk year
  windmill_buffer_t <- windmill_buffer_x %>%
    subset(p_year<=year(trans$`DocumentDate`)[1]) %>%
    st_as_sf()
  
  # find operational windmills_chunks in the trans_chunk year
  if(nrow(windmill_buffer_t)==0){
    flag = 0
  }else{
    
    flag = 1
    
    # find the intersect index between trans and windmill_buffer_t
    trans[, int := c( sf::st_intersects( geometry, windmill_buffer_t ))]
    
    # split the transactions that intersect with multiple polygons into multiple rows
    trans_split <- trans[ , list( int = unlist(int) ) , by = list(id) ]
    trans[,int:=NULL]
    setkey(trans_split,"id")
    setkey(trans,"id")
    trans_wm <- merge(trans_split,trans,all.x=T)
    
    # check if any transaction intersect with viewshed polygon
    if(nrow(trans_wm)==0){ 
      
      flag = 0
      rm(trans_split, trans_wm)
      
    }else{
      
      
      # add case id
      trans_wm[,case := windmill_buffer_t$case_id[int]]
      trans_wm[, int := NULL]
      trans_wm <- unique(trans_wm, by=c("case","id"))
      
      # add windmill location
      windmills_sub <- setDT(windmills_sf_x)[case_id%in%trans_wm$case,
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
                                      list(id = id, dist_prxm_wm = dist, case_dist_prxm_wm = case  )]
      # find the number of wms
      trans_n <- setDT(trans_wm)[, .(n_prxm_wm = uniqueN(case),
                                     case_prxm_wm = paste(unique(case), collapse = ", ")), by = list(id)]
      
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
    # add the windmills_sf visibility info to trans_sf
    trans_t <- trans %>%
      as.data.frame() %>%
      select(-geometry, -grid) %>%
      mutate(
        n_prxm_wm = NA,
        case_prxm_wm = NA,
        dist_prxm_wm =  NA, 
        case_dist_prxm_wm = NA
      ) %>%
      as.data.table()
    
  }
  
  
  ########################################################################
  # For nearby operational windmills in total
  
  if(nrow(windmill_buffer_x)==0){
    flag = 0
  }else{
    
    flag = 1
    
    # find the intersect index between trans and windmill_buffer_t
    trans[, int := c( sf::st_intersects( geometry, windmill_buffer_x ))]
    
    # split the transactions that intersect with multiple polygons into multiple rows
    trans_split <- trans[ , list( int = unlist(int) ) , by = list(id) ]
    trans[,int:=NULL]
    setkey(trans_split,"id")
    setkey(trans,"id")
    trans_wm <- merge(trans_split,trans,all.x=T)
    
    # check if any transaction intersect with viewshed polygon
    if(nrow(trans_wm)==0){ 
      
      flag = 0
      rm(trans_split, trans_wm)
      
    }else{
      
      
      # add case id
      trans_wm[,case := windmill_buffer_x$case_id[int]]
      trans_wm[, int := NULL]
      trans_wm <- unique(trans_wm, by=c("case","id"))
      # add windmill location
      windmills_sub <- setDT(windmills_sf_x)[case_id%in%trans_wm$case,
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
                                      list(id = id, dist_prxm_wm_tot = dist, case_dist_prxm_wm_tot = case  )]
      # find the number of wms
      trans_n <- setDT(trans_wm)[, .(n_prxm_wm_tot = uniqueN(case),
                                     case_prxm_wm_tot = paste(unique(case), collapse = ", ")), 
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
    
    # add the windmills_sf nearby info to trans
    trans_t <- trans_t %>%
      as.data.frame() %>%
      mutate(
        n_prxm_wm_tot = NA,
        case_prxm_wm_tot = NA,
        dist_prxm_wm_tot =  NA, 
        case_dist_prxm_wm_tot = NA
      ) %>%
      as.data.table()
    
  }
  
  
  rm(dis_wm, intersect_dummy, windmills_sf_x, windmill_buffer_x)
  
  # return the trans_chunks with windmills_sf visibility info
  trans_t[,id:=NULL]
  return(trans_t)
}

