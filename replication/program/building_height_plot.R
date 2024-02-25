# function to generate figure s1 (average building height by census block)
building_height_plot <- function(){
  
  setwd(buildheig_folder)
  
  bd_height <- read_sf(dsn = "srtm_bg_building_heights.gdb", layer = "srtm_bldg_heights_class") %>%
    st_transform(epsg4326) %>%
    st_make_valid() 
  bd_height <- bd_height %>%
    st_buffer(0) %>%
    st_crop(xmin=-130, xmax=-60, ymin=20, ymax=50) %>%
    st_transform(epsg3857)
  
  us_map <- states(year = 2010) %>%
    st_transform(epsg4326) %>%
    st_crop(xmin=-130, xmax=-60, ymin=20, ymax=50) %>%
    st_transform(epsg3857) %>%
    mutate(data = 0)
  
  bd_height <- as.data.table(bd_height)
  bd_height[, SEPH1:=SEPH]
  bd_height[SEPH1<1, SEPH1:=NA]
  bd_height <- st_as_sf(bd_height)
  bd_height1 <- bd_height %>%
    subset(!is.na(SEPH1))
  bd_height1$Height_cat <- factor(bd_height1$Height_cat, 
                                  levels = c("Low-medium","Medium" ,"Medium-High","High","Very high"  ),
                                  labels = c("2-3 stories", "3-4 stories", "3-6 stories", "4-9 stories", ">10 stories"))
  # generate spatial distribution of windmill visibility 
  figure <- ggplot() + 
    geom_sf(data = us_map, aes(NULL),fill= "grey88",  color="white", lwd=0.4) + 
    geom_sf(data = bd_height1, aes(fill = Height_cat), 
            colour = NA,  lwd = 0 , alpha = 1) +
    scale_fill_manual(name = "",
                      values = paletteer_c("grDevices::Zissou 1", 5)) +
    labs (title = "Average Building Heights by Block Group, 2000") + 
    theme(axis.line=element_blank(), 
          axis.text.x=element_blank(), axis.title.x=element_blank(),
          axis.text.y=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank(), 
          panel.background = element_blank(),
          legend.position = c(0.9,0.25),
          legend.direction = "vertical",
          legend.text = element_text(size=8),
          plot.title = element_text(hjust = 0.5, vjust = -1, size=11),
          plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0))
  ggsave(paste0(figure_folder,"figure_s1.png"),
         figure,
         scale=1,
         width=7.5, height=5)
}
