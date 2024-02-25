# ========================================================================
# Last Update: 2023/03
# Project: Windmills and House Transactions
# Aim: Generate summary statistics and descriptive figures for wind turbines
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
library(tigris)
library(modelsummary)
library(paletteer)



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
output_folder <<- paste0(PATH, "/results/") 
dir.create(file.path(output_folder), showWarnings = FALSE)


# # find the place where GRASS is installed
grass_installation = findGRASS() 

options("rgdal_show_exportToProj4_warnings"="none") 

# define the useful crs codes
epsg4326 <<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
epsg3857 <<- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
epsg102005 <<- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs"


# read the windmill data
setwd(windmill_folder)
windmills_sf <- read_sf("uswtdb_v4_1_20210721.shp") %>%
  st_transform(crs = epsg3857)


# load US continental map
us_map <- states(year = 2010) %>%
  st_transform(epsg4326) %>%
  st_crop(xmin=-130, xmax=-60, ymin=20, ymax=50) %>%
  st_transform(epsg3857) 

# load urban areas
us_urban <- urban_areas() %>%
  st_transform(crs = epsg3857)
# find windmills in urban areas
windmills_sf <- windmills_sf %>%
  st_join(us_urban)
windmills <- windmills_sf %>%  as.data.frame() %>%
  select(-geometry) %>%
  as.data.table()
windmills[UATYP10%in%c("C","U"), urban:=T]
windmills[is.na(urban), urban:=F]
windmills <- windmills[p_year>0]
windmills[p_cap<0, p_cap:=NA]
windmills[t_cap<0, t_cap:=NA]
windmills[t_hh<0, t_hh:=NA]
windmills[t_rd<0, t_rd:=NA]
windmills[t_rsa<0, t_rsa:=NA]
windmills[t_ttlh<0, t_ttlh:=NA]



# print summary statistics of wind turbines (Table S4)
setwd(output_folder)
datasummary(p_year+p_tnum+p_cap+t_cap+t_hh+t_rd+t_rsa+t_ttlh+retrofit~
                N+Mean+SD+Max+Min,
              data = windmills,
              output = "table_s4.tex",
              title = 'Summary Statistics of Wind Turbines')




# load us map
us_map <- states(year = 2010) %>%
  st_transform(epsg4326) %>%
  st_crop(xmin=-130, xmax=-60, ymin=20, ymax=50) %>%
  st_transform(epsg3857) %>%
  mutate(data = 0)
# divide the us map into cells of 10km resolution
us_grid <- st_make_grid(st_union(us_map), cellsize = 10000)
us_grid <- st_as_sf(us_grid)
inter <- st_intersects(st_union(us_map),us_grid)
us_grid <- us_grid[unlist(inter),]
us_grid$grid_id <- seq_len(nrow(us_grid))
us_grid_dt <- as.data.table(us_grid)

# read in viewsheds of all windmills, and find the number of windmills in each grid
setwd(viewshed_folder)
files <- list.files(path= viewshed_folder,
                    pattern =".shp$",
                    full.names = FALSE) %>%
  gsub(pattern = ".shp", replacement = "")
us_grid_inter <- lapply(files, function(xx){
  count <- st_layers(paste0(xx,".shp"), do_count = TRUE)[["features"]]
  if(count>100000){
    plan(multisession, 
         workers = min(ceiling(count/100000),20), 
         gc = T)
    us_grid_split <- future_lapply(seq(0, count, by = 100000), function(offset){
      if(offset==count){return(NULL)}
      query <- paste0("SELECT * FROM ", paste0(xx)," LIMIT ", as.integer(100000), " OFFSET ", as.integer(offset))
      sp <- st_read(paste0(viewshed_folder,xx,".shp"),
                    query = query,
                    type = 3,
                    quiet = T
      )%>%
        st_transform(epsg3857)
      us_grid_dt1 <- us_grid_dt
      us_grid_dt1[, inter := sf::st_intersects(x,sp)]
      us_grid_split <- us_grid_dt1[ , list( inter = unlist(inter) ) , by = list(grid_id) ]
      rm(us_grid_dt1)
      us_grid_split[,case := sp$case_id[inter]]
      us_grid_split <- unique(us_grid_split[,list(grid_id, case)])
      return(us_grid_split)
    })%>%
      rbindlist() %>%
      as.data.table()
    plan(sequential)
    us_grid_split <- unique(us_grid_split[,list(grid_id, case)])
  }else{
    sp <- st_read(paste0(viewshed_folder,xx,".shp"),
                  type = 3,
                  quiet = T
    ) %>%
      st_transform(epsg3857)
    us_grid_dt1 <- us_grid_dt
    us_grid_dt1[, inter := sf::st_intersects(x,sp)]
    us_grid_split <- us_grid_dt1[ , list( inter = unlist(inter) ) , by = list(grid_id) ]
    rm(us_grid_dt1)
    us_grid_split[,case := sp$case_id[inter]]
    us_grid_split <- unique(us_grid_split[,list(grid_id, case)])
  }
  print(xx)
  return(us_grid_split) 
})
plan(sequential)
us_grid_inter <- us_grid_inter %>%
  rbindlist() %>%
  unique()
us_grid_wm <- setDT(us_grid_inter)[,list(vis_windmills = .N),
                                   by=grid_id]
us_grid <- merge(us_grid, us_grid_wm, by="grid_id", all.x=T)
us_grid <- as.data.table(us_grid)
us_grid[is.na(vis_windmills), vis_windmills := 0]
us_grid <- as.data.table(us_grid)
us_grid <- st_as_sf(us_grid)

# cut the number of windmills in view into several groups
us_grid$vis_break <- cut(us_grid$vs_wndm, 
                         breaks=c(0, 1, 5, 10, 20, 50, 100,Inf), 
                         right = F, 
                         labels=c("0","1-4","5-9","10-19","20-49","50-99", ">=100"))


# Plot a map of distribution on windmill visibility (Figure S2)
figure <- ggplot() + 
  geom_sf(data = us_grid, aes(fill = vis_break), 
          colour = NA,  lwd = 0 , alpha = 1) +
  geom_sf(data = us_map, aes(NULL),fill= NA,  color="white", lwd=0.4) + 
  scale_fill_manual(name = "", 
                    values = paletteer_d("ggsci::light_green_material")
                    ) +
  theme_void(base_family = "serif") +
  guides(fill=guide_legend(direction = "horizontal", nrow = 2, byrow=TRUE)) + 
  theme(legend.position = c(0.75,0.1),
        legend.direction = "horizontal",
        legend.text = element_text(size=9),
        plot.title = element_text(hjust = 0.5, vjust = -1, size=14),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
ggsave(paste0(figure_folder,"figure_s2.png"),
       figure,
       scale=1,
       width=7.5, height=4)