# ========================================================================
# Last Update: 2024/02
# Project: Windmills and House Transactions
# Aim: Generate summary statistics for housing transactions 
# Author: Wei Guo
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

# define folder directory
current_location <<- "H:/WeiGuo/Research/windmill_ztrax/program/"

# # find the place where GRASS is installed
grass_installation = findGRASS() 

options("rgdal_show_exportToProj4_warnings"="none") 


# define the useful crs codes
epsg4326 <<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
epsg3857 <<- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
epsg102005 <<- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs"


## RUN ONLY ONCE!
# combine transaction files
data_list <-list.files(path=data_folder,pattern = "trans", full.names = T)
full_data <- lapply(data_list, function(file){
  read_fst(file, as.data.table = T) %>%
    unique()
})
full_data <- full_data %>%
  rbindlist() %>%
  as.data.table()
gc()

# remove non-al and outliers
full_data <- full_data[SalesPriceAmount>10000 & SalesPriceAmount<4000000]
full_data[,YearBuilt := as.numeric(YearBuilt)]
full_data[YearBuilt<1800,YearBuilt := NA]
full_data[YearBuilt>2030,YearBuilt := NA]
full_data[,TotalRooms := as.numeric(TotalRooms)]
full_data[TotalRooms<0,TotalRooms := NA]
full_data <- full_data[is.na(TotalRooms) | TotalRooms<=20]
full_data[,TotalBedrooms := as.numeric(TotalBedrooms)]
full_data[TotalBedrooms<0,TotalBedrooms := NA]
full_data <- full_data[is.na(TotalBedrooms) | TotalBedrooms<=20]
full_data[,TotalCalculatedBathCount := as.numeric(TotalCalculatedBathCount)]
full_data[TotalCalculatedBathCount<0,TotalCalculatedBathCount := NA]
full_data <- full_data[is.na(TotalCalculatedBathCount) | TotalCalculatedBathCount<=10]
full_data[,LotSizeSquareFeet := as.numeric(LotSizeSquareFeet)]
full_data[LotSizeSquareFeet<0,LotSizeSquareFeet := NA]
full_data <- full_data[is.na(LotSizeSquareFeet) | LotSizeSquareFeet<=5000000]
full_data[,Salesyear := year(DocumentDate)]
full_data[Salesyear<1900,Salesyear := NA]
full_data[Salesyear>2030,Salesyear := NA]
full_data[,STATEFP := as.factor(STATEFP)]

# keep only residential properties
full_data[, property_type := as.factor(substr(PropertyLandUseStndCode,1,2))]
full_data <- full_data[property_type%in%c("RI","Ri","RR")]
gc()

# harmonize the did indicators
full_data[is.na(n_vis_wm), n_vis_wm := 0]
full_data[is.na(n_prxm_wm ), n_prxm_wm  := 0]
full_data[,prox_wm := n_prxm_wm  > 0]
full_data[,vis_wm := n_vis_wm > 0]
full_data[,vis_wm := factor(vis_wm, levels = c(T,F), labels = c("Visible","Non-Visible"))]
full_data[,prox_wm := factor(prox_wm, levels = c(T,F), labels = c("Proximate","Non-Proximate"))]


# generate summ tables on housing transactions (Table S5)
setwd(output_folder)
datasummary(Salesyear+SalesPriceAmount+LotSizeSquareFeet+
                YearBuilt+TotalRooms+TotalBedrooms+TotalCalculatedBathCount+
                n_vis_wm+dist_vis_wm+n_prxm_wm+dist_prxm_wm~
                N+Mean+SD+Max+Min+vis_wm*(N+Mean+SD),
              data = full_data,
              output = "table_S5.tex",
              title = 'Summary Statistics of Housing Transactions')


# find the number of observations before and after the treatment in each state
data_state_prepost <- full_data[,c("Post_Visible","Treated_Visible","STATEFP")]
data_state_prepost[, N_count := .N, by = list(Post_Visible, Treated_Visible, STATEFP)]
data_state_prepost <- data_state_prepost %>% 
  unique () %>% 
  arrange(Treated_Visible, Post_Visible, -N_count)
data_state_prepost[, N_sum := sum(N_count)]
data_state_prepost[, N_sum_treated := sum(N_count), by = list(Treated_Visible)]
data_state_prepost[, N_sum_treated_post := sum(N_count), by = list(Treated_Visible,Post_Visible)]
setwd(output_folder)
data_state_prepost[, treated_state := sum(N_count), by = list(STATEFP, Treated_Visible)]
data_state_prepost[, treated_share := treated_state/N_sum_treated]
data_state_prepost[, treated_post_share := N_count/N_sum_treated_post]
data_state_prepost1 <- data_state_prepost[Treated_Visible==T & Post_Visible==T, list(STATEFP, treated_share, treated_post_share)]
data_state_prepost1[, state := fips(STATEFP, to = "Name")]


# plot a scatter plot of N treated against % treated post treatment by state (Figure S4)
fig <- ggplot(data_state_prepost1, aes(x = treated_share, y = treated_post_share)) +
  geom_point(size = 1.5) +
  labs(title = "Share of Treated and Treated-Post-Treatment Properties by State",
       x = "Percentage of Treated Properties",
       y = "Percentage Treated Properties Podata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==st Treatment") +
  geom_text(aes(label=state), vjust=-1, size=2) + 
  scale_y_continuous(limits = c(0, 0.25)) + 
  scale_x_continuous(limits = c(0, 0.25)) + 
  geom_abline(intercept = 0, slope = 1, size = 0.25, linetype = "dashed" ) + 
  theme_bw(base_family="serif", base_size = 12)+
  theme(legend.position = "none",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size=11),
        plot.title = element_blank())
ggsave(paste0(figure_folder,"figure_s4.png"),fig,
       scale = 1.2,
       width = 5, height = 3) 

# limit to properties within 10km of wind facilities and save it for future regression
reg_data <- full_data[!is.na(dist_prxm_wm_tot)]
write_fst(reg_data, paste0(data_folder,"reg_data.fst"))

