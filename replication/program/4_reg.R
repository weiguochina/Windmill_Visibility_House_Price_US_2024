# ========================================================================
# Last Update: 2024/02
# Project: Windmills and House Transactions
# Aim: Generate regressions on the property value effect of windmill visibility
# Author: Wei Guo (wei,guo@ccmcc.it)
#
# NOTE: Need to load a census key in line 12
# ========================================================================

rm(list=ls())

key = 


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
library(glue)
library(fixest)
library(kableExtra)
library(gridExtra)
library(grid)
library(tidycensus)


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
mountain_folder <<- paste0(data_folder,"mountain/")
political_folder <<- paste0(data_folder,"political/")
buildheig_folder <<- paste0(data_folder,"building_height/")


# define the output folder
output_folder <<- paste0(data_folder,"/final_data/")
dir.create(file.path(output_folder), showWarnings = FALSE)

# # find the place where GRASS is installed
grass_installation = findGRASS() 

options("rgdal_show_exportToProj4_warnings"="none") 

options(modelsummary_get = "broom")

# define the useful crs codes
epsg4326 <<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
epsg3857 <<- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
epsg102005 <<- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs"


# load the census api key
census_api_key(key, install = TRUE)


# function for inverse hyperbolic sine transformation
ihs <- function(x) { 
  y <- log(x + sqrt(x ^ 2 + 1)) 
  return(y) 
}


# ========================================================================
# Data Initialization

reg_data <-  read_fst(paste0(data_folder,"reg_data.fst"),
                      as.data.table = T)

# Add urban indicator
us_urban <- urban_areas() %>%
  st_transform(crs = epsg3857)
# find windmills in urban areas
reg_data_sf <- reg_data %>%
  select(ImportParcelID, X, Y) %>%
  unique() %>%
  st_as_sf(coords = c("X","Y"),
           crs = epsg4326) %>%
  st_transform(crs = epsg3857)
reg_data_sf <- reg_data_sf %>%
  st_join(us_urban)
reg_data_urban <- reg_data_sf %>% 
  as.data.table() %>%
  select(-geometry) %>%
  as.data.table() %>%
  select(ImportParcelID, UACE10, UATYP10)
reg_data_urban[UATYP10%in%c("C","U"), urban:=T]
reg_data_urban[is.na(urban), urban:=F]
reg_data <- merge(reg_data, reg_data_urban, by = "ImportParcelID", all.x=T)


# create useful columns and harmonize non-numeric variables
reg_data[, salesyear := year(DocumentDate)]
reg_data[, month := month(DocumentDate)]
reg_data[,YearBuilt := as.numeric(YearBuilt)]
reg_data[,YearRemodeled := as.numeric(YearRemodeled)]
reg_data[YearBuilt<YearRemodeled, YearBuilt:=YearRemodeled]
reg_data[,NoOfStories := as.numeric(NoOfStories)]
reg_data[,TotalRooms := as.numeric(TotalRooms)]
reg_data[,TotalBedrooms := as.numeric(TotalBedrooms)]
reg_data[,LotSizeAcres := as.numeric(LotSizeAcres)]
reg_data[,TotalCalculatedBathCount := as.numeric(TotalCalculatedBathCount)]
reg_data[,NoOfBuildings := as.numeric(NoOfBuildings)]
reg_data[,NoOfUnits := as.numeric(NoOfUnits)]
reg_data[,NoOfStories := as.numeric(NoOfStories)]
reg_data[, repeat_sale := .N, by=list(ImportParcelID) ]
reg_data[is.na(n_vis_wm ), n_vis_wm := 0 ]
reg_data[is.na(n_vis_wm_tot ), n_vis_wm_tot := 0 ]
reg_data[is.na(n_prxm_wm ), n_prxm_wm := 0 ]
reg_data[is.na(n_prxm_wm_tot ), n_prxm_wm_tot := 0 ]
reg_data[, COUNTYFP := paste0(STATEFP,COUNTYFP) ]
reg_data[, yearmon := paste0(salesyear,str_pad(month,2,"left","0")) ]

# create treatment dummies
reg_data[n_vis_wm_tot==0, Treated_Visible := F]
reg_data[n_vis_wm_tot>0, Treated_Visible := T]
reg_data[n_vis_wm==0 & n_vis_wm_tot>0, Post_Visible := F]
reg_data[n_prxm_wm==0 & n_vis_wm_tot==0, Post_Visible := F]
reg_data[n_vis_wm>0 & n_vis_wm_tot>0, Post_Visible := T]
reg_data[n_prxm_wm>0 & n_vis_wm_tot==0, Post_Visible := T]
reg_data[n_prxm_wm==0, Post_Proxm := F]
reg_data[n_prxm_wm>0, Post_Proxm := T]
reg_data[, Treated_Proxm := dist_prxm_wm_tot>10000]



# ========================================================================
# Add windmill information

# Add information about the closest turbine
setwd(windmill_folder)
windmills <- read_dta(paste0("windmills.dta")) %>%
  as.data.table() %>%
  select(case_id, p_year, p_cap, t_cap, t_hh, t_rd, t_rsa, t_ttlh )
windmills <- windmills[p_year>0]
windmills[p_cap<0, p_cap:=NA]
windmills[t_cap<0, t_cap:=NA]
windmills[t_hh<0, t_hh:=NA]
windmills[t_rd<0, t_rd:=NA]
windmills[t_rsa<0, t_rsa:=NA]
windmills[t_ttlh<0, t_ttlh:=NA]
windmills[, case_dist_vis_wm := case_id]

# for visible transactions after the installation
reg_data <- merge(reg_data, windmills, by = "case_dist_vis_wm", all.x=T)
reg_data[is.na(p_year), p_year:=0 ]
reg_data[is.na(p_cap), p_cap:=0 ]
reg_data[is.na(t_cap), t_cap:=0 ]
reg_data[is.na(t_hh), t_hh:=0 ]
reg_data[is.na(t_rd), t_rd:=0 ]
reg_data[is.na(t_rsa), t_rsa:=0 ]
reg_data[is.na(t_ttlh), t_ttlh:=0 ]
windmills[, case_dist_vis_wm := NULL]

# for visible transactions before the installation
windmills <- read_dta(paste0("windmills.dta")) %>%
  as.data.table() %>%
  select(case_id, p_year, p_cap, t_cap, t_hh, t_rd, t_rsa, t_ttlh )
windmills <- windmills[p_year>0]
windmills[p_cap<0, p_cap:=NA]
windmills[t_cap<0, t_cap:=NA]
windmills[t_hh<0, t_hh:=NA]
windmills[t_rd<0, t_rd:=NA]
windmills[t_rsa<0, t_rsa:=NA]
windmills[t_ttlh<0, t_ttlh:=NA]
names(windmills) <- paste0(names(windmills),"_vis_tot")
windmills[, case_dist_vis_wm_tot := case_id_vis_tot]
reg_data <- merge(reg_data, windmills, by = "case_dist_vis_wm_tot", all.x=T)
reg_data[p_year==0, p_year:=p_year_vis_tot ]
reg_data[p_cap==0, p_cap:=p_cap_vis_tot ]
reg_data[t_cap==0, t_cap:=t_cap_vis_tot ]
reg_data[t_hh==0, t_hh:=t_hh_vis_tot]
reg_data[t_rd==0, t_rd:=t_rd_vis_tot ]
reg_data[t_rsa==0, t_rsa:=t_rsa_vis_tot ]
reg_data[t_ttlh==0, t_ttlh:=t_ttlh_vis_tot ]
windmills[, case_dist_vis_wm_tot := NULL]
reg_data[is.na(p_year), p_year:=0 ]
reg_data[is.na(p_cap), p_cap:=0 ]
reg_data[is.na(t_cap), t_cap:=0 ]
reg_data[is.na(t_hh), t_hh:=0 ]
reg_data[is.na(t_rd), t_rd:=0 ]
reg_data[is.na(t_rsa), t_rsa:=0 ]
reg_data[is.na(t_ttlh), t_ttlh:=0 ]

# for non-visible approximate transactions after the installation
windmills <- read_dta(paste0("windmills.dta")) %>%
  as.data.table() %>%
  select(case_id, p_year, p_cap, t_cap, t_hh, t_rd, t_rsa, t_ttlh )
windmills <- windmills[p_year>0]
windmills[p_cap<0, p_cap:=NA]
windmills[t_cap<0, t_cap:=NA]
windmills[t_hh<0, t_hh:=NA]
windmills[t_rd<0, t_rd:=NA]
windmills[t_rsa<0, t_rsa:=NA]
windmills[t_ttlh<0, t_ttlh:=NA]
names(windmills) <- paste0(names(windmills),"_prxm")
windmills[, case_dist_prxm_wm := case_id_prxm]
reg_data <- merge(reg_data, windmills, by = "case_dist_prxm_wm", all.x=T)
reg_data[p_year==0, p_year:=p_year_prxm ]
reg_data[p_cap==0, p_cap:=p_cap_prxm ]
reg_data[t_cap==0, t_cap:=t_cap_prxm ]
reg_data[t_hh==0, t_hh:=t_hh_prxm]
reg_data[t_rd==0, t_rd:=t_rd_prxm ]
reg_data[t_rsa==0, t_rsa:=t_rsa_prxm ]
reg_data[t_ttlh==0, t_ttlh:=t_ttlh_prxm ]
windmills[, case_dist_prxm_wm := NULL]
reg_data[is.na(p_year), p_year:=0 ]
reg_data[is.na(p_cap), p_cap:=0 ]
reg_data[is.na(t_cap), t_cap:=0 ]
reg_data[is.na(t_hh), t_hh:=0 ]
reg_data[is.na(t_rd), t_rd:=0 ]
reg_data[is.na(t_rsa), t_rsa:=0 ]
reg_data[is.na(t_ttlh), t_ttlh:=0 ]

# for non-visible approximate transactions before the installation
windmills <- read_dta(paste0("windmills.dta")) %>%
  as.data.table() %>%
  select(case_id, p_year, p_cap, t_cap, t_hh, t_rd, t_rsa, t_ttlh )
windmills <- windmills[p_year>0]
windmills[p_cap<0, p_cap:=NA]
windmills[t_cap<0, t_cap:=NA]
windmills[t_hh<0, t_hh:=NA]
windmills[t_rd<0, t_rd:=NA]
windmills[t_rsa<0, t_rsa:=NA]
windmills[t_ttlh<0, t_ttlh:=NA]
names(windmills) <- paste0(names(windmills),"_prxm_tot")
windmills[, case_dist_prxm_wm_tot := case_id_prxm_tot]
reg_data <- merge(reg_data, windmills, by = "case_dist_prxm_wm_tot", all.x=T)
reg_data[p_year==0, p_year:=p_year_prxm_tot ]
reg_data[p_cap==0, p_cap:=p_cap_prxm_tot ]
reg_data[t_cap==0, t_cap:=t_cap_prxm_tot ]
reg_data[t_hh==0, t_hh:=t_hh_prxm_tot]
reg_data[t_rd==0, t_rd:=t_rd_prxm_tot ]
reg_data[t_rsa==0, t_rsa:=t_rsa_prxm_tot ]
reg_data[t_ttlh==0, t_ttlh:=t_ttlh_prxm_tot ]
windmills[, case_dist_prxm_wm_tot := NULL]
reg_data[is.na(p_year), p_year:=0 ]
reg_data[is.na(p_cap), p_cap:=0 ]
reg_data[is.na(t_cap), t_cap:=0 ]
reg_data[is.na(t_hh), t_hh:=0 ]
reg_data[is.na(t_rd), t_rd:=0 ]
reg_data[is.na(t_rsa), t_rsa:=0 ]
reg_data[is.na(t_ttlh), t_ttlh:=0 ]



# ========================================================================
# Regression Initialization

gm <- modelsummary::gof_map
gm$clean[gm$raw == 'nobs'] <- "Observations"
gm$clean[gm$raw == 'r2.adjusted'] <- "Adj. $R^2$"
gm$clean[gm$raw == 'adj.r.squared'] <- "Adj. $R^2$"
gm$clean[gm$raw == 'nclusters'] <- "Clusters"

ctls <- "YearBuilt+TotalBedrooms+TotalCalculatedBathCount+LotSizeAcres"
CLS <- "GEOID + salesyear"
FE1 <- "GEOID^salesyear"
FE2 <- "COUNTYFP^month"

# ========================================================================
# Test parallel pre-trend assumption (Figure S3)

# subset to data with all non-missing property characteristics
reg_data_valid <- reg_data %>%
  filter(if_all(c(YearBuilt, TotalBedrooms ,TotalCalculatedBathCount, LotSizeAcres,
                  Post_Visible, Treated_Visible,GEOID, salesyear, COUNTYFP, month), negate(is.na))) %>%
  subset(dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000)
mod4 <- feols(as.formula(paste0("log(SalesPriceAmount)~",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data_valid,
              cluster = CLS)
reg_data_valid$resid <- mod4$residuals
reg_data_valid[, vis_year := p_year]
reg_data_valid[is.na(vis_year), vis_year := p_year_vis_tot]
reg_data_valid[, year_from_vis := salesyear - vis_year]
reg_data_valid[year_from_vis< -15, year_from_vis := NA]
reg_data_valid[year_from_vis> 15, year_from_vis := NA]
reg_data_valid[Post_Visible==T & year_from_vis<0, year_from_vis := 0]
reg_data_valid[,resid_05 := quantile(resid, 0.05,na.rm=T), by = list(year_from_vis,
                                                                      Treated_Visible,
                                                                      Post_Visible)]
reg_data_valid[,resid_95 := quantile(resid, 0.95,na.rm=T), by = list(year_from_vis,
                                                                      Treated_Visible,
                                                                      Post_Visible)]
reg_data_valid[resid>resid_95 & resid < resid_05, resid := NA]
resid_year <- setDT(reg_data_valid)[,
                                     list(resid = mean(resid, na.rm = T)),
                                     by = list(year_from_vis,
                                               Treated_Visible,
                                               Post_Visible)] %>%
  subset(!is.na(year_from_vis))
reg_data_valid <- subset(reg_data_valid, !is.na(year_from_vis))
set.seed(1)
reg_data_valid[,  Treated_Visible :=factor(Treated_Visible,
                                 levels = c(FALSE,TRUE),
                                 labels = c("Control","Treated"))]
plot <- ggplot(data=reg_data_valid, 
       aes(x=year_from_vis, y=resid,
           group = interaction(Treated_Visible, Post_Visible),
           color = Treated_Visible,
           linetype = Treated_Visible)) +
  geom_smooth(size=1,
              method = "gam",
              formula = y ~ poly(x, 2),
              alpha=0.2,
              se = T) + 
  geom_hline(yintercept=0, color="black",linetype=4,size=0.2)+
  scale_color_manual("Windmill Visibility",
                       values = paletteer_d("ggthemes::Classic_Gray_5",2,direction=-1)
                     )+
  scale_fill_discrete("Windmill Visibility")+
  scale_shape_discrete("Windmill Visibility",)+
  scale_linetype_manual("Windmill Visibility",
                        values = c(2,1))+
  scale_x_continuous(limits = c(-15,15),
                     breaks = seq(-15,15,5)) + 
theme_bw(base_family="serif",base_size = 12) +
  theme(legend.position = c(0.2,0.15), 
        legend.direction = "vertical",
        plot.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size=10),
        panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(title=glue("Test on Parallel Pre-Trend"),
       x="Years from Windmill Installation",
       y="Residualized Log Property Price")
ggsave(paste0(output_folder,"figure_s3.png"),plot,
       scale = 1.2,
       width = 5, height = 4)

  



# ========================================================================
# Baseline Regression (Table 1)

# 1  1-10km from windmills, did indicators, prop char, and location-year + county-seasonality FEs
mod1 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible +",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)

# 2 with intensity of the visibility using the number of windmills in view
mod2 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible + n_vis_wm*Treated_Visible + ",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)

# 3 with intensity of the visibility using the indicator for <20 windmills in view
reg_data[,n_vis_wm_break:=cut(n_vis_wm, breaks=c(0,1,20,nrow(windmills)), right = F)]
mod3 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible + Treated_Visible + i(n_vis_wm_break,Treated_Visible,'[0,1)') +  ",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)

# 4 with windmill installation year
reg_data[, windmill_year := p_year]
reg_data[is.na(windmill_year), windmill_year := p_year_vis_tot]
reg_data[is.na(windmill_year), windmill_year := p_year_prxm]
reg_data[is.na(windmill_year), windmill_year := p_year_prxm_tot]
reg_data[windmill_year<1800, windmill_year := NA]
reg_data[windmill_year>2100, windmill_year := NA]
reg_data[, de_windmill_year := windmill_year - mean(windmill_year, na.rm=T)]
mod4 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible +",
                                 "i(Post_Visible*Treated_Visible,de_windmill_year,0) + ",
                                 ctls,"|",FE1,"+",FE2)),
               data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
               cluster = CLS,
               lean = T)

# export the results
mod <- list(mod1, mod2, mod3, mod4)
table <- modelsummary(mod, 
                      output="kableExtra",
                      coef_omit = "YearBuilt|Total|Lot|Intercept", 
                      stars = c('*' = .1, '**' = .05, '***' = .01), 
                      stars_note = T,
                      gof_omit='BIC|AIC|Sigma|RMSE|Within|Log|Pseudo',
                      gof_rename = gm,
                      fmt = '%.3g',
                      title =  "Table 1",
                      booktabs = T, escape = FALSE) %>%
  kable_styling(latex_options = 'HOLD_position')
file_name <- paste0("table_1.html")
kableExtra::save_kable(table, file = file_name)



# ========================================================================
# Robustness checks (Table S1)


# 1  baseline
mod1 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible +",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)


# 2 removing non-disclosure states
ND_STATEFP <- c("02","16","18","20","22","28","30","35","38","46","48","49","56")
mod2 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible +",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[!STATEFP %in% ND_STATEFP & dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)


# 3 using repeated sale only
reg_data[, repeat_sale := .N, by=list(ImportParcelID) ]
reg_data[, repeat_sale_pre := sum(as.numeric(!Post_Proxm)), by=list(ImportParcelID) ]
reg_data[, repeat_sale_post := sum(as.numeric(Post_Proxm)), by=list(ImportParcelID) ]
mod3 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible",
                        "|",FE2, " + COUNTYFP^salesyear + ImportParcelID")),
      data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000 & repeat_sale_pre >1 & repeat_sale_post >1],
      cluster = CLS,
      lean = T)



# 4 property characteristics interacted with Year
mod4 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible +",
                                "i(windmill_decade,YearBuilt)+",
                                "i(windmill_decade,TotalBedrooms)+",
                                "i(windmill_decade,TotalCalculatedBathCount)+",
                                "i(windmill_decade,LotSizeAcres)","|",FE1,"+",FE2)),
              data = reg_data[ dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)

# 5 property characteristics interacted with STATEFP (for the major 5 states: 06, 08, 17, 48, 36)
reg_data[, state06 := STATEFP=="06"]
reg_data[, state08 := STATEFP=="08"]
reg_data[, state17 := STATEFP=="17"]
reg_data[, state48 := STATEFP=="48"]
reg_data[, state36 := STATEFP=="36"]
mod5 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible +",
                                 ctls,
                                 "+i(state06,YearBuilt)+i(state08,YearBuilt)+i(state17,YearBuilt)+i(state48,YearBuilt)+i(state36,YearBuilt)",
                                 "+i(state06,TotalBedrooms)+i(state08,TotalBedrooms)+i(state17,TotalBedrooms)+i(state48,TotalBedrooms)+i(state36,TotalBedrooms)",
                                 "+i(state06,TotalCalculatedBathCount)+i(state08,TotalCalculatedBathCount)+i(state17,TotalCalculatedBathCount)+i(state48,TotalCalculatedBathCount)+i(state36,TotalCalculatedBathCount)",
                                 "+i(state06,LotSizeAcres)+i(state08,LotSizeAcres)+i(state17,LotSizeAcres)+i(state48,LotSizeAcres)+i(state36,LotSizeAcres)",
                                 "|",FE1,"+",FE2)),
               data = reg_data[ dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
               cluster = CLS,
               lean = T)


# export the results
mod <- list(mod1, mod2, mod3, mod4, mod5)
names(mod) <- c("baseline","remove ND states","repeated sales","Prop interacted with year", "Prop interacted with state")
table <- modelsummary(mod, 
                      output="kableExtra",
                      stars = c('*' = .1, '**' = .05, '***' = .01), 
                      stars_note = T,
                      gof_omit='BIC|AIC|Sigma|RMSE|Within|Log|Pseudo',
                      gof_rename = gm,
                      fmt = '%.3g',
                      title =  "Table S1",
                      booktabs = T, escape = FALSE) %>%
  kable_styling(latex_options = 'HOLD_position')
file_name <- paste0("table_s1.html")
kableExtra::save_kable(table, file = file_name)


# ========================================================================
# Limit to places with low building height (Table S3 and Figure S1)


# plot a map of building height for each block (Figure S1)
setwd(code_location)
source(building_height_plot)
building_height_plot()


# Add census block
block <- block_groups(cb = T) %>%
  st_transform(crs = epsg3857)
reg_data_sf <- reg_data %>%
  select(ImportParcelID, X, Y) %>%
  unique() %>%
  st_as_sf(coords = c("X","Y"),
           crs = epsg4326) %>%
  st_transform(crs = epsg3857)
reg_data_sf <- reg_data_sf %>%
  st_join(block)
reg_data_block <- reg_data_sf %>% 
  as.data.table() %>%
  select(-geometry) %>%
  as.data.table() %>%
  select(ImportParcelID, GEOID)
reg_data_block[, AREAKEY := GEOID]
reg_data_block[, GEOID := NULL]

# read building height data
setwd(buildheig_folder)
bd_height_sf <- read_sf(dsn = "srtm_bg_building_heights.gdb", 
                        layer = "srtm_bldg_heights_class") 
bd_height <- bd_height_sf %>% 
  as.data.table() %>%
  select(AREAKEY,SUM:Height_cat)


# merge the building data
reg_data_block <- merge(reg_data_block, bd_height, by = "AREAKEY", all.x=T)
reg_data_block[is.na(SUM), SUM := 0]
reg_data_block[is.na(SEPH), SEPH := 0]
reg_data_block[is.na(Height_cat), Height_cat := "Low"]
reg_data_block[, Height_cat := as.factor(Height_cat)]
reg_data <- merge(reg_data, reg_data_block, by = "ImportParcelID", all.x=T)
reg_data[, Shape_Length:= NULL]
reg_data[, Shape_Area:= NULL]
reg_data[, Shape:= NULL]
reg_data[, Cat_desc:= NULL]
reg_data[, AREALAND:= NULL]
reg_data[, AREAWATR:= NULL]
reg_data[, value:= NULL]


# recode the height category
reg_data[Height_cat=="Medium-High", Height_cat := "Medium"]
reg_data[Height_cat=="Very high", Height_cat := "High"]
hgt_cat <- levels(reg_data$Height_cat)
mod1 <- lapply(hgt_cat, function(x){
  feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible + ",
                                  ctls,"|",FE1,"+",FE2)),
                data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000 & Height_cat==x],
                cluster = CLS)
})

  
# export the results
mod <- list(mod1, mod2, mod3)
names(mod) <- hgt_cat
table <- modelsummary(mod, 
                      output="kableExtra",
                      coef_omit = "YearBuilt|Total|Lot|Intercept", 
                      stars = c('*' = .1, '**' = .05, '***' = .01), 
                      stars_note = T,
                      gof_omit='BIC|AIC|Sigma|RMSE|Within|Log|Pseudo',
                      gof_rename = gm,
                      fmt = '%.3g',
                      title =  glue("Table S3"),
                      booktabs = T, escape = FALSE) %>%
  kable_styling(latex_options = 'HOLD_position')
file_name <- paste0("table_s3.html")
kableExtra::save_kable(table, file = file_name)  


# ========================================================================
# Effect by distance from the windmill (Figure 2)

# create distance variables
reg_data[, dist := dist_vis_wm]
reg_data[is.na(dist), dist := dist_vis_wm_tot]
reg_data[is.na(dist), dist := dist_prxm_wm]
reg_data[is.na(dist), dist := dist_prxm_wm_tot]
reg_data[,dist_bin:=cut(dist, breaks=c(seq(0,10000,500)), right = F)]

# average post_program effect
mod1 <- feols(as.formula(paste0("log(SalesPriceAmount)~i(dist_bin, Post_Visible*Treated_Visible) +",
                                "Post_Visible  + Treated_Visible + ",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist<=10000],
              cluster = CLS)
cm_fig <- c(seq(500,20000,500))
names(cm_fig) <- c(paste0("dist_bin::[",seq(0,19500,500),",",seq(500,20000,500),"):Post_Visible * Treated_Visible"))
dat <- map_dfr(c(0.95), function(x) {
  modelplot(list(mod1), conf_level = x, draw = FALSE) %>%
    mutate(.width = x)
}) %>%
  subset(grepl("dist_bin",term)) %>%
  as.data.table()
dat[, model:="All"]
dat[, term1:= gsub("\\w.*?=", "", term)]
dat[, term1:= gsub(".*\\[(.*)\\,.*", "\\1",term1)]
dat[, term1:= 500 + as.numeric(term1)]
dat[, model:=factor(model, levels = c("All","Urban","Rural"))]
dat[,x1:=500]
dat[,y1:=0.1]
dat[,text1:="Noise"]
dat[,x2:=2200]
dat[,y2:=0.1]
dat[,text2:="Visibility"]

# generate the number of transactions for each distance bin
reg_data <- reg_data[dist<=10000]
reg_data[, trans_all := .N, by = list(dist_bin)]
reg_data[, trans := .N, by = list(dist_bin, urban)]
dat_trans <- reg_data[, c("trans_all","trans","dist_bin","urban")] %>%
  unique()
dat_trans1 <- dat_trans[, c("trans_all","dist_bin")] %>%
  unique()
dat_trans1[, model := "All"]
dat_trans[urban==T, model := "Urban"]
dat_trans[urban==F, model := "Rural"]
dat_trans[, trans_all := trans]
dat_trans <- dat_trans[, c("trans_all","dist_bin","model")] %>%
  unique()
dat_trans <- rbind(dat_trans, dat_trans1)
dat_trans[, term1:= gsub("\\w.*?=", "", dist_bin)]
dat_trans[, term1:= gsub(".*\\[(.*)\\,.*", "\\1",term1)]
dat_trans[, term1:= 500 + as.numeric(term1)]
dat_trans[, trans_all := -trans_all]

# add the number of observations to each bin
dat_all <- merge(dat, dat_trans, by = c("model","term1"), all.x=T)

coeff <- mean(dat_all[model==x]$trans_all)*18
add <- .2
breaks1 <- seq(-0.1,0.025,0.025) + add
labels <- seq(-0.1,0.025,0.025)
breaks2 <- seq(0,750000,250000)
fig <- ggplot(dat_all[model=="All"], aes(
  x = term1, 
  ymin = conf.low + add, ymax = conf.high+add)) +
  geom_point(aes( y = estimate+ add), size=1, color = "black") +
  geom_line(aes( y = estimate+ add), lwd=0.5, color = "black")+
  geom_bar(aes(y= trans_all / coeff), size=1,  stat="identity",fill = "black", color=NA, alpha=.3) +
  geom_ribbon(alpha = 0.2, colour = NA)+
  scale_x_continuous(breaks = seq(0,10000,1000))+
  scale_y_continuous(
    name = NULL,
    sec.axis = sec_axis(~.*(coeff), 
                        name=NULL, 
                        breaks = breaks2,
                        labels = label_comma()), 
    breaks = breaks1,
    labels = labels,
  ) +
  scale_color_discrete(name = "") +
  scale_shape_discrete(name = "") +
  facet_wrap(~model,nrow=3, scales = "free_y")+
  geom_hline(yintercept=0 + add, color="black",linetype=4,size=0.2)+
  theme_bw(base_family="serif", base_size = 11)+
  labs(x = NULL)+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=11))
  
ggsave("figure_2.png",fig,
       width=4,
       height=4)



# ========================================================================
# Dynamic Effects (Figure 4)

reg_data[, vis_year := p_year]
reg_data[is.na(vis_year), vis_year := p_year_vis_tot]
reg_data[, year_from_vis := salesyear - vis_year]
reg_data[is.na(vis_year), year_from_vis := -1]
reg_data[year_from_vis< 0, year_from_vis := -1]
reg_data[year_from_vis> 20, year_from_vis := NA]
mod1 <- feols(as.formula(paste0("log(SalesPriceAmount)~i(year_from_vis,Post_Visible*Treated_Visible, -1) +",
                                "i(year_from_vis,Post_Visible, -1) +",
                                "i(year_from_vis,Treated_Visible, -1)  +",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)

# get the dynamic coefficients
dat <- map_dfr(c(0.95), function(x) {
  modelplot(list(mod1), conf_level = x, draw = FALSE) %>%
    mutate(.width = x)
}) %>%
  subset(grepl("Post_Visible",term) & grepl("Treated_Visible",term)) %>%
  as.data.table()
dat[, term1:= as.numeric(str_extract(term, "[0-9]+"))]
dat <- rbind(dat,
             data.table(term1 = -2, conf.low = 0, conf.high = 0, estimate = 0),
              fill = T)
dat[, text := as.character(term1)]
dat[text=="-2", text := "Before"]
dat[, estimate1:=estimate*100]
dat[, conf.low1:=conf.low*100]
dat[, conf.high1:=conf.high*100]
fig <- ggplot(dat, aes(
  x = term1, y = estimate1,
  ymin = conf.low1, ymax = conf.high1)) +
  geom_point(size=2, color = "black") +
  geom_line(lwd=1, color = "black")+
  geom_ribbon(alpha = 0.2, colour = NA)+
  scale_x_continuous(limits = c(-2.5,10),
                     breaks = dat$term1,
                     labels = dat$text)+
  scale_y_continuous(limits = c(-5, 2.5)) + 
  geom_hline(yintercept=0, color="black",linetype=4,size=0.2)+
  geom_vline(xintercept=0, color="red",linetype=2,size=1)+
  labs(title = "",
       x = "Years from Windmill Installation", y = "Change in Property Values (%)")+
  theme_bw(base_family="serif", base_size = 12)+
  theme(legend.position = "none",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size=11),
        plot.title = element_blank())
ggsave(paste0(output_folder,"figure_4.png"),fig,
       scale = 1.2,
       width = 4, height = 3.8)


# ========================================================================
#  Heterogeneity analysis (Results for Figure 3)


# 1 Urban versus Rural

mod1_1 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible +",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000 & urban==T],
              cluster = CLS,
              lean = T)
mod1_2 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible +",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000 & urban==F],
              cluster = CLS,
              lean = T)



# 2 Mountainous

# find counties with mountain (>1000 meter)
setwd(mountain_folder)
county_mountain <- readxl::read_excel("county_elev_stats.xlsx") %>%
  as.data.table
mountainous_fips <- county_mountain[RANGE>1000,]$FIPS
reg_data[, mountainouns := COUNTYFP %in% mountainous_fips]

mod2_1 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible	+",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[mountainouns == T & dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)
mod2_2 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible	+",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[mountainouns == F & dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)


# 3 By Income

# load county level income data
census_var <- load_variables(year=2015, cache = TRUE, dataset = "acs5")
var <- c("B19013_001")
names <- c("med_inc")
demog_tract <- get_acs(geography = "county", year = 2015,   geometry = F,
                       variables = var, output = "wide",
                       survey = "acs5") %>%
  dplyr::select(-NAME) %>%
  as.data.table

# find counties above the national median income
med_income <- median(demog_tract$B19013_001E, na.rm=T)
county_above_med_income <- demog_tract[B19013_001E>med_income, ]$GEOID 
reg_data[, above_med_income := COUNTYFP %in% county_above_med_income]

mod3_1 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible	+",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[above_med_income == T & dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)
mod3_2 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible	+",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[above_med_income == F & dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)



# 4 By political leaning

# load the presidential election outcome of each county
setwd(political_folder)
county_pol <- read_fst("countypres_2000-2016.fst") %>%
  subset(year==2016) %>%
  subset(party %in% c("republican")) %>%
  as.data.table
county_pol[, red := candidatevotes/totalvotes >= 0.5]
county_pol[, FIPS := str_pad(FIPS,5,"left","0")]
red_county <- county_pol[red==T,]$FIPS
reg_data[, red := COUNTYFP %in% red_county]

mod4_1 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible	+",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[red == T & dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)
mod4_2 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible	+",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[red == F & dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)

# export the results
mod <- list(mod1, mod2)
names(mod) <- c("> Median Income","< Median Income")
table <- modelsummary(mod, 
                      output="kableExtra",
                      coef_omit = "YearBuilt|Total|Lot|Intercept", 
                      stars = c('*' = .1, '**' = .05, '***' = .01), 
                      stars_note = T,
                      gof_omit='BIC|AIC|Sigma|RMSE|Within|Log|Pseudo',
                      gof_rename = gm,
                      fmt = '%.3g',
                      title =  "Marginal Effect on Log(Prop Values), Breaking Counties by Median Income",
                      booktabs = T, escape = FALSE) %>%
  kable_styling(latex_options = 'HOLD_position')
file_name <- paste0("reg_income.html")
kableExtra::save_kable(table, file = file_name)


# 5 Marginal effect of new wind farm relative to >1 wind farms

# Load windmill data
windmills <-read_sf(paste0(windmill_folder, "uswtdb_v4_1_20210721.shp")) %>%
  st_transform(crs = epsg3857)

# spatial join with census tract
census_tract <- tracts(cb = TRUE) %>%
  st_transform(crs = epsg3857)
windmills <- st_join(windmills, census_tract)

# find # of windmills in each county
wind_county <- setDT(windmills)[,
                                list(n_plant = uniqueN(eia_id),
                                     n_turbine = uniqueN(case_id)),
                                by = t_fips]
countyplant_1 <- wind_county[n_plant==1,]$t_fips
countyplant_2<- wind_county[n_plant>=2,]$t_fips
reg_data[, new_windmill:=COUNTYFP %in% countyplant_1]

mod5 <- feols(as.formula(paste0("log(SalesPriceAmount)~i(new_windmill,Post_Visible*Treated_Visible) + i(new_windmill,Treated_Visible)  + i(new_windmill,Post_Visible)	+",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)



# export the results
mod <- list(mod1_1,mod1_2,mod2_1,mod2_2,mod3_1,mod3_2,mod4_1,mod4_2,mod5)
table <- modelsummary(mod, 
                      output="kableExtra",
                      coef_omit = "YearBuilt|Total|Lot|Intercept", 
                      stars = c('*' = .1, '**' = .05, '***' = .01), 
                      stars_note = T,
                      gof_omit='BIC|AIC|Sigma|RMSE|Within|Log|Pseudo',
                      gof_rename = gm,
                      fmt = '%.3g',
                      title =  "Results for Figure 3",
                      booktabs = T, escape = FALSE) %>%
  kable_styling(latex_options = 'HOLD_position')
file_name <- paste0("figure_3_result.html")
kableExtra::save_kable(table, file = file_name)



# ========================================================================
# Robustness check by Model Specification (Table S2)

# 1 Single difference between visible and non-visible areas 

mod1_1 <- feols(as.formula(paste0("log(SalesPriceAmount)~Treated_Visible1 + ",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)

mod1_2 <- feols(as.formula(paste0("log(SalesPriceAmount)~Treated_Visible1*Treated_Proxm +",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=50000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)


# 2 Baseline DID
mod2 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible +",
                                ctls,"|",FE1,"+",FE2)),
              data = reg_data[dist_prxm_wm_tot<=10000 & dist_prxm_wm_tot>=1000],
              cluster = CLS,
              lean = T)

# 3 Triple Diff with proximity to windmill, expanding to 50km from a windmill
reg_data[, Post_Visible1 := Post_Visible]
reg_data[dist_prxm_wm_tot>=10000 , Post_Visible1 := F]
reg_data[, Treated_Visible1 := Treated_Visible]
reg_data[, Treated_Visible1 := Treated_Visible]
reg_data[dist_prxm_wm_tot>=10000 , Treated_Visible1 := F]

mod3 <- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible1*Treated_Proxm +",
                                  ctls,"|",FE1,"+",FE2)),
                data = reg_data[dist_prxm_wm_tot<=50000 & dist_prxm_wm_tot>=1000],
                cluster = CLS,
                lean = T)

# 4 Triple Diff with proximity*post-installation, expanding to 50km from a windmill
mod4<- feols(as.formula(paste0("log(SalesPriceAmount)~Post_Visible*Treated_Visible1*Treated_Proxm + Treated_Proxm*Post_Proxm +",
                                  ctls,"|",FE1,"+",FE2)),
                data = reg_data[dist_prxm_wm_tot<=50000 & dist_prxm_wm_tot>=1000],
                cluster = CLS,
                lean = T)

# export the results
mod <- list(mod1_1, mod1_2, mod2, mod3, mod4)
table <- modelsummary(mod, 
                      output="kableExtra",
                      coef_omit = "YearBuilt|Total|Lot|Intercept", 
                      stars = c('*' = .1, '**' = .05, '***' = .01), 
                      stars_note = T,
                      gof_omit='BIC|AIC|Sigma|RMSE|Within|Log|Pseudo',
                      gof_rename = gm,
                      fmt = '%.3g',
                      title =  glue("Table S2"),
                      booktabs = T, escape = FALSE) %>%
  kable_styling(latex_options = 'HOLD_position')
file_name <- paste0("table_s2.html")
kableExtra::save_kable(table, file = file_name)
