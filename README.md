# Supporting Documents for "The visual effect of wind turbines on property values is small and diminishing in space and time"

This package provides the replication code for the main results in Guo, W., Wenz, L., & Auffhammer, M. (2024). The visual effect of wind turbines on property values is small and diminishing in space and time. Proceedings of the National Academy of Sciences, 121(13), e2309372121. The paper is available at [here](https://www.pnas.org/doi/10.1073/pnas.2309372121).

The folder ./program contains all programs, while the folder ./data includes all needed input data files.

## Data

The folder ./data contains necassary data for replication. Please note that due to size limit, not all data files are included. Below we offer a comprehensive guide to acquiring and using the data essential for replicating ths study.

1. Windmill data can be accessed from the United States Wind Turbine Database (USWTDB) produced by USGS50 (1). The key data is consolidated and and saved as .\data\windmill.dta.

2. ZTRAX data is offered by Zillow’s research team (2). To access ZTRAX data, the user will need to first review and agree to the ZTRAX Data License Agreement, then complete the registration online. Once logged in, the user can request access to specific ZTRAX datasets. For acquisition, the user will need to be prepared to provide details on the intended use of the data.

3. Digital Elevation Models are produced by NASA’s Shuttle Radar Topographic Mission (3). These are crucial for viewshed computations in the replication. Due to size constraints, the original DEMs are not included as part of our replication kit. The user will need to download and extract the data to the directory .\data.

4. Other Data for heterogeneity analysis. Data for heterogeneity analysis are drawn from multiple sources. All these data are either acquired through public access in our code or included in the replication kit.
   - The county-level median household income records come from the 2015 American Community Survey. The key data is acquired internally from the census through the code.
   - Presidential election data is sourced from the MIT Election Data and Science Lab (4). The data is consolidated and and saved as .\data\election.dta.
   - The county-level average elevation data is derived from the Environmental Systems Research Institute. The data is consolidated and and saved as .\data\elevation.dta.

## Replication Code

The folder ./program includes all code files for replication.

### Prerequisite

The analysis is processed on a server with 16 cores, 32 threads, 3.10GHz CPU, and 256 GB of memory. The following software and related libraries are required:
1. R version 4.2.1, with the following libraries and their versions:
   leafem   0.2.0, colorspace   2.0-3, deldir   1.0-6, class   7.3-20, leaflet   2.2.0,   satellite   1.0.4,  base64enc   0.1-3,  fs   1.5.2,  rstudioapi   0.14,  proxy   0.4-27,    roxygen2   7.2.1,   listenv   0.8.0,    hexbin   1.28.2,    remotes   2.4.2,    fansi   1.0.3,     xml2   1.3.3,   codetools   0.2-18, doParallel   1.0.17,   cachem   1.0.6, knitr   1.40,  pkgload   1.3.0,    jsonlite   1.8.2,   png   0.1-7, shiny   1.7.2,  BiocManager   1.30.22, compiler   4.2.1,  assertthat   0.2.1, fastmap   1.1.0,    cli   3.6.1, later   1.3.0, 
 htmltools   0.5.3,  prettyunits   1.1.1   tools   4.2.1,  glue   1.6.2,   vctrs   0.6.1, iterators   1.0.14, crosstalk   1.2.0,  lwgeom   0.2-8, xfun   0.33, stringr   1.4.1,   globals   0.16.1,   ps   1.7.1,  mime   0.12, miniUI   0.1.1.1,   lifecycle   1.0.3, devtools   2.4.5,   terra   1.5-34, zoo   1.8-11,   hms   1.1.3,  promises   1.2.0.1,   parallel   4.2.1,   RColorBrewer   1.1-3  yaml   2.3.5,   memoise   2.0.1,    latticeExtra   0.6-30, stringi   1.7.8,    e1071   1.7-11, pkgbuild   1.3.1,   rlang   1.1.0,  pkgconfig   2.0.3, evaluate   0.17,    purrr   0.3.5,  htmlwidgets   1.5.4   processx   3.7.0,   tidyselect   1.2.0, parallelly   1.32.1   magrittr   2.0.3,   R6   2.5.1, generics    0.1.3,   profvis   0.3.7,   DBI   1.1.3,  pillar   1.8.1, units   0.8-0,  tibble   3.1.8, crayon   1.5.2,    interp   1.1-3, KernSmooth   2.23-20  utf8   1.2.2,   rmarkdown   2.17,   urlchecker   1.0.1, jpeg   0.1-10,  progress   1.2.2,   usethis   2.1.6,    grid   4.2.1,   callr   3.7.2, digest   0.6.31,    classInt   0.4-7,   webshot   0.5.4,    xtable   1.8-4, httpuv   1.6.6,    stats4   4.2.1, munsell   0.5.0,    viridisLite   0.4.1,   sessioninfo   1.2.2 
 2. ArcGis version 10.7.2, and ArcGis business analyst, with the USA Local Composite locator included.
  3. Grass GIS version 7.8.3.

### Data Pre-processing

As ZTRAX data may contain coordinates that do not accurately represent a property's true location, we conduct a comprehensive geocoding process to obtain a geospatial dataset with accuracy. 

The geocoding process translates a street address into its precise latitude and longitude coordinate. It is executed through ArcGIS 10.7.2 and the Business Analyst toolbox. The user will need to load the "USA Local Composite locator" when asked to select geolocator. In our tests, the geocoding program processed nearly 1 million addresses within 2 hours. 

### Viewshed Calculations

The viewshed calculations for all windmills are processed through an integration between GRASS GIS with R software. In the replication kit, we provide a replication code file .\program\1-calculate_viewshed.R that completes these calculates. 

A brief outline of the calculation process is as follows:
1. Import the windmill data and the DEMS into GRASS GIS. The DEMs are segmented into 30 degree by 30 degree tiles in their original format.
2. Re-project the DEMs tiles to EPSG:3857 (WGS 84 Pseudo Mercator in meter unit), and merge the tiles into a singular surface raster map.
3. Convert the windmill data into spatial format and re-project it to the same projection.
4.  Partition the windmill data into 100km by 100km grids for parallel purpose. This results in 480 grids each of which containing at least one wind turbine.
5.  Iterate the viewshed calculation over the grids sequentially. For each grid, define a buffer within 100km radius of the grid and set it as the analysis region. Then compute the viewsheds for wind turbines inside the grid in parallel, using the windmill's hub height, setting the observer's height as 1.75m, designating the maximum visibility distance as 10km, and factoring in the Earth's curvature.
6.  Export the raster map of viewsheds, which identifies areas from where a wind turbine is visible. Save the viewshed raster in output.

In our tests, the viewshed calculation of a sample wind turbine took a few seconds. We established an integration of GRASS GIS with R to automate this calculation process and facilitate parallel programming. The cumulative size of viewshed raster files is 91.9 gigabytes for all windmills in the sample. 

### Mapping Property to Viewsheds

We map properties with windmill viewsheds based on their geocoded location.  In the replication kit, we provide a file of replication code .\program\2-map_ztrax.R that completes the mapping process.

Encompassing more than 300 million housing transactions and viewshed rasters for over 60,000 wind turbines, this poses substantial computational challenge. To manage this, we employ parallel processing for the mapping operation. We identify 6 key variables that will later determine the DiD indicators. These include: 1) the visibility of a wind turbine from the property at the time of the transaction; 2) the property's potential visibility of a wind turbine if transacted today; 3) the distance to the nearest visible wind turbine at the time of the transaction; 4) the distance to the nearest wind turbine, regardless of visibility, at the time of the transaction; 5) the distance to the nearest visible wind turbine as of today; and 6) the distance to the nearest wind turbine, regardless of visibility, as of today. 

### Data Analysis

We provide the replication codes .\program\3_1-turbine_data_summ.R, .\program\3_2-ztrax_data_summ.R, and .\program\4_reg.R that generate all the figures and tables in the main text and the SI. 

Note that to compile the file, the user will need to acquire a token from the census website and add it to the program.

## Contact

Please contact Wei Guo (wei.guo@cmcc.it) for any questions regarding the code or data files.

Please see the paper for more information on the codes and data. If you use these codes files or data, please **CITE** this paper as the source

## Reference

1. B. Hoen et al., United States Wind Turbine Database. U.S. Geological Survey, American Clean Power Association, and Lawrence Berkeley National Laboratory data release. https://doi.org/10.5066/F7TX3DN0. Deposited 10 August 2022.

2. Zillow Group, Zillow Transaction and Assessment Dataset (ZTRAX). Zillow. https://www.zillow.com/ztrax. Deposited 1 September 2022.

3. A. Jarvis, H. Reuter, A. Nelson, E. Guevara, Hole-filled seamless SRTM data V4. International Centre for Tropical Agriculture (CIAT). https://srtm.csi.cgiar.org. Deposited 1 January 2022.
  
4. MIT Election Data and Science Lab, U.S. President 1976–2020. Harvard Dataverse. https://doi.org/10.7910/DVN/42MVDX. Deposited 10 August 2022.



