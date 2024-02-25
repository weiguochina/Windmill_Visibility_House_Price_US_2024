ZTRAX data is offered by Zillow’s research team (https://www.zillow.com/ztrax). To access ZTRAX data, the user will need to first review and agree to the ZTRAX Data License Agreement, then complete the registration online. Once logged in, the user can request access to specific ZTRAX datasets. For acquisition, the user will need to be prepared to provide details on the intended use of the data.

As ZTRAX data may contain coordinates that do not accurately represent a property's true location, we conduct a comprehensive geocoding process to obtain a geospatial dataset with accuracy.

The geocoding process translates a street address into its precise latitude and longitude coordinate. It is executed through ArcGIS 10.7.2 and the Business Analyst toolbox. The user will need to load the "USA Local Composite locator" when asked to select geolocator. In our tests, the geocoding program processed nearly 1 million addresses within 2 hours.

After geocoding, we suggest the user saves a separate output in FST format for each state, naming the data for the state with FIPS code X as "X__ztrax.fst".