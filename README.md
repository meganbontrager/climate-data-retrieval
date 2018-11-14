# Climate data retrieval

Code for retrieving/reformatting climate data from ClimateNA, the Flint Basin Characterization model, and PRISM. Please let me know if you notice things that are out of date or incorrect. 

These scripts will guide you through:

1. Getting your location data into the input format required for each data source.

2. Plugging your location data into data sources to get climate time series.

3. Reformating these time series so that you can calculate a variety of climate variables.

Sample data are localities of *Caulanthus anceps* downloaded from the Consortium of California Herbaria. Your data should contain these columns:
- id: a unique id for each row/location
- latitude: in decimal degrees
- longitude: in decimal degrees
- elev_m: (optional) elevation in meters
- year: (optional) an event year, such as year of collection of a specimen
- month: (optional) an event month, such as month of collection as a specimen

If you just want  normals (averages) using bioclim is the easiest option (and it has global coverage), but if you want data from specific months/years, you'll need to use another source (you can always calculate long-term normals from these).

There are 3 options for California and all are pretty good. Make sure to read the terms of use for whichever you use and cite them appropriately in any research products.

1. PRISM (http://prism.oregonstate.edu/explorer/bulk.php)
     - Other data sources often downscale this dataset
     - Pros: easy download from web, very recent data is available (there is also a bulk file download option, but I haven't explored it)
     - Cons: can only download 15 years of data for 500 locations at once from web, so need to merge multiple files
     - Geographic coverage: Contiguous US and a bit into Canada and Mexico
     - Geographic resolution: 4km
     - Temporal coverage: 1895 - a few months before present
     - Temporal resolution: monthly (daily also available, but in smaller batches)
     
2. ClimateNA (https://sites.ualberta.ca/~ahamann/data/climatena.html)
     - Pros: easy bulk download if you install the software, can include elevation (but not required), future climate projections are also available
     - Cons: software must be installed on PC (or can be installed on a Mac with Wine)
     - Geographic coverage: North America (other programs are available for other regions: ClimateEU, ClimateSA, ClimateAP)
     - Geographic resolution: downscaled to point estimate (from CRU dataset)
     - Temporal coverage: 1901 - 2015 (?) 
     - Temporal resolution: monthly 

3. Flint BCM (https://ca.water.usgs.gov/projects/reg_hydro/basin-characterization-model.html, download from https://cida.usgs.gov/thredds/CA-BCM-Catalog.html)
     - Pros: specifically downscaled to California's topography
     - Cons: hardest dataset to work with, raw files are big, data extraction methods are new to me and probably most ecologists, I still haven't diagnosed why data doesn't extract for some locations
     - Geographic coverage: California (drainage boundaries)
     - Geographic resolution: 270 m (downscaled from PRISM and oother data sources)
     - Temporal coverage: 1895 - 2010, additional data available for 2011-2016 
     - Temporal resolution: monthly 


