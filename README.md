# Sacramento-San Joaquin Delta Water Imports and Exports R Shiny App Documentation

## Introduction
The [Sacramento Water Allocation Model (SacWAM)](https://www.waterboards.ca.gov/waterrights/water_issues/programs/bay_delta/sacwam/) is a hydrologic and system operations model developed by the Stockholm Environment Institute (SEI) and State Water Resources Control Board (State Water Board) to assess potential revisions to instream flow and other requirements in the Bay-Delta Watershed including the current Sacramento/Delta review and update of the 2006 Water Quality Control Plan for the San Francisco Bay/Sacramento-San Joaquin Delta Estuary (2006 Bay-Delta Plan). SacWAM was developed using the Water Evaluation And Planning (WEAP) software platform and is intended to be user-friendly and usable for efficient comparison of multiple scenarios.

## App Description
This app uses SacWAM results to visualize how imports and exports of water to and from the Sacramento-San Joaquin Delta change depending on water year type. SacWAM classifies water years as follows:

Water-Year-Class | Code
---|---
Wet | 1
Above Normal | 2
Below Normal | 3
Dry | 4
Critical | 5

More documentation on water year type classifications can be found in Chapter 7: https://www.waterboards.ca.gov/waterrights/water_issues/programs/bay_delta/sacwam/docs/sacwam_documentation_beta_0.2.pdf.

The water year types for water years 1921-2014 are avaialable in the Excel postprocessor file available at this link: (https://www.waterboards.ca.gov/waterrights/water_issues/programs/bay_delta/sacwam/sacwam_download.html/) in the "Water Year Types" tab.

## Dayflow Stations
The flow stations chosen to represent Delta water imports and exports are based on the Department of Water Resources Dayflow Program (https://water.ca.gov/Programs/Environmental-Services/Compliance-Monitoring-And-Assessment/Dayflow-Data). A map of the stations for which data is utilized in the app can be found at this [link] (file:///C:/Users/JSpector/Documents/Delta%20Imports%20Exports%20Shiny%20App/map.html). Below is a table of the station names/locations and whether they are classified as import or export stations:

Station Name | Classification
--- | ---
Sacramento River at Freeport | Imports
Yolo Bypass at Woodland | Imports
South Putah Creek | Imports
Consumnes River at Michigan Bar | Imports
Delta Cross Channel | Imports
Georgiana Slough | Imports
Mokelumne River at Woodbridge | Imports
Calaveras River | Imports
CCC-Old River Pumping | Exports
CCC-Rock Slough Pumping | Exports
Clifton Court Inflow | Exports
San Joaquin River at Vernalis | Imports
Barker Slough Pumping | Exports
Central Valley Pumping | Exports


