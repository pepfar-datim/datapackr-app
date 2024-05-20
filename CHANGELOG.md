## May 20, 2023
 * Changed methods to upload files to PDAP/PAW
 * Upgraded dependencies

## Aug 4, 2023
 * Upgraded to latest version of datapackr
 * Fixes issue where blank sheets were being flagged as having invalid PSNUs. 

## May 24, 2023
 * Fixed issue with blank values not being flagged in the PSNUxIM tab.

## April 14, 2023
 * Fixed issue with flatpack downloads with standalone PSNUxIM tools

## April 12, 2023
 * Fixed issue with Year 2 pivot 
 * Fixed issue with HTS Recency for COP23 tools
 * Updated validation rules
 
## April 12, 2023
 * Update to datapackr 6.2.3
 * Adds functionality to allow for updating PSNUxIM target values
 
## April 4, 2023
 * Update to datapackr 6.2.1
 
## March 30, 2023
 * Fixed issue with PAW exports when data was allocated
 * Applied rounding to Year 2 exports
 * Fixed issue in Year 2 pivot with doubling of PLHIV data
 * Fixed issue with AGYW_PREV not being exported
 
## March 24, 2023
 * Fixed issue with KP Pyramid chart
 * Better analytics with TST/PSNU combinations
 
## March 23, 2023
 * Enhanced support for COP23 PSNUxIM tab parsing and generation

## March 20, 2023,
 * Fixed an issue with subnat pyramids chart for COP23 

## March 09, 2023
 * Add a pivot table for COP23 Year 2 data
 * Adds initial support for parsing COP23 datapacks
 * Update to datapackr 6.2.0
 
## Jan 26, 2023
 * Update to Datapackr 6.0.0

## May 13, 2022
 * Update to datapackr 5.2.0
 * Remove processing for COP20 OPU
## May 12, 2022
 * Update to datapackr 5.1.13
 * Fix alignment of app memo tables

## Mar 04, 2022
 * Update to datapackr 5.1.4

## Feb 24, 2022
 * Update to datapackr 5.1.3
 * Suppress HTS_RECENT in modality visuals
 * Replace ansistrings with fansi
 * Downgrade openxlsx to 4.2.3

## January 23, 2022
* Remove HTS_TST/KP_PREV validation rule

## January 17, 2022
* Update to datapackr 5.1.0
* Initial updates to process COP22 datapacks
* Enhanced app analytics
* Major reorganization of code

## December 1, 2021
* Update to datapackr 5.0.2
* Add a reset button to the comparison pivot
* Enhanced logging for app analytics
* Remove agency totals from the comparison
* Various bug fixes

## October 4, 2021
* Update to datapackr 5.0.0
* Fix bug when logging out
* Add feature to fetch cached data values
* Improved comparison functionality
* Code linting

## May 12, 2021
* Fix bug on VLS testing visual
* Alter UI to provide a dropdown for output types
* Update to Datapackr 4.5.1

## May 11, 2021
* [DP-247](https://jira.pepfar.net/browse/DP-247) AGYW_PREV was doubling for DREAMS countries who set targets below the PSNU level
* TX_TB age disaggs were not being displayed

## May 1, 2021
* Update to datapackr 4.5.0
* Include analytics checks in app

## April 29, 2021
* Added initial app analytics function

## April 28, 2021
* Better error checking with missing HTS modalities

## April 26, 2021
* Fix bug in HTS summary table

## April 23, 2021
* Update to datapackr 4.3.7
* Update to datapackr 4.4.0
* Update to datapackr 4.4.0


## April 22,2021
* [DP-226](https://jira.pepfar.net/browse/DP-226) Datpacks with multiple country UIDs cause an error

## April 16, 2021
* [DP-220](https://jira.pepfar.net/browse/DP-220) Visuals in Self-Service App broken for certain OPU Workbooks
* [DP-221](https://jira.pepfar.net/browse/DP-221) HTS Yield Rates graph shows "Total"
* [DP-219](https://jira.pepfar.net/browse/DP-219) Fix Fiscal year in HTS charts 

## April 15, 2021
* [DP-216](https://jira.pepfar.net/browse/DP-216) Fix issue related to incorrect indicator group being used in the prioritization table. 
* Update datapackr to 4.3.4
* Add uuid package
* Add version information to login screen

## April 14, 2021
* [DP-214](https://jira.pepfar.net/browse/DP-214) Fix issue with HTS tables and visuals related to known positives being included in calculations.

## April 13, 2021
* Fixed issue related to validation rules and dedupe values

## April 12, 2021
* Fixed issue related to duplicate uploads to PAW
* Fixed issue related to HTS Summary tables not displaying
* Minor improvements to SNU Summary tables

## April 12, 2021
* Add announcements text box on login screen
* Fixed unhandled error in DP-211

## April 9, 2021
* Add functionality for CSO download output
