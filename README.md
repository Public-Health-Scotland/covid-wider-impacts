# Code to produce the [Covid-19 wider impacts dashboard](https://scotland.shinyapps.io/phs-covid-wider-impact/).

### data_prep folder

- 1.running_data_prep_scripts: This script is the one to be used for the update process of most sections. There are a number of scripts for each set of datasets containing the functions the  script runs. 
- Some sections don't use the above script and use their own topic script.
- functions_packages_data_prep: functions, packages and fileptahs used accross all the data preparation.
- final_app_files: script that includes all the data files used by the app and the right version (datestamped) of each one. This script should be run to start working on the app for the first time or before deploying the dashboard.

### shiny_app folder
Folder includes all the code used to produce the Shiny app. In the root folder, you can find the key scripts for the app (ui, server and global) and the google analytics tracking code.

- server folder: Each tab or group of tabs has its own script. There is also a functions_server script with common functions/modules used throughout the app.
- ui folder: Each tab or group of tabs has its own script. There is also a modules_ui script with common functions/modules used throughout the app.
- www folder: Includes images and logos used in the dashboard and the CSS stylesheet.
- Other folders: When working with the app other folder should be present: data and admin. These won't be commited to Git. Another folder called rsconnect could be present if you have previously deployed the dashboard.

### google_analytics folder
Code to extract the statistics on visits to the dashboard from Google Analytics and to produce a report with some of the key figures.

### data folder
It includes a few lookups used in the data preparation

### open data
create_open_data_files script to produce files for PHS open data plaform.
