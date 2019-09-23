# Cpt-Picarr

[![Build Status](https://travis-ci.org/EarthSystemDiagnostics/cpt-picarr.svg?branch=master)](https://travis-ci.org/EarthSystemDiagnostics/cpt-picarr)
![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange)

Take a look at the basic UI prototype: https://twollnik.shinyapps.io/cpt_picarr/ 

## Description

Cpt-Picarr is a tool to help you with stable water isotope analysis. It was originally developed 
for Picarro measurement devices but may also work for other kinds of devices.

TODO: insert screenshots

## Setup

### Run Cpt-Picarr locally

1. Clone the repository: `git clone https://github.com/EarthSystemDiagnostics/cpt-picarr.git`
2. Check system requirements.
  - Linux
  - R version >= 3.6.
  - `sudo apt install build-essential libxml2-dev libssl-dev libcurl4-openssl-dev`
3. Install dependencies: `Rscript install.R`.
4. Configure the app. Open the file `config.yaml` and set `BASE_PATH` to the directory where you would 
   like to store the data associated with the app.
5. Start the app: `Rscript -e "shiny::runApp('R')"`

### Run Cpt-Picarr with shiny server

1. Setup [Shiny server](https://www.rstudio.com/products/shiny/shiny-server/) on one of your servers.
   Talk to your IT department about this.
2. Proceed with the steps listed under "Run Cpt-Picarr locally".
 

## How is the data stored?

```
BASE_PATH
  -> processingOptions
      -> 56789.oikhj56789
          -> processingOptions.csv
          -> sampleDescription.csv
      -> ...
  -> Project A
      -> project.info (name, people involved, Additional info, expedition date)
      -> templates
          -> sampleDescription
          -> processing
      -> data
          -> xyz
              -> fileInfo.json (name, additional information, device)
              -> sampleDescription.csv
              -> processingOptions.csv
              -> raw.csv (use name of uploaded file)
              -> processed.csv
          -> ...
  -> Project B
  ...
```

## Use Cpt-Picarr with existing measurement data

1. Create a project using Cpt-Picarr's user interface. Let `projName` denote the name that you chose for the project.
2. For every measurement file do the following:
    1. Create a folder `{BASE_PATH}/{projName}/data/{name of dataset}`
    2. Copy the measurement file to that folder.
    3. Create the file `processingOptions.csv` in the folder.

TODO: Make sure that `sampleDescription.csv` is not needed.
