# Cpt-Picarr

[![Build Status](https://travis-ci.org/EarthSystemDiagnostics/cpt-picarr.svg?branch=master)](https://travis-ci.org/EarthSystemDiagnostics/cpt-picarr)
![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)

## Description

Cpt-Picarr is a graphical tool to help you do stable water isotope analysis. It is built in R with [Shiny](https://shiny.rstudio.com/) and has the following main features:

- Create sample description files to read into your measurement device(s)
- Improve the quality of your measurement data with calibration, memory correction, and drift correction
- Visualize the measurement data
- Analyze the performance of your measurement instruments

Cpt. Picarr relies on the [piccr](https://github.com/EarthSystemDiagnostics/piccr) package for calibration, memory correction, and drift correction.

In Cpt. Picarr your measurement data is divided into projects. Each project contains all datasets belonging to specific research task (e.g. one expedition, one researcher, one paper).

The best way to get to know Cpt. Picarr is to start it up, take a look at the demo project we have provided, and play with it. 

You can find some more information and screenshots in [this presentation](https://docs.google.com/presentation/d/1ndO-RKQ9BWW8kfVON3J0AhX6gkiNfV7bhQ_AIp_WHd0/edit?usp=sharing).

## Setup

### Run Cpt-Picarr locally

1. Run the install script `install.sh` ([How to run a shell script](https://www.cyberciti.biz/faq/run-execute-sh-shell-script/))
2. Configure the app. Open the file `config.yaml` and set `BASE_PATH` to the directory where you would 
   like to store the data associated with the app.
3. Start the app: `Rscript -e "shiny::runApp('R')"` (You may have to pass `launch.browser = TRUE` to make your browser open Cpt-Picarr automatically.)

### Run Cpt-Picarr with shiny server

1. Setup [Shiny server](https://www.rstudio.com/products/shiny/shiny-server/) on one of your servers.
   Talk to your IT department about this.
2. Proceed with the steps listed under "Run Cpt-Picarr locally".

## Bugs and Questions

If you encounter bugs or have any questions, feel free to [open an issue](https://github.com/EarthSystemDiagnostics/cpt-picarr/issues).

## How does Cpt-Picarr store data?

In the config.yaml users specify a `BASE_PATH`. This should be an absolute path to a directory that will contain
all data associated with Cpt-Picarr. The structure of the `BASE_PATH` directory will be as follows:

```
BASE_PATH
  -> devices.json (information on known measurement devices)
  -> processingOptions
      -> 56789.oikhj56789 (the folder name is the unique identifier)
          -> processingOptions.csv
          -> sampleDescription.csv
      -> ...
  -> Project A
      -> projectInfo.json (project name, people involved, Additional info, expedition date)
      -> templates
          -> sampleDescription
              -> template 1
              -> ...
          -> processing
              -> template A
              -> ...
      -> data
          -> dataset A
              -> fileInfo.json (dataset name, additional information, device)
              -> sampleDescription.csv
              -> processingOptions.csv
              -> raw.csv (uses name of uploaded file)
              -> processed.csv
          -> ...
  -> Project B
  ...
```

## Workflows

### 1. Starting a new project

1. On the page "Home": Create a new project.
2. On the page "Manage Devices": Add the devices that were used to measure your data if they are not known already.
3. On the page "Generate a sample description": Create a sample description template and a processing options template.
4. On the page "Generate a sample description": Generate a sample description specific to the next measurement.
5. Prepare the samples and start the measurement.
6. On the page "Upload measurement data": Upload the measurement data obtained from the previous step.
7. On the page "Process isotope measurement data": Process the uploaded data (this includes calibration, drift and memory correction) and take a look at the visualizations at the bottom of the page.
8. Continue with the next measurement.

### 2. Analyzing instrument performance

1. Take a look at the main pages for the different projects to make sure that all the data that you want to take into account has been processed already. (You can navigate to the main project pages by loading a project on the page "Home")
2. Go to page "Home" and click on "Go to page 'Instrument performance'".
3. Select one or multiple devices and a time span that interest you.
4. Take a look at the plots on the bottom of the page.

### 3. Using existing measurement data

1. Create a new project.
2. For each measurement file do
    - On the page "Generate a sample description": Generate a sample description and set the processing options for this measurement file.
    - Copy the unique identifier displayed next to the button "Download sample description".
    - Append the unique identifier to every entry in the column "Identifier 2" in the measurement file. Separate the original Identifier 2 and the unique identifier with an underscore ("_").
    - On the page "Upload measurement data": Upload the modified measurement file.
3. Process datasets or prepare further measurements.

