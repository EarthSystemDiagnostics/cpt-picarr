# cpt-picarr
[![Build Status](https://travis-ci.org/EarthSystemDiagnostics/cpt-picarr.svg?branch=master)](https://travis-ci.org/EarthSystemDiagnostics/cpt-picarr)

Take a look at the basic UI prototype: https://twollnik.shinyapps.io/cpt_picarr/ 
 
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
          -> processingOptions
      -> data
          -> xyz
              -> dataset.info (name, additional information, device)
              -> sampleDescription.csv
              -> processingOptions.csv
              -> raw.csv (use name of uploaded file)
              -> processed.csv
          -> ...
  -> Project B
  ...
```