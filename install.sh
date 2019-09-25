echo Step 1: installing system dependencies

apt install build-essential libxml2-dev libssl-dev libcurl4-openssl-dev

echo Step 2: installing R packages

Rscript \
  -e "install.packages('devtools')" \
  -e "install.packages('shiny')" \
  -e "install.packages('rhandsontable')" \
  -e "install.packages('tidyverse')" \
  -e "install.packages('futile.logger')" \
  -e "install.packages('testthat')" \
  -e "install.packages('digest')" \
  -e "install.packages('rlist')" \
  -e "install.packages('scales')" \
  -e "devtools::install_github('EarthSystemDiagnostics/piccr', ref = 'dev')"

echo done.
