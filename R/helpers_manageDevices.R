library(tidyverse)

getDevices <- function(basePath = BASE_PATH){
  
  path <- file.path(basePath, "devices.json")
  if (file.exists(path))
    list.load(path)
  else
    list()
}

getDevicesAsStrings <- function(basePath = BASE_PATH){
  map(getDevices(basePath), ~ sprintf("%s (%s)", .$name, .$code))
}