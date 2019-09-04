# use as style attribute to make blue buttons
blue <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"

# store data in this directory
BASE_PATH = "./appdata"

# TODO: move this to a setup script
dir.create(BASE_PATH, showWarnings = FALSE)
dir.create(file.path(BASE_PATH, "templates"), showWarnings = FALSE)
dir.create(file.path(BASE_PATH, "templates", "sample_description"), showWarnings = FALSE)
dir.create(file.path(BASE_PATH, "templates", "processing"), showWarnings = FALSE)
dir.create(file.path(BASE_PATH, "data"), showWarnings = FALSE)