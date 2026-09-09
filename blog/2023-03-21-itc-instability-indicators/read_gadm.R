## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ITC Fragility
## src/read_gadm.R
## Jack Gregory
## 27 February 2023
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION -----------------------------------------------------------------------------------
## This script provides a Database of Global Administrative Areas (GADM -- https://gadm.org) 
## geodata read function for the ITC Fragility project.

## It has the following dependencies:
##  - dplyr
##  - fs
##  - here
##  - purrr
##  - zip


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  27Feb2023 Jack Gregory  Initial version


### START CODE ###


# read_gadm ---------------------------------------------------------------------------------------
## Read GADM shapefiles from within a zip archive.

## colour = Colour as a string in set {"red","blue","green"}
## text = Text to be coloured as a string

read_gadm <- function(zipfile, level=0L, exdir=fs::path_dir(zipfile)) {
  
  ## Assertions
  stopifnot(
    length(zipfile)==1,
    fs::file_exists(zipfile) & fs::path_ext(zipfile)=="zip",
    level %in% list(0L,1L,2L,3L,"national","subnational"),
    length(exdir)==1,
    fs::dir_exists(exdir)
  )
  
  ## Convert level to integer
  if (is.character(level)) {
    level <- switch(level,
                    "national"=0L,
                    "subnational"=1L)
  }
  
  ## Get file list
  files <- zip::zip_list(zipfile=zipfile) |>
    dplyr::filter(grepl(paste0("_", level, "\\."), filename)) |>
    dplyr::pull(filename)
  
  ## Unzip files
  zip::unzip(
    zipfile=zipfile,
    files=files,
    exdir=exdir
  )

  ## Read geodata
  shp <- grep("\\.shp", files, value=TRUE)
  if (length(shp)!=1) {
    stop("More than one shapefile exists in the set of unzipped files.")
  }
  sf <- fs::path(exdir, shp) |>
    sf::st_read()
  
  ## Remove unzipped files
  files |>
    purrr::map_chr(~here::here(exdir, .x)) |>
    fs::file_delete()
  
  ## Return geodata
  return(sf)
}


### END CODE ###

