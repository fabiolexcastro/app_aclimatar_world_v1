
# Load libraries ----------------------------------------------------------
library(pacman)
p_load(
  terra, sf, fs, tidyverse, shiny, leaflet, glue, geodata
)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Functions ---------------------------------------------------------------
to.num <- function(x){
  y <- x * 1
  y <- terra::ifel(y == 2, 1, y)
  y <- terra::ifel(y > 1, y - 1, y )
}

# ISO ---------------------------------------------------------------------

## Coffee ----------------------------------------
cffe.aez <- '//catalogue/aclimatar-world/aclimatar-w_v1/workspace_coffee/rf/output/run_2/results/process' |> 
  dir_ls(regexp = '.tif$') |> 
  as.character() |> 
  grep(paste0(c('mixed-bsl', 'mix_ftr-modal.tif'), collapse = '|'), x = _, value = T) |> 
  rast()

names(cffe.aez) <- basename(sources(cffe.aez))
names(cffe.aez) <- c('Future', 'Baseline')

# cffe.imp <- rast('//catalogue/aclimatar-world/aclimatar-w_v1/workspace_coffee/rf/output/run_2/results/process/rf-impactGradient_mdl_v2.tif')

## Cocoa ----------------------------------------
coco.aez <- '//catalogue/aclimatar-world/aclimatar-w_v1/workspace_cocoa/rf/output/results/process' |> 
  dir_ls(regexp = '.tif$') |> 
  as.character() |> 
  grep(paste0(c('mixed_bsl', 'mix_ftr-modal.tif'), collapse = '|'), x = _, value = T) |> 
  rast()

names(coco.aez) <- basename(sources(coco.aez))
names(coco.aez) <- c('Baseline', 'Future')

# coco.imp <- rhast('//catalogue/aclimatar-world/aclimatar-w_v1/workspace_cocoa/rf/output/results/process/rf-impactGradient_mdl_v2.tif')
names(coco.imp) <- 'Impact'

# Coffee ------------------------------------------------------------------

cffe.aez <- as.numeric(cffe.aez)

## To numeric 
cffe.aez[[1]] <- to.num(cffe.aez[[1]])
cffe.aez[[2]] <- to.num(cffe.aez[[2]])

## To make the stack 
cffe <- c(cffe.aez[[2]], cffe.aez[[1]], cffe.imp)

## To extract by mask
# cffe <- cffe |> 
#   terra::crop(shp) |> 
#   terra::mask(shp)

dir.create('./www')
terra::writeRaster(x = cffe, filename = glue('./www/rf_coffee_world.tif'), overwrite = TRUE)

# Cocoa -------------------------------------------------------------------

coco.aez <- as.numeric(coco.aez)

## To numeric 
coco.aez[[1]] <- to.num(coco.aez[[1]])
coco.aez[[2]] <- to.num(coco.aez[[2]])

## To make the stack 
coco <- c(coco.aez, coco.imp)

## To extract by mask
# coco <- coco |> 
#   terra::crop(shp) |> 
#   terra::mask(shp)

dir.create('./www')
terra::writeRaster(x = coco, filename = glue('./www/rf_cocoa_world.tif'), overwrite = TRUE)

