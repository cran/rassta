## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data, fig.show = 'hold', fig.height = 2.5, fig.width = 2.8, message = FALSE, warning = FALSE, error = TRUE----
# Compressed folder with files from rassta’s installation folder
wasoil <- system.file("exdat/wasoil.zip", package = "rassta")
# Directory for temporary files
o <- tempdir()
# Copy compressed folder to directory for temporary files
file.copy(from = wasoil, to = o)
# Extract files to subfolder
d <- paste(o, "/rassta", sep = "")
unzip(paste(o, "/wasoil.zip", sep = ""), exdir = d)

## ----sim, fig.show = 'hold', fig.height = 5.4, fig.width = 6, message = FALSE, warning = FALSE, error = TRUE----
# Load rassta and terra packages
library(rassta)
library(terra)
# Multi-layer SpatRaster with spatial signatures of classification units
clim.sig <- rast(list.files(d, pattern = "climate_", full.names = TRUE)) # For climatic units
mat.sig <- rast(list.files(d, pattern = "material_", full.names = TRUE)) # For material units
terr.sig <- rast(list.files(d, pattern = "terrain_", full.names = TRUE)) # For terrain units
# Single-layer SpatRaster of stratification units
su <- rast(paste(d, "/su.tif", sep = ""))
# Landscape similarity to stratification units
su.ls <- similarity(su.rast = su, sig.rast = c(clim.sig, mat.sig, terr.sig),
                    su.code = list(climate = c(1, 1),
                                   material = c(2, 2),
                                   terrain = c(3, 3)
                                   )
                  )
# Plot landscape similarity to stratification unit '111' and its spatial signatures
plot(c(su.ls$landsim[[1]], clim.sig[[1]], mat.sig[[1]], terr.sig[[1]]),
     col = hcl.colors(100, "Oslo", rev = TRUE), nc = 2, mar = c(1.5, 1.5, 1.5, 3.5)
    )

## ----sucode, fig.show = 'hold', message = FALSE, warning = FALSE, error = TRUE----
# Consider the following nested list:
su.code <- list(climate = c(1, 1), material = c(2, 2), terrain = c(3, 3))
# The stratification units are composed of classification units from three landscape factors
names(su.code)
# For climate, the classification units are represented by the first digit in the numeric code
su.code$climate
# For material, the classification units are represented by the second digit in the numeric code
su.code$material
# For terrain, the classification units are represented by the third digit in the numeric code
su.code$terrain
# Thus, the numeric code of the stratification units 111 and 468 means:
su <- c(111, 468)
su[1]   # 'climate' = 1, 'material' = 1, and 'terrain' = 1
su[2]   # 'climate' = 4, 'material' = 6, and 'terrain' = 8

## ----clean, message = FALSE, warning = FALSE, error = TRUE--------------------
unlink(c(paste(o, "/wasoil.zip", sep = ""), d), recursive = TRUE)

