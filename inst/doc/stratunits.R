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

## ----climclass, fig.show = 'hold', fig.height = 6, fig.width = 3.9, message = FALSE, warning = FALSE, error = TRUE----
# Load rassta and terra packages
library(rassta)
library(terra)
# Multi-layer SpatRaster with 3 sets of classification units
cu <- c("climate.tif", "material.tif", "terrain.tif")
cudir <- paste(d, cu, sep = "/")
all.cu <- rast(cudir)
# Plot the sets of classification units
par(mfrow = c(3, 1))
plot(all.cu[[1]], type = "classes", main = "Climatic Classification Units", 
     col = hcl.colors(4, "spectral"), mar = c(1.5, 1.5, 1.5, 16),
     levels = c("1. Highest Rainfall and Lowest Temperature",
                "2. High Rainfall and Low Temperature",
                "3. Low Rainfall and High Temperature",
                "4. Lowest Rainfall and Highest Temperature"
              )
    )
plot(all.cu[[2]], type = "classes", main = "Soil Parent Material Units", 
     col = hcl.colors(6, "spectral"), mar = c(1.5, 1.5, 1.5, 16),
     levels = c("1. Igneous", "2. Sedimentary",
                "3. Alluvium - Moderately weathered ",
                "4. Alluvium - Somewhat weathered",
                "5. Alluvium - Rich in organic matter",
                "6. Alluvium - Rich in clay and organic matter"
                )
    )
plot(all.cu[[3]], type = "classes", main = "Terrain Classification Units", 
     col = hcl.colors(8, "spectral"), mar = c(1.5, 1.5, 1.5, 16),
     levels = c("1. Summit", "2. Shoulder", "3. Backslope",
                "4. Backslope - Steep", "5. Footslope",
                "6. Footslope - Steep", "7. Toeslope", "8. Floodplain"
                )
)

## ----strata, fig.show = 'hold', fig.height = 2.9, fig.width = 5, message = FALSE, warning = FALSE, error = TRUE----
# Stratification units from the intersection of classification units
su <- strata(cu.rast = all.cu)
# Plot SpatRaster of stratification units
plot(su$su.rast, type = "classes", main = "Stratification Units",
     col = hcl.colors(length(unique(su$su.rast)[, 1]), "spectral"),
     plg = list(ncol = 4), mar = c(1.5, 1.5, 1.5, 12)
    )

## ----code, fig.show = 'hold', fig.height = 2.9, fig.width = 5, message = FALSE, warning = FALSE, error = TRUE----
# Print multipliers used to code each stratification unit
su$code.mult

## ----clean, message = FALSE, warning = FALSE, error = TRUE--------------------
unlink(c(paste(o, "/wasoil.zip", sep = ""), d), recursive = TRUE)

