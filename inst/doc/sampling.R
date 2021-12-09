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

## ----sim, message = FALSE, warning = FALSE, error = TRUE----------------------
# Load rassta and terra packages
library(rassta)
library(terra)
# Single-layer SpatRaster of stratification units
su <- rast(paste(d, "/su.tif", sep = ""))
# Multi-layer SpatRaster with spatial signatures of classification units
clim.sig <- rast(list.files(d, pattern = "climate_", full.names = TRUE)) # For climatic units
mat.sig <- rast(list.files(d, pattern = "material_", full.names = TRUE)) # For material units
terr.sig <- rast(list.files(d, pattern = "terrain_", full.names = TRUE)) # For terrain units
# Landscape similarity to stratification units
su.ls <- similarity(su.rast = su, sig.rast = c(clim.sig, mat.sig, terr.sig),
                    su.code = list(climate = c(1, 1),
                                   material = c(2, 2),
                                   terrain = c(3, 3)
                                   )
                  )

## ----obs, message = FALSE, warning = FALSE, error = TRUE----------------------
# SpatVector with SOC observations for stratification units
socobs <- vect(paste(d, "/soc.shp", sep = ""))
# Representative SOC observation for each stratification unit
su.obs <- observation(su.rast = su, obs = socobs, col.id = 1, col.resp = 2,
                      method = "mls", ls.rast = su.ls$landsim
                      )
# Information about sample and representative SOC observations
socobs
su.obs$su_repobs.sp

## ----dist, fig.show = 'hold', fig.height = 4, fig.width = 7, message = FALSE, warning = FALSE, error = TRUE----
# Set graphics arrangement
par(mfrow = c(1,2))
# Plot stratification units and response observations
plot(su, type = "classes", col = hcl.colors(56, "spectral"), legend = FALSE,
     mar = c(3, 2, 3, 1.5), main = paste("Soil Organic Carbon Observations"),
     fun = function() c(points(socobs, pch = 21, bg = rgb(0,1,0,1)),
                        points(su.obs$su_repobs.sp, pch = 21, bg = rgb(0,0,1,1))
                      )
    )
# Set new graphics arrangement
par(mar = c(2, 1.5, 1.5, 1.5))
# Plot histogram of soil organic carbon values from all observations
hist(socobs$soc, 4, col = rgb(0,1,0,0.8), main = "Soil Organic Carbon (%)", xlab = "")
# Plot histogram of soil organic carbon values from representative observations
hist(su.obs$su_repobs.sp$soc, 4, add = T, col = rgb(0,0,1,0.9))
# Add legend
legend("topright", legend = c("Sample", "Representative Observations"),
       col = c(rgb(0,1,0,0.8), rgb(0,0,1,0.9)), pch = 20, bty = "n", pt.cex = 2,
       cex = 0.6, text.col = "black", horiz = F, inset = c(0, 0.05)
      )

## ----locs, message = FALSE, warning = FALSE, error = TRUE---------------------
# Representative sampling location and its buffer area for each stratification unit
su.samp <- locations(ls.rast = su.ls$landsim, su.rast = su, method = "buffer")
# Information about representative sampling locations and corresponding buffer areas
su.samp$locations
su.samp$buffers

## ----spat, fig.show = 'hold', fig.height = 4, fig.width = 7, message = FALSE, warning = FALSE, error = TRUE----
# Set graphics arrangement
par(mfrow = c(1,2))
# Plot stratification units, representative sampling locations and buffer areas
plot(su, type = "classes", col = hcl.colors(56, "spectral"), legend = FALSE,
     mar = c(3, 2, 3, 1.5), main = "Representative Sampling Locations",
     fun = function() c(polys(su.samp$buffers, col = rgb(0,1,0,0.5)),
                        points(su.samp$locations, pch = 21, col = "black",
                               bg = rgb(0,1,0,1)
                              )
                      )
    )
# Set new graphics arrangement
par(mar = c(2, 1.5, 1.5, 1.5))
# Plot histogram of landscape similarity values at sampling locations
hist(su.samp$locations$land_sim, 4, main = "Landscape Similarity")

## ----clean, message = FALSE, warning = FALSE, error = TRUE--------------------
unlink(c(paste(o, "/wasoil.zip", sep = ""), d), recursive = TRUE)

