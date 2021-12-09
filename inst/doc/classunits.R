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

## ----terrvars, fig.show = 'hold', fig.height = 5, fig.width = 5.5, message = FALSE, warning = FALSE, error = TRUE----
# Load rassta and terra packages
library(rassta)
library(terra)
# Multi-layer SpatRaster with 4 terrain variables
var <- c("height.tif", "midslope.tif", "slope.tif", "wetness.tif")
vardir <- paste(d, var, sep = "/")
terr.var <- rast(vardir)
# Plot terrain variables
par(mfrow = c(2,2))
nc <- c("Zissou", "Batlow", "Lajolla", "Spectral")
for(i in 1:4){
  plot(terr.var[[i]], main = names(terr.var)[i],
       col = hcl.colors(100, nc[i]), mar = c(2, 2, 2, 3.5)
      )
}

## ----terrclas, fig.show = 'hold', fig.height = 3, fig.width = 2.8, message = FALSE, warning = FALSE, error = TRUE----
# Set seed
set.seed(963)
# Scale variables to mean = 0 and standard deviation = 1
terr.varscale <- scale(terr.var)
# With terra::aggregate(), reduce spatial resolution to reduce computing time
terr.varscale <- aggregate(terr.varscale, fact = 3)
# Dimensionality reduction and estimation of optimum k (max k to evaluate: 9)
terr.som <- som_gap(terr.varscale, xdim = 9, ydim = 9, K.max = 9)
# Plot results
plot(terr.som$SOM, main = "SOM Codes") # Self-Organizing Map's codes
par(mar = c(4.5, 4.5, 2, 1))
plot(terr.som$SOMgap, main = "Gap Statistic") # Gap statistic
abline(v = terr.som$Kopt) # Optimum k

## ----terrclasr, fig.show = 'hold', fig.height = 2.5, fig.width = 2.8, message = FALSE, warning = FALSE, error = TRUE----
# Create reference SpatRaster
rastref <- aggregate(terr.var[[1]], fact = 3)
# Rasterization of terrain SOM grid and terrain PAM clustering
terr.sompam <- som_pam(ref.rast = rastref, kohsom = terr.som$SOM,
                       k = terr.som$Kopt
                      )
# Plot results
plot(terr.sompam$sompam.rast[[1]], main = "Terrain Self-Organizing Map",
     mar = c(1.5, 1.3, 1.5, 3.3), col = hcl.colors(100, "spectral", rev = TRUE)
    )
plot(terr.sompam$sompam.rast[[2]], main = "Terrain Classification Units",
     type = "classes", col = hcl.colors(terr.som$Kopt, "spectral", rev = TRUE),
     mar = c(1.5, 2, 1.5, 2.5)
    )

## ----terrclasrec, fig.show = 'hold', fig.height = 2.5, fig.width = 2.8, message = FALSE, warning = FALSE, error = TRUE----
# Extract rasterized PAM solution
terr.cu <- terr.sompam$sompam.rast[[2]]
# With terra::zonal(), unit-wise mean value of terrain height
terrh <- aggregate(terr.var$height, fact = 3) # Match spatial resolution
terr.stat <- zonal(terrh, terr.cu, fun = mean)
# Order numeric IDs based on terrain height (descending)
terr.stat <- terr.stat[order(terr.stat$height, decreasing = TRUE), ]
# Column with original numeric IDs
terr.stat$CU <- seq(1, terr.som$Kopt)
# With terra::classify(), reclassify numeric IDs
terr.cu <- classify(terr.cu, terr.stat[, c(1,3)])
# Plot original and reclassified terrain classification units
plot(terr.sompam$sompam.rast[[2]], main = "Terrain Classification Units",
     type = "classes", col = hcl.colors(terr.som$Kopt, "spectral", rev = TRUE),
     mar = c(1.5, 2, 1.5, 2.5)
    )
plot(terr.cu, type = "classes", col = hcl.colors(terr.som$Kopt, "spectral"),
     main = "Reclassified Terrain Classification Units",
     mar = c(1.5, 2, 1.5, 2.5)
    )

## ----clean, message = FALSE, warning = FALSE, error = TRUE--------------------
unlink(c(paste(o, "/wasoil.zip", sep = ""), d), recursive = TRUE)

