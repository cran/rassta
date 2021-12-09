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

## ----df, fig.show = 'hold', fig.height = 2.5, fig.width = 2.25, echo = FALSE, warning = FALSE, error = TRUE----
x <- rnorm(500, 60, 9)
plot(density(x), main = "PDF", xlab = "Variable")
xecdf <- ecdf(x)
qx <- quantile(x, probs = seq(0, 1, by = 0.01))
plot(qx, xecdf(qx), main = "ECDF", xlab = "Variable", ylab = "ECDF", type = "l")
iecdf <- ((xecdf(qx) - max(xecdf(qx))) * -1) + min(xecdf(qx))
plot(qx, iecdf, main = "iECDF", xlab = "Variable", ylab = "iECDF", type = "l")

## ----sdf, fig.show = 'hold', message = FALSE, warning = FALSE, error = TRUE----
# Load rassta and terra packages
library(rassta)
library(terra)
# Multi-layer SpatRaster with 2 climatic variables
var <- c("precipitation.tif", "temperature.tif")
vardir <- paste(d, var, sep = "/")
clim.var <- rast(vardir)
# Single-layer SpatRaster with 4 climatic classification units
clim.cu <- rast(paste(d, "/climate.tif", sep = ""))
# Automatic selection of statistical distribution functions
clim.difun <- select_functions(cu.rast = clim.cu,
                               var.rast = clim.var, 
                               mode = "auto"
                              )

## ----plots, fig.show = 'hold', fig.height = 1.9, fig.width = 7, message = FALSE, warning = FALSE, error = TRUE----
# Plot climatic classification units and variables
plot(c(clim.cu, clim.var), col = hcl.colors(100, "Spectral"), nc = 3,
     mar = c(1.5, 1.5, 1.5, 5)
    )
# Selected distribution functions
knitr::kable(clim.difun$distfun, filter = "none", selection = "none")

## ----pdf, fig.show = 'hold', fig.height = 2.89, fig.width = 6.8, message = FALSE, warning = FALSE, error = TRUE----
# Multi-layer SpatRaster of climatic variables and classification units
clim.all <- c(clim.var, clim.cu)
# Ouput table from select_functions()
df <- clim.difun$distfun
# Predicted distribution functions for climatic variables
clim.pdif <- predict_functions(cuvar.rast = clim.all, cu.ind = 3, 
                               cu = df$Class.Unit,
                               vars = df$Variable,
                               dif = df$Dist.Func
                              )
plot(clim.pdif, col = hcl.colors(100, "Oslo", rev = TRUE), nc = 4,
     mar = c(1.5, 1.5, 1.5, 3.5)
    )

## ----spatsig, fig.show = 'hold', fig.height = 4, fig.width = 4.69, message = FALSE, warning = FALSE, error = TRUE----
# Spatial signatures from predicted distribution functions
clim.sig <- signature(pdif.rast = clim.pdif,
                      inprex = paste(seq(1, 4), "_", sep = ""),
                      outname = paste("climate_", seq(1, 4), sep = "")
                    )
# Plot spatial signatures
plot(clim.sig, col = hcl.colors(100, "Oslo", rev = TRUE), nc = 2, 
     mar = c(1.5, 1.5, 1.5, 3.5))

## ----clean, message = FALSE, warning = FALSE, error = TRUE--------------------
unlink(c(paste(o, "/wasoil.zip", sep = ""), d), recursive = TRUE)

