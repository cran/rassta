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

## ----in, message = FALSE, warning = FALSE, error = TRUE-----------------------
# Load rassta and terra packages
library(rassta)
library(terra)
# Single-layer SpatRaster of stratification units
su <- rast(paste(d, "/su.tif", sep = ""))
# SpatVector with the boundaries of the area of interest
aoi <- vect(paste(d, "/aoi.shp", sep = "")) # Single area, no tiles
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
# SpatVector with SOC observations for stratification units
socobs <- vect(paste(d, "/soc.shp", sep = ""))
# Representative SOC observation for each stratification unit
su.obs <- observation(su.rast = su, obs = socobs, col.id = 1, col.resp = 2,
                      method = "mls", ls.rast = su.ls$landsim
                      )
# Data frame with numeric codes of stratification units and representative SOC values
su.soc <- su.obs$su_repobs[, c("SU", "soc")]

## ----pred, message = FALSE, warning = FALSE, error = TRUE---------------------
# Prediction of SOC across the landscape based on 3 winning stratification units
soc <- engine(ls.rast = su.ls$landsim,
              n.win = 3, # n highest landscape similarity values
              su.repobs = su.soc,
              tiles = aoi,
              outdir = d, # engine() writes results directly on disk
              overwrite = TRUE
            )

## ----ev, fig.height = 4, fig.width = 7, message = FALSE, warning = FALSE, error = TRUE----
# Set graphics arrangement
par(mfrow = c(1,2))
# Plot modeled soil organic carbon
plot(soc, col = hcl.colors(100, "Fall", rev = TRUE),
     main = "Modeled Soil Organic Carbon (%)", mar = c(3, 1.7, 3, 2)
    )
# Evaluate modeling performance
evalobs <- vect(paste(d, "/soc_valid.shp", sep = "")) # independent SOC measurements
evalmodel <- extract(soc, evalobs) # modeled SOC to independent sample locations 
names(evalmodel)[2] <- "soc_model"
evalobs <- cbind(evalobs, evalmodel[2]) # Table with measured and modeled SOC
eval.rmse <- sqrt(mean((evalobs$soc-evalobs$soc_model)^2)) # RMSE 
eval.mae <- mean(abs(evalobs$soc-evalobs$soc_model)) # MAE
# Set new graphics arrangement
par(mgp=c(1.2, 0.2, 0), mar = c(2.5, 1.7, 3.4, 1))
# Plot measured versus modeled SOC
plot(evalobs$soc, evalobs$soc_model, cex.axis = 0.83,
     xlab = "Measured Soil Organic Carbon (%)", ylab = "", yaxt = "n"
    )
axis(2, at = c(3, 3.5, 4, 4.5, 5, 5.5), labels = NA)
abline(0, 1, lty = "dashed")
text(x = 4, y = 5.8, cex = 0.8,
     paste("Root mean squared error: ", round(eval.rmse,2), "%", sep = "")
    )
text(x = 3.85, y = 5.6, cex = 0.8,
     paste("Mean Absolute Error: ", round(eval.mae,2), "%", sep = "")
    )

## ----clean, message = FALSE, warning = FALSE, error = TRUE--------------------
unlink(c(paste(o, "/wasoil.zip", sep = ""), d), recursive = TRUE)

