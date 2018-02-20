##########
## cfg ##
#########
library(raster)
library(rasterExtras)
source("libs/logit.r")
source("libs/converting_between_pacific_centric_and_regular_grid.r")

cropFile = '../LimFIRE/outputs/cropland2000-2014.nc'
modfFile = '../fireMIPbenchmarking/data/ModelOutputs/Inferno/Inferno_S1_burntArea.nc'
modvFile = '../fireMIPbenchmarking/data/ModelOutputs/Inferno/Inferno_S1_LandCoverFrac.nc'
obsfFile = '../LimFIRE/outputs/fire2000-2014.nc'

x0 = -15.41
k  = -0.08

modLayers = (3767 - 13*12 + 1):3767
modLayers = (3767 - 11):3767

scaling   = c(0.6, 1.4, 1.2)

###########################
## open Data             ##
###########################
crop = brick(cropFile)	

mod = mean(openInferno.fire(modfFile, modvFile))
mod = convert_pacific_centric_2_regular(mod)

obs = mean(brick(obsfFile))
obs = raster::resample(obs, mod)

fpc = brick('data/qrparm.veg.frac.nc')
fpc = convert_pacific_centric_2_regular(fpc)

## calculate suppression
suppression = layer.apply(crop, logit1At0, x0, k)