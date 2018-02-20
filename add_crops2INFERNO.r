##########
## cfg ##
#########
library(raster)
library(rasterExtras)
source("libs/logit.r")
source("libs/converting_between_pacific_centric_and_regular_grid.r")
source("libs/openInferno.r")

cropFile = '../LimFIRE/outputs/cropland2000-2014.nc'
modfFile = '../fireMIPbenchmarking/data/ModelOutputs/Inferno/Inferno_S1_burntArea.nc'
modvFile = '../fireMIPbenchmarking/data/ModelOutputs/Inferno/Inferno_S1_LandCoverFrac.nc'
obsfFile = '../LimFIRE/outputs/fire2000-2014.nc'

x0 = -15.41
k  = -0.08

modLayers  = (3767 - 13*12 + 1):3767
modLayers  = (3767 - 11):3767

scaling_in = c(0.6, 0.6, 0.6, 0.6, 0.6, 1.4, 1.4, 1.2, 1.2)

###########################
## open Data             ##
###########################

mod = mod0 = openInferno.fire(modfFile, modvFile, scaling_in)
mod = convert_pacific_centric_2_regular(mod)

obs = mean(brick(obsfFile))
obs = raster::resample(obs, mod)

fpc = brick('data/qrparm.veg.frac.nc')[[1:9]]
fpc = convert_pacific_centric_2_regular(fpc)

crop = brick(cropFile)[[1:12]]
crop = raster::resample(crop, mod)	

## calculate suppression
suppression = mean(layer.apply(crop, logit1At0, x0, k)) ## Want to apply to each month maybe?
mod = mod * suppression

mask = !is.na(sum(fpc))

vmod = mod[mask]
vobs = obs[mask]
vfpc = fpc[mask]

makeFire <- function(fire, fpc, ...) {
	fire = sweep(fire, 2, c(...), '*')
	fire = fire * fpc
	return(fire)
 }
 
nls(obs ~ makeFire(fire, fpc, ...
	