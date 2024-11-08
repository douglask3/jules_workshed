##########
## cfg ##
#########
library(raster)
library(rasterExtras)
library(rasterPlot)
library(mapdata)
library(mapplots)
source("libs/logit.r")
source("libs/converting_between_pacific_centric_and_regular_grid.r")
source("libs/openInferno.r")

cropFile = '../LimFIRE/outputs/cropland2000-2014.nc'
modfFile = '../fireMIPbenchmarking/data/ModelOutputs/Inferno/Inferno_S1_burntArea.nc'
modvFile = '../fireMIPbenchmarking/data/ModelOutputs/Inferno/Inferno_S1_LandCoverFrac.nc'
obsfFile = '../LimFIRE/outputs/fire2000-2014.nc'
obsvFile = 'data/qrparm_frac_JULES-ESM-GA6.dump.spin3.1997_ordered.nc'

x0 = -0.0
k  = -0.08

modLayers  = (3767 - 13*12 + 1):3767
modLayers  = (3767 - 11):3767

Tree = 0.6; grass = 1.4; shrub = 1.2
veg_in = c(Tree, grass, shrub)
scaling_in = 
scaling_in0 = c(Tbe = Tree, tbe = Tree, bd = Tree, ne = Tree, nd = Tree, c3 = grass, c4 = grass, es = shrub, ds = shrub)
scaling_odr = c(Tbe = 1   , tbe = 1   , bd = 2   , ne = 2   , nd = 3   , c3 = 6    , c4 = 7    , es = 4    , ds = 5    )

lims0 = c(0.01, 0.02, 0.05, 0.10, 0.2, 0.4, 0.6, 0.8)
cols0 = c("#EEEEEE", '#FF9900', '#990000', '#110000')

dlims = c(-0.2, -0.1, -0.05, -0.02, -0.01, 0.01, 0.02, 0.05, 0.1, 0.2)
dcols = c('#000011', '#000099', '#0099FF', '#EEEEEE', '#FF9900', '#990000', '#110000')

pft_order  = c(1:6, 9, 12:13)

###########################
## open Data             ##
###########################

mod  = openInferno.fire(modfFile, modvFile, scaling_in)
mod0 = sum(openInferno.fire(modfFile, modvFile))
mod  = convert_pacific_centric_2_regular(mod )
mod0 = convert_pacific_centric_2_regular(mod0)

obs  = obs0 = mean(brick(obsfFile))
obs  = raster::resample(obs, mod)

#fpc = layer.apply(pft_order, function(i) brick(obsvFile, level = i))
fpc  = layer.apply(1:9, function(i) sum(brick(modvFile, level = i)[[modLayers]]))
fpc  = convert_pacific_centric_2_regular(fpc)
fpc  = raster::resample(fpc, mod[[1]])

crop = brick(cropFile)[[1:12]]
crop = raster::resample(crop, mod)	

## calculate suppression
suppression = mean(layer.apply(crop, logit1At0, x0, k)) ## Want to apply to each month maybe?
mod = mod * suppression

mask = !is.na(mod[[1]])

vmod = mod[mask]
min_fire = min(obs[obs > 0])
obs[obs == 0] =  min_fire
vobs = (obs[mask])
vfpc = fpc[mask]

makeFire <- function(params, fire, fpc, crop) {
	fire = sweep(fire, 2, params, '*')
	fire = fire * fpc
	fire = apply(fire, 1, sum)
	#fire[fire < min_fire] = min_fire
	#fire = log(fire)
	
	return(fire)
 }
 
for (i in rev(sort(unique(scaling_odr))[-1])) 
	scaling_in[scaling_odr == i] = scaling_in[scaling_odr == i] - max(scaling_in[scaling_odr == (i-1)])


parameterOrder <- function(FUN, params, order, ...) {
	
	for (i in (sort(unique(order))[-1]))
		params[order == i] = params[order == i] + max(params[order == (i-1)])

	return(FUN(params, ...))
}
	
mod = sum(mod)

mod1 <- function() {
	fit = nls(vobs ~ parameterOrder(makeFire, c(Tbe, tbe, bd, ne, nd, c3, c4, es, ds), scaling_odr, vmod, vfpc),
			  start  = as.list(scaling_in), trace = TRUE, algorithm = "port", 
			  lower = rep(0, length(scaling_in)), upper = rep(10, length(scaling_in)))
			  
	mod[mask] = predict(fit)
	return(mod)
}
		  

mod2 <- function() {
	fit = nls(vobs ~ makeFire(c(Tree, Tree, Tree, Tree, Tree, c3, c4, shrub, shrub), vmod, vfpc),
			  start  = list(Tree = Tree, c3 = grass, c4 = grass, shrub = shrub), trace = TRUE, algorithm = "port", 
			  lower = rep(0.0, 4), upper = rep(10, 4))
		
	params = summary(fit)$coefficients
	pval = params[,4]
	params = params[,1]
	params[pval > 0.05] = veg_in[pval > 0.05]
	Tree = params[1]; grass = params[2]; shrub = params[3]
	grass = grass
	mod[mask] = makeFire(c(Tree, Tree, Tree, Tree, Tree, grass, grass, shrub, shrub), vmod, vfpc)
	return(mod)
}	

mod3 <- function() {
	fit = nls(vobs ~ makeFire(c(Tree, Tree, Tree, Tree, Tree, c3 + shrub, c4 + c3, Tree + shrub, Tree + shrub), vmod, vfpc),
			  start  = list(Tree = Tree, c3 = grass - shrub, c4 = 0, shrub = shrub - Tree), trace = TRUE, algorithm = "port", 
			  lower = rep(0.0, 4), upper = rep(10, 4))
	
	mod[mask] = predict(fit)
	return(mod)
}

mod = list(mod1(), mod2(), mod3())

plotFun <- function(x, cols = cols0, limits = lims0, title = "") {
	x[!mask] = NaN
	plot_raster_from_raster(x * 12, limits = limits, cols = cols, add_legend = FALSE, y_range = c(-60, 90), quick = TRUE)
	mtext(title, side = 1, adj = 0.6, line = -1.25, cex = 0.8)
	grid('#000000FF')
}

plotDiff <- function(x, ...) {
	plotFun(x, ...)
	x = x - obs
	plotFun(x, cols = dcols, limits = dlims, title = 'Diff')
}

nmps = length(mod) + 4
mat = 1:(2*(nmps))
mat = t(matrix(mat, nrow = 2))
layout(mat, heights = c(rep(1, nmps), 0.2))

par(mar = rep(0, 4))
plotFun(obs, title = 'GFED4s')
plot.new()
mapply(plotDiff, c(mod0/12, mod0 * suppression / 12, mod), title = c('INFERNO', '+ fragmentation', '- tile opt. 1', '-tile opt. 2', '-tile opt. 3'))

add_raster_legend2(cols0, lims0, dat = obs, add = FALSE, transpose = FALSE, plot_loc = c(0.2, 0.8, 0.8, 0.93))
add_raster_legend2(dcols, dlims, dat = obs, add = FALSE, transpose = FALSE, plot_loc = c(0.2, 0.8, 0.8, 0.93))