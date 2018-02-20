openInferno.fire <- function(fireFile, vegFile) {
	byLayer <- function(l, mnth) {
		fire = brick(fireFile, level = l)[[mnth]]
		veg  = brick( vegFile, level = l)[[mnth]]
		return(fire * veg)
	}
	byLevel <- function(...) {
		fire = layer.apply(1:9, byLayer, ...)
		fire = sum(fire) * 60 * 60 * 24 * 30
		return(fire)
	}
	fire = layer.apply(modLayers, byLevel)
	return(fire)
}

openInferno.veg <- function(vegFile, mnths) {
	byLayer <- function(l) 
		veg  = mean(brick( vegFile, level = l)[[mnths]])
	
	return(layer.apply(1:9, byLevel))
}