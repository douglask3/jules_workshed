openInferno.fire <- function(fireFile, vegFile, deconstruct = NULL) {
	byLayer <- function(l, mnth) {
		
		fire = brick(fireFile, level = l)[[mnth]]
		veg  = brick( vegFile, level = l)[[mnth]]
		if (is.null(deconstruct)) fire = fire * veg else fire = fire / deconstruct[l]
		return(fire)
	}
	byLevel <- function(...) {
		fire = layer.apply(1:9, byLayer, ...)
		if (is.null(deconstruct)) fire = sum(fire) 
		fire = fire * 60 * 60 * 24 * 30
		return(fire)
	}
	fire = layer.apply(modLayers, byLevel)
	if (!is.null(deconstruct)) {
		meanLayer <- function(l) {
			index = seq(l, nlayers(fire), 9)
			mean(fire[[index]])
		}
		fire = layer.apply(1:9, meanLayer)
	}
	return(fire)
}

openInferno.veg <- function(vegFile, mnths) {
	byLayer <- function(l) 
		veg  = mean(brick( vegFile, level = l)[[mnths]])
	
	return(layer.apply(1:9, byLevel))
}