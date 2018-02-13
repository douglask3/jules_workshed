addComments2nc <- function(nc, comments) {
	attPutStandard <- function(name, val)
			ncatt_put(nc, 0, name, val)
			
	mapply(attPutStandard, names(comment), comment)
}