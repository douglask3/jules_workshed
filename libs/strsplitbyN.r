strsplitbyN <- function(X, i, ...) 
	sapply(X, function(x) strsplit(x, ...)[[1]][i])