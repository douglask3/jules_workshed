logit <- function(x, x0, k) 
	1/(1 + exp(-k * (x - x0)))
	
	
logit1At0 <- function(x, x0, k)
	logit(x, x0, k) / logit(0, x0, k)