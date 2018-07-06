darken <- function(color, factor=1.4){
    col <- col2rgb(color)
    col <- col/factor
    col <- rgb(t(col), maxColorValue=255)
    col
}

invert.color <- function(color, factor=1.4){	
	col <- col2rgb(color)
	col = sqrt((255^2) - (col^2))
	col <- rgb(t(col), maxColorValue=255)
	return(col)
}

hue_shift <- function(color, shift = -1/6) {
	col = col2rgb(color)
	col = rgb2hsv(col)
	col[1,] = col[1,] + shift
	
	while (min(col[1,]) < 0 || max(col[1,]) > 1) {
		col[1,col[1,] > 1] = col[1,col[1,] > 1] - 1
		col[1,col[1,] < 0] = col[1,col[1,] < 0] + 1
	}
	#browser()
	col <- hsv(t(col))
	return(col)
}

lighten <- function(color, factor=1.4){
	col <- col2rgb(color)
    col <- col2rgb(color)
    col <- col*factor
    col <- rgb(t(as.matrix(apply(col, 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
    col
}