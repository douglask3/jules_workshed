source("libs/mtext.units.r")

dat = read.csv('data/LOCATE.csv')

cols = c('grey', 'green', 'blue')
pch = c(19, 3, 4)
cex = c(1, 2, 2)

plot(dat[,'Obs'], dat[, 'Mod'], type = 'n', 
	 xlim = c(0, 1200), ylim = c(0, 1200), xaxs = 'i', yaxs = 'i',
	 xlab = '', ylab = '')
	 
	 mtext.units(side = 1, line = 2.5, 'Observed aNPP (gC ~m-2~ ~yr-1~)', cex = 1.2)
	 mtext.units(side = 2, line = 2.5, 'Modelled aNPP (gC ~m-2~ ~yr-1~)', cex = 1.2)
	 
dats =  split(dat, dat[,1])

darken <- function(color, factor=1.4){
    col <- col2rgb(color)
    col <- col/factor
    col <- rgb(t(col), maxColorValue=255)
    col
}


addDat <- function(dat, col, ...) {
	col = rep(col, nrow(dat))
	coli = rep(col, max(dat[, 'type']))
	for (i in 2:max(dat[, 'type'])) {
		test = dat[, 'type'] >= i
		col[test] = darken(col[test], 1.1)
		coli[i:7] = darken(coli[i:7])
	}
	
	points(dat[, 'Obs'], dat[, 'Mod'], col = col, ...)
	
	fit = lm(dat[, 'Mod'] ~ dat[, 'Obs'])
	line =  coefficients(fit)[1] + c(0, 1200) * coefficients(fit)[2]
	lines(c(0, 1200), line, col = col, lwd = 2)
	return(coli)
}

col = mapply(addDat, dats, cols, pch = pch, cex = cex, lwd = 2, SIMPLIFY = FALSE)

lines(c(0,1200), c(0, 1200), lty = 2, lwd = 3)

#legend('bottomright', legend = c('JULES', '   + ECOSSE N limitation', '       + fertilizer'), pch = c(19, 3, 4), pt.cex = c(1, 2, 2), col = c('grey', 'green', 'blue'), lty = 1, lwd = 2)

txt.col = c("white", "white", "black")

subLegend <- function(i, title, col, ...) {
	col = unique(col)
	
	x = 300 + 170 * i
	legend(x, 400, col = col, legend = 
		  c('Peat', 'Rough grass', 'Conifer', 'BL tree', 'Improved grass', 'Arbale', 'Heath'), 
		  bty = 'n', ...)
	text(x + 50, 400, title)
	lines(c(x, x) + c(20, 60), c(20, 380), col = col[1], lwd = 2)
}

mapply(subLegend, 1:3, c('JULES', '+ ECOSSE', '+ fertilizer'),
	  col, pch = pch, pt.cex = cex *0.8, text.col = txt.col, lwd = 2, lty = 0)