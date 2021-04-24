source("libs/mtext.units.r")
source("libs/lighten_darken.r")
graphics.off()


cols = c('#FF2222', '#22FF22', '#2222FF')
pch = c(19, 3, 4)
cex = c(1, 2, 2)


addDat <- function(dat, col, pch, cex, lwd,..., fitFun = addFit1) {
	col = lighten(col, 3.0)
	col = rep(col, nrow(dat))
	coli = rep(col, max(dat[, 'type']))
	
	cex = rep(cex, nrow(dat)) * 4/3
	lwd = rep(cex, nrow(dat)) * 3/4
	for (i in 2:max(dat[, 'type'])) {
		test = dat[, 'type'] >= i
		col[test] = darken(col[test], 2.0)
		coli[i:3] = darken(coli[i:3], 2.0)
		
		cex[test] = cex[test] * 3/4
		lwd[test] = lwd[test] * 4/3
	}
	
	points(dat[, 3], dat[, 4], col = col, pch = pch, cex = cex, lwd = lwd, ...)
		
	fitFun(dat, coli)
	return(coli)
}

addFit2 <- function(dat, coli) {
	fit = lm(dat[, 4] ~ log(dat[, 3]))
	
	x = seq(0, 20, 0.01)
	y =  log(x ) * coefficients(fit)[2] +  coefficients(fit)[1]
	
	lines(x, y, col = coli[2], lwd = 2)
}

addFit1 <- function(dat, coli) {
	
	fit = lm(dat[, 4] ~ dat[, 3])
	line =  coefficients(fit)[1] + c(0, 1200) * coefficients(fit)[2]
	
	lines(c(0, 1200), line, col = coli[2], lwd = 2)
}
############
## Plot 1 ##
############

dat = read.csv('data/LOCATE.csv')
dats =  split(dat, dat[,1])

plot(dat[,'Obs'], dat[, 'Mod'], type = 'n', 
	 xlim = c(0, 1200), ylim = c(0, 1200), xaxs = 'i', yaxs = 'i',
	 xlab = '', ylab = '')
	 
	 mtext.units(side = 1, line = 2.5, 'Observed aNPP (gC ~m-2~ ~yr-1~)', cex = 1.2)
	 mtext.units(side = 2, line = 2.5, 'Modelled aNPP (gC ~m-2~ ~yr-1~)', cex = 1.2)
	 



col = mapply(addDat, dats, cols, pch = pch, cex = cex, lwd = 2, SIMPLIFY = FALSE)

lines(c(0,1200), c(0, 1200), lty = 2, lwd = 3)

txt.col = c("white", "white", "black")

subLegend <- function(i, title, col, x0 = 300, dx = 40,...) {
	col = unique(col)
	
	x = x0 + dx * 4 * i
	legend(x, 300, col = col, legend = 
		  c('Natural/\nSemi-natural', 'Broad leaf trees', 'Improved/\nSemi-improved'), 
		  bty = 'n', ...)
	text(x + dx, 300, title)
	lines(c(x, x) + c(0.5, 1.5) * dx, c(20, 270), col = col[2], lwd = 2)
}

cex = lapply(cex, '*', c(4/3, 1, 3/4))
lwd = c(1/2, 1, 2)

mapply(subLegend, 1:3, c('JULES', '+ ECOSSE', '+ fertilizer'),
	  col, pch = pch, pt.cex = cex, text.col = txt.col, lty = 0,
	  MoreArgs = list(lwd = lwd))

############
## Plot 2 ##
############
dev.new()

dat = read.csv('data/LOCATE2.csv')
dats =  split(dat, dat[,1])

plot(dat[,'x'], dat[, 'y'], type = 'n', 
	 xlim = c(0, 20), ylim = c(0, 1000), xaxs = 'i', yaxs = 'i',
	 xlab = '', ylab = '')
	 
	 mtext.units(side = 1, line =2.5, 'N~O2~ (gN ~m-2~ in top 1m soil)', cex = 1.2)
	 mtext.units(side = 2, line = 2.5, 'aNPP (gC ~m-2~ ~yr-1~)', cex = 1.2)

col = mapply(addDat, dats, cols[2:3], pch = pch[2:3], cex = cex[2:3], lwd = 2, SIMPLIFY = FALSE, MoreArgs = list(fitFun = addFit2))

mapply(subLegend, 1:2, c('Modelled', 'Observed'),
	  col, pch = pch[2:3], pt.cex = cex[2:3], text.col = txt.col[2:3], lwd = 2, lty = 0, x0 = 2, 
	 dx = 1)