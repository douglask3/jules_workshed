dat = read.csv('data/albedomodel_trace2.csv')
dat = dat[,24:32]

plotParamDist <- function(params, col, name) {
	hist(params[params < 0.5], xlim = c(0, 0.5), 1000, freq = FALSE,
		 xlab = '', ylab = '', main = '', axes = FALSE, col = col, border = col)
	axis(1)
	axis(2, at = par("usr")[3:4], labels = c('', ''))
	lines(rep(par("usr")[1], 2),  par("usr")[3:4])
	lines(rep(par("usr")[2], 2),  par("usr")[3:4])
	lines(par("usr")[1:2],  rep(par("usr")[3], 2))
	lines(par("usr")[1:2], rep( par("usr")[4], 2))
	mtext(name, line = -1.5)

}

png('docs/albedo_parameter_dist.png', width = 7, height = 2, units = 'in', res = 600)
layout(rbind(1:7, c(rep(0, 5), 8:9)))
par(mar = c(2, 1, 1, 0), oma = c(0, 1, 0, 1))
mapply(plotParamDist, dat, c('#d78288', '#52bed0', '#a1d782'),
       c('BDT', 'BET-Tr', 'BET-te', 'NDT', 'NET', 'ESh', 'DSh', 'C3G', 'C4G'))
dev.off()