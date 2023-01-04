library(raster)
graphics.off()
sourceAllLibs("../jules_inferno_benchmark/libs/plotting/")

dir = "data/lightn/"
extent = c(-90, -30, -60, 12)
files = c(Historical = "bb277", "SSP1-26" = "be397", "SSP3-70" = "be395",
          "SSP5-85" = "be396")

cols = c('#f7f4f9','#e7e1ef','#d4b9da','#c994c7','#df65b0','#e7298a','#ce1256','#980043','#67001f')
dcols = c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788')

limits = c(0, 0.1, 0.2, 0.5, 1, 1.5, 2)*10
dlimits1 = c(-1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1)*10
dlimits2 = c(1, 1.05, 1.1, 1.2, 1.5)
dlimits2 = c(rev(signif(1/dlimits2, 2)), dlimits2) * 100
dlimits2 = c(70, 90, 95, 99, 101, 105, 110, 150)


maski = openMod("u-bi607_Hist_new/", "../jules_outputs/", 'frac', 2001:2005,1, 
                levels = 1:5, cover = TRUE)/60
maski = convert_pacific_centric_2_regular(maski)
maski = raster::crop(maski, extent)
mask = is.na(maski)
mask2 = maski<0.5


openDat <- function(file) {
    print(file)
    dat = brick(paste0(dir, file, '.nc'))
    dat = dat[[(nlayers(dat)-9):nlayers(dat)]]
    dat = mean(dat)
    dat = convert_pacific_centric_2_regular(dat)
    dat = raster::crop(dat, extent)
    dat = raster::resample(dat, mask)
    dat[mask] = NaN
    dat*12
}

dats = lapply(files, openDat)

plotFun <- function(dat, col = cols, limit = limits, plotT = TRUE, mask2 = NULL) {
    if (plotT) {
        plot(extent[1:2], extent[3:4], type = 'n', axes = FALSE, xlab = '', ylab = '')
        plotStandardMap(dat, cols = col, limits = limit, add = T)
        if (!is.null(mask2))
            contour(mask2, levels = c(0.5), labels = '', drawlabels = FALSE,
                    add = TRUE, lwd = 2, col = '#0B4E37')
    }
    return(dat)
}

plotFunDiff1 <- function(dat, ...) 
    plotFun(dat-dats[[1]], dcols, dlimits1, ...)

sourceAllLibs('../jules_inferno_benchmark/libs/dataProcess/')
modFracVar = 'frac'
plotFunDiff2 <- function(dat, ...) 
    plotFun(100* dat/dats[[1]], dcols, dlimits2, ...)

legFUN <- function(...) 
    StandardLegend(..., add = FALSE, oneSideLabels = FALSE, ytopScaling = 2.2)

png("figs/SAlightning_maps.png", height = 7, width = 4.8*4/3, res = 200, units = 'in')
par(mar = rep(0, 4), oma = c(0, 0.67, 1, 0.67), rep(1, 4))
    layout(cbind(1:5, c(0, 6:9), c(0, 10:13), c(14:17, 0), c(18:21, 0)),
                 heights = c(1, 1, 1, 1, 0.3))
    lapply(dats, plotFun)
    legFUN(dat = dats[[1]], cols = cols, limits = limits)
    mtext.units('strikes k~m-2~ ~yr-1~', side = 1, line = -1.1, cex = 0.8, adj = 1.8, xpd = NA)
    ddat1 = lapply(dats[-1], plotFunDiff1)

    legFUN(dat = dats[[2]] - dats[[1]], cols = dcols, limits = dlimits1, 
           extend_min = TRUE)

    ddat2 = lapply(dats[-1], plotFunDiff2, mask2 = mask2)

    legFUN(dat = 100*dats[[2]]/dats[[1]], cols = dcols, limits = dlimits2,
           extend_min = TRUE)    
    mtext.units('%', side = 1, line = -1.1, cex = 0.8)

    mtext(side = 3, outer = TRUE, adj = seq(1/20, 1, by = 1/4.3),
          c("Lightning", "Difference\n(strikes)", "Difference\n(%)", "Diff.", "Diff. TC > 50%"), 
          padj = c(0, 0.67, 0.67, 0, 0), line = -0.2)
    mtext(side = 2, outer = TRUE, adj =1-seq(1/16, 1, by = 1/3.5)*4/4.3, names(files), line = -1)

    histFun <- function(x, maskin, logt = FALSE, side = 'topright', ...) {
        x0 = x
        x = x[!maskin]
        #return(x)
        txt = signif(quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE), 2)
        if (logt) x = log(x + 0.01)
        hist(x, xlab = '', ylab = '', yaxt = 'n', main = '', 100, xaxt = 'n',
             border = tail(cols, 1), col = tail(cols, 1))
        if (logt) {
            at  = range(x)
            at = seq(at[1], at[2], length.out = 6)       
            at = signif(exp(at) - 0.01, 1)
            at = unique(at)
            axis(1, at = log(at + 0.01), labels = at)
        } else axis(1)
        legend(side, paste(names(txt), txt, sep = ': '), bty = 'n', cex = 0.8, ...)
    }        
    par(mar = c(1.5, 0, 1, 0))

    histFun(dat[[1]], mask, TRUE, 'topleft')
    
    mtext.units(side = 1, 'strikes k~m-2~ ~yr-1~', adj = 2.2, xpd = NA, line = 1.9, cex = 0.8)
    lapply(ddat2, histFun, mask)
    mtext(side = 1, '% Difference', adj = 2, xpd = NA, line = 1.9, cex = 0.8)
    histFun(dat[[1]], mask2, TRUE, 'topleft', ncol = 2)
    #ddat3 = lapply(dats[-1], plotFunDiff2, plotT = FALSE)
    lapply(ddat2, histFun, mask2)
    
dev.off()


