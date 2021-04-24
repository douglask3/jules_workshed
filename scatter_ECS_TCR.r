source("../rasterextrafuns/rasterPlotFunctions/R/mtext.units.r")

graphics.off()
ecs_tcr_file = 'data/ECS_TCR.csv'
gwt_file = 'data/GWT_window_21.csv'

ecs_tcr = read.csv(ecs_tcr_file, stringsAsFactors = FALSE)
gwt     = read.csv(gwt_file    , stringsAsFactors = FALSE)

ecs_tcr = ecs_tcr[!apply(ecs_tcr[,2:3], 1, function(i) any(is.na(as.numeric(i)))),]

tab = matrix("NULL", ncol=25, nrow = 1)
processLine <- function(line) {
    #if (is.na(line[5])) return()
    tab0 = tab
    test = line[2] == ecs_tcr[,1]
    if (!any(test)) return()
    tline = ecs_tcr[test,]
    tline = unlist(c(line[c(1, 4)], tline))
    test = sapply(1:5, function(i) which(tab[,i] == tline[i]))
    
    if (any(sapply(test, length)==0) ||diff(range(sapply(test, max)))!=0) {        
        tab = rbind(tab, c(tline, line[5], rep("NULL", 19)))
    } else {
        nr = nrow(tab)
        index = which(tab[nr,] == "NULL")[1]
        
        tab[nr, index] = line[5]
    }
    #if (dim(tab)[1] == 27) browser()
    tab <<- tab
}


apply(gwt, 1, processLine)
tab = tab[-1,]
tab = tab[,!apply(tab,2, function(i) all(i == "NULL"))]

cx = seq(0, 2*pi, 0.01)

limits = 2010:2100
cols = list(rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')),
            rev(c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')),
            c('#050000', '#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695', '#000005'))
cols = lapply(cols, make_col_vector, ncols = length(limits))
addCombo <- function(line, radius, cols, GWT, SSP) {
    if (line[1] != SSP || as.numeric(line[2]) != GWT) return()
    x = as.numeric(line[4])
    y = as.numeric(line[5])
    zs = line[-c(1:5)]
    zs = zs[zs != "NULL"]
    NZ = length(zs)
    addArc <- function(z, zn) {
        cx = seq(2*pi * (zn-1)/NZ, 2*pi * zn/NZ, 0.01)
        cy = sin(cx); cx = cos(cx)
        cy = y + c(0, cy, 0) * diff(par("usr")[3:4])*radius
        cx = x + c(0, cx, 0) * diff(par("usr")[1:2])*radius
        
        if (is.na(z)){
            col = "white"
            density = 10
            border = "white"
        } else {
            col = cols[z == limits]
            density = NULL
            border = "black"
        }
        polygon(cx, cy, col = col, border = border, density = density)
        #print(z)
    }
    
    mapply(addArc, zs, 1:NZ)
    modi = mods == line[3]
    if (!mods_add_test[modi]) {
        text(line[3], x = x + diff(par("usr")[1:2])*radius * 0.9 *xmod_lab[modi], 
                      y = y + diff(par("usr")[3:4])*radius * 0.9 *ymod_lab[modi],
                      adj = amod_lab[modi], col = "white")
        mods_add_test[mods == line[3]] = TRUE
        mods_add_test <<- mods_add_test
    }
}


plotSSP <- function(ssp, r, tab, gwt) 
    apply(tab, 1, addCombo, r, cols[[3]], gwt, ssp)

ssps = sort(unique(tab[,1]))
mods = unique(tab[,3])
heights = c(0.7, 0.2, 1, 0.12, 0.3)
png("figs/GWT_scatter.png", height = 7.2 * 0.49 * sum(heights),
    width = 7.2, units = 'in', res =300)
layout(rbind(c(4, 5), 6, c(1, 2), 0, 3),
       heights = heights)
par(mar = rep(0.5, 4), oma = rep(3.5, 4))

pieScatter <- function(tabi, gwt, xlim, ylim, xlab, ylab) {
    plot(range(tab[,4]), range(tab[,5]), xlab = '', ylab = '', cex = 10000,
         col = "black", pch = 19, xlim = xlim, ylim = ylim)
    mtext.units(side = 3, adj = 0.9, line = -2,
           paste0("GWT: ", gwt, "~DEG~C"), col = "white")
    axis(3)
    mtext.units(side = 1, xlab, line = 2.6)
    mtext.units(side = 2, ylab, line = 2)

    mods_add_test <<- rep(FALSE, length(mods))
    rs = rev(sqrt(seq(0.00, 0.085^2, length.out = length(ssps)+1))[-1])
    mapply(plotSSP, ssps, rs, MoreArgs = list(tabi, gwt))
}
tabi = tab
tabi[,4:5] = tabi[,5:4]
tabi[,5] = as.numeric(tabi[,4])/as.numeric(tabi[,5])

ymod_lab = c(1, -1,-1,-1, 1, 1)
xmod_lab = c(1, -1,-1,-1, 1, -1)
amod_lab = c(0, 1, 1, 1, 0, 1)

pieScatter(tabi, 2, c(1.5, 2.9), c(0.39, 0.68),
           'TCR (~DEG~C)', 'TCR/ECS (~DEG~C/~DEG~C)')

tabi = tab
tabi[,5] = as.numeric(tabi[,5])/as.numeric(tabi[,4])
ymod_lab = c(1, -1.45,-1,-1, 1, 1.45)
xmod_lab = c(1, 0,-1,-1, 1, 0)
amod_lab = c(0, 0.33, 1, 1, 0, 0.67)
pieScatter(tabi, 4, c(2.3, 6), c(0.39, 0.68),
           'ECS (~DEG~C)', '')
axis(4)
plot(c(2000, 2150), c(0, 1), axes = FALSE, pch = 19, cex = 1000)
points(2010:2100, rep(0.5, length(cols[[3]])), pch = 19, col = cols[[3]])
legLabs = c(2010, 2030, 2050, 2070, 2090)
text(x = legLabs, y = 0.2, legLabs, col = "white")

cx = seq(0, 2*pi, 0.01)
polygon(2115 + sin(cx)*2.5*1.9 , 0.7+cos(cx)*2.5/10, col = "white", density = 10)
text(x = 2115, y = 0.2, "out of\nrange", col = "white") 

legSSP <- function(ssp, r, xp, yp, srt) {
    polygon(2133 + sin(cx)*r * 1.9 , 0.5+cos(cx)*r / 10, border = "white")
    text(x = 2133 + (r-0.5) * 1.9 * xp, y = 0.5 + yp* (r-0.5) / 10, ssp, srt = srt, col = "white", adj = c(0.5, 0.5))

}
mapply(legSSP, ssps, rev(1:length(ssps)),
       c(0, 1, 0, -1, 0, 1), c(-1, 0, 1, 0, -1, 0), c(0, 90, 0, 90, 0, 90))



addPoints <- function(line, id, GWT, adjx = 0,cols, ...) {
    if (as.numeric(line[2]) != GWT) return()

    y = line[-c(1:5)]
    y = as.numeric(y[y!="NULL"])
    y[is.na(y)] = 2200
    #if (length(y) == 1 && y == 2200) y = c(2100, 2200)
    
    if (all(y == 2200)) y = c(2100, 2200)
    y = sort(y)
    x = as.numeric(line[id]) + adjx
    z = which(line[1] == ssps)
    col = cols[z]

    addLine <- function(i, lwd) {
        ys = y[c(i, length(y)-i+1)]
        lty = 1
        if (all(ys >= 2100)) {
            lty = 3
            lwd = 1
        }
        #if (any(ys<2100) && any(y>2100))
        if (diff(ys) == 0) points(x, y[1], col = col, pch = 19, cex = 1.5)
        lines(c(x, x), ys, col = col, lwd = lwd, lty = lty, ...)
    }
    
    if (diff(range(y)) ==0)
        points(x, y[1], col = col, pch = 19, cex = 1.5)
    else
        mapply(addLine,rev(1:floor(length(y)/2)), seq(9, 0, -3)[1:floor(length(y)/2)])
}

ssps_pch = c(4, 10, 8, 17, 19)
ssps_cols = rev(c('#d73027','#f46d43','#fdae61', '#abd9e9','#74add1','#4575b4'))
ssps_cols = list(c('#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081'),
                 c('#f768a1','#dd3497','#ae017e','#7a0177','#49006a'))

plotRegress <- function(id = 4, ylab = 'Year of exceedance', xlim = c(1.5, 2.9)) {

    plot(range(tab[,id]),c (2010, 2110), xlab = '', ylab = '',
         pch = 19, xlim = xlim, cex = 1000)#,xaxt = 'n'
        axis(3)
    mtext.units(side = 2, ylab, line = 2)

    apply(tab, 1, addPoints, id, 2, -0.01, ssps_cols[[1]])

    apply(tab, 1, addPoints, id, 4, 0.01, ssps_cols[[2]])
}
plotRegress(5)
plotRegress(4, '', c(2.3, 6))
axis(4)
plot(c(0, 1), c(0,1),cex = 1000, pch = 19, axes = FALSE, xlab = '', ylab = '')

x = seq(0.1, 0.5, length.out = length(ssps))
addTT <- function(name, y, cols) {
    points(x, rep(y, length(x)), pch = 19, col = cols)
    mapply(function(i, col) lines(c(i, i), c(y-0.8/6, y+0.8/6),
                    lwd = 4, col = col), x, cols)
    text.units(x = 0.05, y = y, name, col = "white", srt = 90)

    lines(c(0.65, 0.65), c(y-0.8/6, y+0.8/6), col = cols[1], lty = 2)
}
addTT("2~DEG~C", 0.25, ssps_cols[[1]])
addTT("4~DEG~C", 0.75, ssps_cols[[2]])
text(x+0.02, rep(0.5, length(x)), ssps, col = "white", srt = 90)
text(0.67, 0.5, "out of\nrange", col = "white", srt = 90)

dev.off()


