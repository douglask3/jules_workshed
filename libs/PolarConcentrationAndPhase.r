PolarConcentrationAndPhase <- function(x, ...)
    UseMethod("PolarConcentrationAndPhase")
    
PolarConcentrationAndPhase.RasterStack <-
        function(dat, phase_units = "radians", n = min(12, nlayers(dat)),
                 disagFact = NaN, justPhase = FALSE) {
    if (nlayers(dat) < n) {
        warning(paste('number of layers in dat is less than n. n set to',
                      nlayers(dat)))
    }
    if (nlayers(dat) > n) {
        dat0 = dat
        dat  = dat0[[1:n]]

        for (i in 1:n) {
            index = seq(i, nlayers(dat0), by = n)
            dat[[i]] = mean(dat0[[index]])
        }
    }
    if (!is.na(disagFact))
     dat = layer.apply(dat, disaggregate, disagFact, method = "bilinear")

    out        = dat[[1:2]]
    names(out) = c('Phase', 'Concentration')

    vout = PolarConcentrationAndPhase(values(dat), phase_units)
    test = sum(dat)==0

    if (justPhase) index = 1 else index = 1:2
    for (i in index) {
        out[[i]] = vout[[i]]
        out[[i]][test] = NaN
    }
    if (justPhase) return(out[[1]])
    return(out)
}

atans <- function(x, y, units = 'months', revs = FALSE) {
    classX = class(x)

    if (classX   == "RasterLayer") phase_out = x[[1]]
    if (classX   == "RasterLayer") x = as.matrix(x)
    if (class(y) == "RasterLayer") y = as.matrix(y)

    phase = atan2(x, y)

    if (units == 'months') {
        phase = 6 * (phase / pi)
        test = !is.na(phase) & phase < 0 
        phase[test] = phase[test] + 12
    } else if (units == 'degrees') {
        phase = phase
        phase = phase * 360 / (2 * pi)
    } else if (units == 'radians') phase = phase

    if (classX == "RasterLayer") {
    	values(phase_out) = phase
    	return(phase_out)
    } else {
    	return(phase)
    }
}

testModal <- function(r) {
    out0 = rep(NaN, 12)
    cellModel <- function(y) {
        if (any(is.na(y))) return(out0)
        if (sum(y) == 0) return(out0)
        if (all(y == y[1])) return(out0)
        out0[1] = 1 - min(y)/max(y)
        out0[7] = which.max(y)
        testy = y[c(which.min(y):12, 1:which.min(y))]
        dy = diff(testy)
        peaks = which(testy > c(testy[-1], testy[1]) & testy >= c(tail(testy, 1), head(testy,-1)))
        if (length(peaks) == 1) return(out0)
        
        peaks = peaks[sort.int(testy[peaks], index.return = TRUE)[[2]]]
        out0[8:(6+length(peaks))] = head(peaks - tail(peaks,1),-1)
        prominance <- function(p1, p2)
            (testy[p1] - min(testy[p1:p2]))/max(testy)
        
        out = mapply(prominance, head(peaks, -1), peaks[-1])
        out0[2:(length(out)+1)] = out
        if (length(out) > 5) browser()
        return(out0)
    }
    outr = r[[1:12]]
    outr[] = t(apply(r[], 1,cellModel))
    return(outr)
}
PolarConcentrationAndPhase.RasterBrick <- function(...)
    PolarConcentrationAndPhase.RasterStack(...)

PolarConcentrationAndPhase.default <-
    function(cdata, phase_units = "radians",  ncycle = NULL) {
    if (class(cdata) == "numeric") cdata =  t(matrix(cdata))
	if (is.null(ncycle)) ncycle = dim(cdata)[2]
    xdata = ydata = matrix(0, dim(cdata)[1], 1)
	
    for (k in 1:ncycle) {
        angle = 2 * pi * (ncycle - k + 1) / ncycle
        xdata = xdata + cdata[, k] * cos(angle)
        ydata = ydata + cdata[, k] * sin(angle)
    }
	
    adata = apply(cdata, 1, sum)

    phase = atans(-ydata, xdata, phase_units)
    conc  = sqrt (xdata^2 + ydata^2) / adata
    return(list(phase, conc))
}

PolarConcentrationAndPhase <- function(x, ...)
    UseMethod("PolarConcentrationAndPhase")

PolarConcentrationAndPhase.default <-
    function(cdata, phase_units = "radians",  ncycle = NULL) {
    if (class(cdata) == "numeric") cdata =  t(matrix(cdata))
	if (is.null(ncycle)) ncycle = dim(cdata)[2]
    xdata = ydata = matrix(0, dim(cdata)[1], 1)
	
    for (k in 1:ncycle) {
        angle = 2 * pi * (ncycle - k + 1) / ncycle
        xdata = xdata + cdata[, k] * cos(angle)
        ydata = ydata + cdata[, k] * sin(angle)
    }
	
    adata = apply(cdata, 1, sum)

    phase = atans(-ydata, xdata, phase_units)
    conc  = sqrt (xdata^2 + ydata^2) / adata
    return(list(phase, conc))
}