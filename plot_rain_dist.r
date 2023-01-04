library(logitnorm)

max_rain = 4000
nsamples = 1000

ndays = 1:364
x = ndays/365

rainfalldist <- function(mn, sd) {
    x = ndays/365
    rain = runif(1, 0, max_rain)
    mn = mn * runif(1, 0, 1)# (mn + 0.5 * mn * runif(1, 0, 1))/2
    
    dist = dlogitnorm(x, mn, sd * runif(1, 0.5, 1) * rain/max_rain)
    if (sum(dist) == 0) dist[1] = rain
    dist = rain * dist/sum(dist, na.rm = TRUE)

    sorted = sort(dist, TRUE)
    cumm = 0
    for (i in ndays) {
        cumm = cumm + sorted[i]
        if (cumm > (0.8 * rain)) break()
    }
    
    ndry = sum(dist > 3)
    return(c(rain, i, 365 - ndry))
}

rainfallsdist <- function(...) 
    sapply(1:nsamples, function(i) rainfalldist(...) )

mod = list(rainfallsdist(0.5, 2.0),
           rainfallsdist(0.5, 0.8))


par(mfcol = c(2, 2))



plotMod <- function(mod) {
    plot(mod[1,], mod[2,], xlab = 'MAP/mm', ylab = 'Days for 80% annual P',
         xlim = c(0, max_rain), ylim = c(0, 365))
    plot(mod[1,], mod[3,], xlab = 'MAP/mm', ylab = 'Number of dry days',
         xlim = c(0, max_rain), ylim = c(0, 365))
}

lapply(mod, plotMod)
