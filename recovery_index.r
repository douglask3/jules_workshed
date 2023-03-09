
#################################
## generate example data      ##
###############################
x = seq(-5, 10)
xlim = c(-5, 20)

y = 1 + runif(length(x), 0, 0.5)^2 - runif(length(x), 0, 0.5)^2

disturbance <- function(x) {
    out = rep(0, length(x))
    out[x>=0] = exp(-(1/runif(1, 3, 10)) * (x[x>=0]))
    out = -sqrt(runif(1, 0.5, y[x==0])) * out/max(out)
    return(out)
}

y = y + disturbance(x)


plot(x, y, type = 'l', ylim = c(0, max(y)), xlim = xlim, lwd = 2, xlab = 'time', ylab = 'Veg index')
lines(c(0, 0), c(0, 12), col = 'red', lwd = 2, lty = 2)
text(x = 0.5, y = 0, adj =  c(0, -0.75), srt = 90, col = 'red', 'Fire event')

#################################
## Fit recovery model         ##
###############################
preFire_state = mean(y[x<0])
lines(xlim, rep(preFire_state, 2), col = 'blue', lwd = 2, lty = 2)
text(x = xlim[2], y = preFire_state, col = 'blue', 'Pre-fire state', adj = c(1, -0.75))

model_recovery <- function(x, impact, rate) {
    y = preFire_state - impact * exp(-rate * x)
    return(y)
}

train_x = x[x>=0]
train_y = y[x>=0]

xnew = seq(0, 50, 0.1)

fit = nls(train_y ~ model_recovery(train_x, impact, rate), start = c(impact = 1, rate = 1))
lines(xnew, predict(fit, newdata = list(train_x = xnew)), col = 'red', lwd = 2)

#################################
## Fit recovery indicies      ##
###############################
time_to_recover_x_value <- function(recovery_threshold) {
    recovery_value = preFire_state + impact*(recovery_threshold-1)
    time2recover = (1/rate) * log((impact)/(preFire_state - recovery_value))

    lines(xlim, rep(recovery_value, 2), col = 'blue', lty = 2)
    text(x = xlim[2], y = recovery_value, col = 'blue', adj = c(1, 1.25),
         paste('Recovery point for threshold', recovery_threshold))

    lines(rep(time2recover, 2), c(0, max(y)), col = 'red', lty = 2)
    text(x = time2recover, y = 0, col = 'red', srt = 90, adj = c(0, -0.75),
         paste0('Time of recovery for threshold', recovery_threshold))
    
    return(round(c(recovery_value = recovery_value, time = time2recover), 3))
}

params = coefficients(fit)
impact = params[1]; rate = params[2]

recovery_thresholds = c(0.5, 0.9, 0.99)

time2rec = cbind(sapply(recovery_thresholds, time_to_recover_x_value), c(round(impact, 3), ''), 
                 round(c(impact*rate, rate), 3))

colnames(time2rec) = c(paste0("Recovery Threshold ", recovery_thresholds),
                       'Initial impact (total, frac of initial state)',
                       'Recovery rate in first year (total, frac of  impact)')

print(time2rec)
