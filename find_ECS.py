import os
from   io     import StringIO
import numpy  as np
import pandas as pd
import csv
import math
from scipy import stats 
import matplotlib.pyplot as plt

filename = "data/TOA_vs_TAS.csv"
degree_sign = '\N{DEGREE SIGN}C'

dat = pd.read_csv(filename)

def notNaNvals(var):
    x = dat[var].values
    return(x[~np.isnan(x)])

def perform_regression(xvar, yvar, symbol, color):
    x = notNaNvals(xvar)
    y = notNaNvals(yvar)
    
    plt.plot(x, y, symbol, color = color)    
    slope, intercept, r, p, std_err = stats.linregress(x, y)
    ecs = -intercept/slope

    xp = np.arange(0, ecs + 0.01, 0.01)
    yp = intercept + slope * xp
    plt.plot(xp, yp, linestyle = "dotted", color = color)
    return(slope, intercept, ecs, std_err)

fire = perform_regression("TOA_with", "Temp_with", 'x', color = 'red')
nofire = perform_regression("TOA_without", "Temp_without", 'x', color = 'blue')

z = np.abs(fire[2]-nofire[2])/np.sqrt(fire[3]**2 + nofire[3]**2)
p_value = 2*stats.norm.sf(z)


def addLab(y, lab1, res, col):
    t = plt.text(2.5, y, lab1 + str(np.around(res[2], 3)) + "\u00B1" +
                       str(np.around(res[3], 3)) +  degree_sign,  color=col)
    t.set_bbox(dict(facecolor='white', alpha=1.0, edgecolor='white'))
  
addLab(7, "ECS without fire:", nofire, 'blue')
addLab(6.5, "ECS with fire:", fire, 'red')
t = plt.text(2.5, 6, 'p-value:' + 
                     np.format_float_positional(p_value, precision=2, \
                                                unique=False, fractional=False, trim='k'))
t.set_bbox(dict(facecolor='white', alpha=1.0, edgecolor='white'))

plt.grid()
plt.gca().autoscale(enable=True, axis='x', tight=True)
plt.gca().autoscale(enable=True, axis='y', tight=True)
plt.savefig("figs/fire_ECS_test.pdf")

