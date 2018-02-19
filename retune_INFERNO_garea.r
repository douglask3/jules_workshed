#########
## cfg ##
#########
library(raster)
library(gitBasedProjects)

vfFiles = paste(data_dir, c(obs = '', mod = ''))
baFiles = paste(data_dir, c(md1 = '', md2 = ''))

########
## open ##
##########

vf = lapply(vfFiles, stack)
ba = lapply(baFiles, stack)

modFPC <- function(M1, f1, M0, f0, FPC, BA1, BA0)
    FPC * (1 - M1) * (1 - f1*BA1) / (1 - M0) * (1 - f0 * BA0)




