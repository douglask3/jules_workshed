######################################################################
## cfg																##
######################################################################
## required libs
library(raster)
library(ncdf4)
library(gitBasedProjects)
setupProjectStructure()
sourceAllLibs()

## File list
inputs_dir = '../LimFIRE/data/hyde_land/'
inputs_fid = 'popd_'
outputs_eg = 'data/jules_eg_input/PD_2013.nc'

## Parameters
years = 1850:2016
remove_mask = TRUE


comment = list('data description' = 'HYDE3.2 regridded for JULES input.',
			   'note of file structure and lat and lon' = "File based on previous JULES inputs. Note lat and lon are not quite right. See previous comments for source of file structure.",
               'data reference'   = 'Klein Goldewijk, K. , A. Beusen, M. de Vos and G. van Drecht (2011). The HYDE 3.1 spatially explicit database of human induced land use change over the past 12,000 years, Global Ecology and Biogeography20(1): 73-86. DOI: 10.1111/j.1466-8238.2010.00587.x.
                                     Klein Goldewijk, K. , A. Beusen, and P. Janssen (2010). Long term dynamic modeling of global population and built-up area in a spatially explicit way, HYDE 3 .1. The Holocene20(4):565-573. http://dx.doi.org/10.1177/0959683609356587',
               'data url'         = 'ftp://ftp.pbl.nl/hyde/hyde32/2017_beta_release/001/zip/',
               'data variable'    = 'population density',
			   'data units'       = '(inhabitants/km2)',
			   'repo URL'         = gitRemoteURL(),
			   'revision number'  = gitVersionNumber())			   

######################################################################
## setup    														##
######################################################################
## open example data
example = raster(outputs_eg)
example_clean = example

## Make a version with corrected coordinates
extent(example_clean) = extent(c(0, 360, -90, 90))
example_clean = convert_pacific_centric_2_regular(example_clean)

strsplitbyN <- function(X, i, ...) 
	sapply(X, function(x) strsplit(x, ...)[[1]][i])

## Listing input files
input_files = list.files(inputs_dir, full.names = TRUE, recursive = TRUE)
input_files = input_files[grepl(inputs_fid, input_files)]
input_years = strsplitbyN(input_files, 2, inputs_fid)
input_years = as.numeric(strsplitbyN(input_years, 1, 'AD.asc'  ))
c(input_years, index) := sort.int(input_years, index.return=TRUE)
input_files = input_files[index]


yr = years[1]

######################################################################
## make new inputs													##
######################################################################
makePopDenYear <- function(yr) {
	
	## make a copy of example input file
	output_file =  paste(outputs_dir, 'PD_HYDEv3.2', yr, '.nc', sep = "")
	file.copy(outputs_eg, output_file, overwrite = TRUE)
	
	## Open hyde data
	
	index = which(yr == input_years)
	if (length(index) == 0) {
		diff = input_years - yr
		index = which.min(abs(diff))
		if (diff[index] > 0) index = c(index - 1, index)
			else index = c(index, index + 1)
		diff = abs(diff[index])
	}
	file    =  input_files[index]
	cat("====\n", "year:", yr, "\n")
	cat("files:\n", paste("\t", file, "\n"))
	data    = stack(file)
	
	if (remove_mask) data[is.na(data)] = 0.0
	
	## Resample to example grod
	data_rg = raster::resample(data, example_clean)
	if (nlayers(data_rg) > 1) 
		data_rg = sum((data_rg * 1/diff)) / sum(1/diff)
	
	data_rg = convert_regular_2_pacific_centric(data_rg)
	nrows = nrow(data_rg)
	
	## Covert from raster and orientate correctly
	vdata = vdatai =  matrix(data_rg, ncol = nrows)
	
	for (i in 1:nrows) vdata[, i] = vdatai[, nrows - i + 1]
	
	## Output to newly made ouput file
	nc = nc_open(output_file, write = TRUE)
		ncvar_put( nc,  names(nc[['var']])[1], vdata )	# no start or count: write all values
		addComments2nc(nc, comments)
	nc_close(nc)
}

lapply(years, makePopDenYear)