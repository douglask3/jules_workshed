library(raster)
library(ncdf4)
library(gitBasedProjects)
setupProjectStructure()

inputs_dir = '../LimFIRE/data/hyde_land/'
inputs_fid = 'popd'
outputs_eg = 'data/jules_eg_input/PD_2013.nc'

remove_mask = TRUE

years = 2000:2014


comment = list('data description' = 'HYDE3.2 regridded for JULES input.',
			   'note of file structure and lat and lon' = "File based on previous JULES inputs. Note lat and lon are not quite right. See previous comments for source of file structure.",
               'data reference'   = 'Klein Goldewijk, K. , A. Beusen, M. de Vos and G. van Drecht (2011). The HYDE 3.1 spatially explicit database of human induced land use change over the past 12,000 years, Global Ecology and Biogeography20(1): 73-86. DOI: 10.1111/j.1466-8238.2010.00587.x.
                                     Klein Goldewijk, K. , A. Beusen, and P. Janssen (2010). Long term dynamic modeling of global population and built-up area in a spatially explicit way, HYDE 3 .1. The Holocene20(4):565-573. http://dx.doi.org/10.1177/0959683609356587',
               'data url'         = 'ftp://ftp.pbl.nl/hyde/hyde32/2017_beta_release/001/zip/',
               'data variable'    = 'population density',
			   'data units'       = '(inhabitants/km2)',
			   'repo URL'         = gitRemoteURL(),
			   'revision number'  = gitVersionNumber())
			   

convert_regular_2_pacific_centric <- function(dat, tempWrite = FALSE) {
	library(rasterExtras)
    if (xmax(dat) > 180) return(dat)

    index = 1:length(values(dat[[1]]))

    xyz = cbind(xyFromCell(dat,index), values(dat))
    x = xyz[, 1]
	
    test = x <0
	
    x[test] = x[test] + 360

    xyz[,1] = x
    dat = rasterFromXYZ(xyz, crs = projection(dat))
    if (tempWrite) dat = writeRaster(dat, file = memSafeFile())
    return(dat)
}

convert_pacific_centric_2_regular <- function(dat, tempWrite = FALSE) {
    if (xmax(dat) < 180) return(dat)

    index = 1:length(values(dat[[1]]))

    xyz = cbind(xyFromCell(dat,index), values(dat))
    x = xyz[, 1]
    test = x > 180

    x[test] = x[test] - 360

    xyz[,1] = x
    dat = rasterFromXYZ(xyz, crs = projection(dat))
    if (tempWrite) dat = writeRaster(dat, file = memSafeFile())
    return(dat)
}

			   

example = raster(outputs_eg)
example_clean = example

extent(example_clean) = extent(c(0, 360, -90, 90))
example_clean = convert_pacific_centric_2_regular(example_clean)



addComments2nc <- function(nc, comments) {
	attPutStandard <- function(name, val)
			ncatt_put(nc, 0, name, val)
			
	mapply(attPutStandard, names(comment), comment)
}

input_files = list.files(inputs_dir, full.names = TRUE, recursive = TRUE)
input_files = input_files[grepl(inputs_fid, input_files)]

yr = years[1]

makePopDenYear <- function(yr) {
	
	## make a copy of example input file
	output_file =  paste(outputs_dir, 'PD_', yr, '.nc', sep = "")
	file.copy(outputs_eg, output_file, overwrite = TRUE)
	
	## Open hyde data
	file    =  input_files[grepl(yr, input_files)]
	print(file)
	data    = raster(file)
	
	if (remove_mask) data[is.na(data)] = 0.0
	
	## Resample to example grod
	data_rg = raster::resample(data, example_clean)
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