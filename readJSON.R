getRawData <- function(){
	# Returns the raw data as a nice dataframe.
	# The minimum expectation is that the tar.xz data file is present.
	library("jsonlite")

	dataRoot = "../../DATA/DRT2/"
	rawDataFolder = paste0(dataRoot, "RAWDATA/")
	rawDataTarball = paste0(dataRoot, "RAWDATA.tar.xz")
	datafileName = paste0(dataRoot, "drt2.RData")

	if(file.exists(datafileName)) {
		cat("Loading", datafileName, "\n")
		load(datafileName) # Overwrites the local rawdata
		return(rawdata)
	}
	untar(rawDataTarball, exdir=dataRoot) # Overwrite the folder (if it existed)
	cat("Reading files from folder", rawDataFolder, "\n")
	files = list.files(rawDataFolder, full.names=TRUE)
	rawdata <- do.call(rbind, lapply(files, fromJSON))
	cat("Writing", datafileName, "\n")
	save(rawdata, file=datafileName)
	return(rawdata)
}
