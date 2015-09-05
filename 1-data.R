# Process Picarro data for Peyton's DWP lab experiment
# This script reads all available data, writing to a `rawdata` file
# Ben Bond-Lamberty July 2015

source("0-functions.R")

library(stringr)

SCRIPTNAME  	<- "1-data.R"
DATA_DIR      <- "data/picarro/"

# -----------------------------------------------------------------------------
# read a single output file, returning data frame
read_outputfile <- function(fqfn) {
  printlog("Reading", fqfn)
  stopifnot(file.exists(fqfn))
  
  f <- fqfn
  if(grepl(".gz$", fqfn)) {
    f <- gzfile(fqfn)
  } else if(grepl(".zip$", fqfn)) {
    f <- unz(fqfn)
  }
  
  d <- read.table(f, header=T)
  
  print_dims(d)
  
  # Add ancillary data
  d$file <- basename(fqfn)
  #  d$dir <- dirname(fqfn)
  
  return(d)
} # read_outputfile

# -----------------------------------------------------------------------------
# scan a directory and process all files in it, returning tempfile names
process_directory <- function(input_path, tempfile) {
  samplenum <- 0
  filelist <- list.files(path=input_path, pattern="dat$|dat.gz$|dat.zip$", recursive=T)
  ncolumns <- NA
  for(f in seq_along(filelist)) {
    d <- read_outputfile(file.path(input_path, filelist[f]))

    if(f > 1 & ncol(d) != ncolumns)
      stop("Columns differ between files!")
    ncolumns <- ncol(d)
    
    first <- !file.exists(tempfile)
    write.table(d, tempfile, row.names=FALSE, append=!first, col.names=first, sep=",")
  }
}


# ==============================================================================
# Main 

sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Data directory is", DATA_DIR)

tf <- tempfile()
printlog("Tempfile is", tf)

process_directory(file.path(DATA_DIR), tf)

printlog(SEPARATOR)
printlog("Reading in full data set...")
rawdata <- readr::read_csv(tf)
print_dims(rawdata)
print(summary(rawdata))

printlog("Writing output file...")
save_data(rawdata, scriptfolder=FALSE, gzip=TRUE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
