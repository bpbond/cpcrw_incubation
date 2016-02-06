# Process Picarro data for our CPCRW lab experiment
# This script reads all available Picarro outputs in `data/picarro/`,
# concatenating and writing to an `outputs/rawdata.csv.gz` file.
# Ben Bond-Lamberty July 2015

source("R/0-functions.R")

SCRIPTNAME  	<- "1-data.R"
PROBLEM       <- FALSE

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
  
  d <- read.table(f, header = TRUE)
  
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
  filelist <- list.files(path = input_path, pattern = "dat$|dat.gz$|dat.zip$", 
                         recursive = TRUE)
  ncolumns <- NA
  for(f in seq_along(filelist)) {
    d <- read_outputfile(file.path(input_path, filelist[f]))

    if(f > 1 & ncol(d) != ncolumns)
      stop("Columns differ between files!")
    ncolumns <- ncol(d)
    
    first <- !file.exists(tempfile)
    write.table(d, tempfile, row.names = FALSE, append = !first, 
                col.names = first, sep = ",")
  }
}


# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE)

printlog("Welcome to", SCRIPTNAME)

printlog("Data directory is", DATA_DIR)

tf <- tempfile()
printlog("Tempfile is", tf)

process_directory(file.path(DATA_DIR), tf)

printlog(SEPARATOR)
printlog("Reading in full data set...")
rawdata <- readr::read_csv(tf, col_types = "ccddddiiiddddddddc")
print_dims(rawdata)
print(summary(rawdata))

printlog("Writing output file...")
save_data(rawdata, scriptfolder = FALSE, gzip = TRUE)

printlog("All done with", SCRIPTNAME)
closelog()

if(PROBLEM) warning("There was a problem - see log")
