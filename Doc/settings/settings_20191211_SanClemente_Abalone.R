# User-defined variables -------------------------------------------------------
# ROV database information
db.source <- "Access" # Access or SQL
db.dir    <- here("Data", "ROV_atsea_20191209.accdb")  

# Dive information
# If only processing one dive, dir.start and dir.end are the same
dir.start 	<- "19-345A" # First dive
dir.end 	  <- "19-352F" # Last dive
dir.rm      <- c("19-351A") # List of dives to remove manually

# Precision options
options(digits = 9)

# ROV information --------------------------------------------------------------
# Is the CTD present (this will almost always be TRUE)
ctd.on <- TRUE

# Calculate pitch from DVL pitch/roll
adjust.pitch <- FALSE

# Which ROV was used (e.g., HDHV or Phantom)
ROV <- "HDHV"

# Photo information ------------------------------------------------------------
# Fix photo date/time
# If set correctly, use # '+="0:0:0 0:0:0"', else -=Y:M:D h:m:s
timefix <- '+="0:0:0 0:0:0"' 

# Navigation data information --------------------------------------------------
# Process NAV date from DAT files ####
if (.Platform$OS.type == "unix") { 
  # if working on a Mac
  dat.root 	<- "/Users/kevinstierhoff/NAVDATA" 	# This is the WinFrog project directory
  photo.root 	<- "/Users/kevinstierhoff/PHOTOS" # This is the photo directory
} else { 
  # if working on a Windows PC
  dat.root 	<- "C:/ROV_DATA/NAVDATA" # This is the WinFrog project directory
  photo.root 	<- "C:/ROV_DATA/PHOTOS"#  This is the photo directory
}

# Interval for Loess smoother used to plot nav data
nav.smoother <- 15