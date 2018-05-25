# Retrieve freeway detector data from the State of California PeMS website.
#
# * Looks up detector health data for a set of days and a set of freeways.
# * Please configure as needed. See the "Configuration" section for details.
# 
# Source Script Copyright Brian High (https://github.com/brianhigh) and Surakshya Dhakal
# Modified by Colin Santos (https://github.com/sssantos)
# License: GNU GPL v3 http://www.gnu.org/licenses/gpl.txt

# Close connections and clear objects.
closeAllConnections()
rm(list=ls())

# Load one or more packages into memory, installing as needed.
load.pkgs <- function(pkgs, repos = "http://cran.r-project.org") {
  result <- sapply(pkgs, function(pkg) { 
    if (!suppressWarnings(require(pkg, character.only = TRUE))) {
      install.packages(pkg, quiet = TRUE, repos = repos)
      library(pkg, character.only = TRUE)}})
}

# Install packages and load into memory.
load.pkgs(c("RCurl", "XML", "plyr","data.table"))

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

# Data folder configuration - where the data files are to be stored
data.folder <- 'data'

# Session configuration - variables used to set up the HTTP session
# You should only need to change the first two (username and password).
username <- 'sssantos@ucdavis.edu'
password <- ')cg5Bopd2'
base.url <- 'http://pems.dot.ca.gov'
user.agent <- 'Mozilla/5.0'         # https://en.wikipedia.org/wiki/User_agent
cookies <- 'cookies.txt'            # https://en.wikipedia.org/wiki/HTTP_cookie

# Lanes configuration - specific freeway and direction to query
# - Freeway-lane entries must be listed as one entry per line
# - Entries much match this "regex": ^(?:I|SR|US)\\d+[NSEW]?-[NSEW]{1}$
# - Where ^(?:I|SR|US) means: starts with I or SR or US
# - And \\d+[NSEW]?- means:
#   - one or more digits 
#   - *optionally* followed by a single N or S or E or W
#   - followed by a single dash
# - And [NSEW]{1}$ means: ends with a single N or S or E or W
# - Example: SR24-W
# - Example: I880S-S
freeways.of.interest.file <- "freeways_of_interest.txt"

# Start date configuration - a vector of a single date or multiple dates
# - query date(s) must be in ISO 8601 form: YYYY-MM-DD
# - See: https://en.wikipedia.org/wiki/ISO_8601
# - Dates much match this "regex": '^\\d{4}-\\d{2}-\\d{2}$'
# - Where this means: four digits, a dash, two digits, a dash, and two digits
# Examples:
# search.date <- c('2016-02-05')
# search.date <- c('2016-02-05', '2016-02-06', '2016-02-07')
# search.date <- seq(as.Date("2015-01-01"), as.Date("2015-12-31"), "days")
search.date <- seq(as.Date("2018-04-20"), as.Date("2018-04-20"), "days")

# Read in configuration file. This file can contain the settings listed above.
if (file.exists("conf.R")) source("conf.R")

# --------------------------------------------------------------------------
# Functions
# --------------------------------------------------------------------------
## Function getStationIDs will fetch page with station ID's for a freeway
getStationIDs <- function(freeway, direction, search.date.str, 
                                  s.time.id, curl, base.url, data.folder, abspm_start = -1, abspm_end = 100000000000) {
  # Combine variables into a "lane" string
  lane <- paste('fwy=', freeway, '&dir=', direction, sep='')
  
  # Parse the search.date.str into a vector
  search.date.v <- unlist(strsplit(search.date.str, '-'))
  names(search.date.v) <- c("year", "month", "day")
  
  # Combine variables into a "start date" (sdate) string
  sdate <- paste(search.date.v[['month']], 
                 search.date.v[['day']], 
                 search.date.v[['year']], 
                 sep='%2F')
  
  # Combine variables into a "file date" (fdate) string
  fdate <- paste(search.date.v[['year']], 
                 search.date.v[['month']], 
                 search.date.v[['day']], 
                 sep='')
  
  # Page configuration - query specification for type of report page
  form.num <- '1'
  node.name <- 'Freeway'
  content <- 'elv'
  export.type <- 'text'
  
  # Combine variables into a "page" (page) string
  page <- paste('report_form=', form.num, '&dnode=', node.name, '&content=', 
                content, '&export=', export.type, sep='')
  
  # Combine variables into a "output filename" (output.filename) string
  output.filename <- paste(data.folder, '/', node.name, '-', 
                           content, '-', freeway, '-', direction, '-', fdate, 
                           '.tsv', sep='')
  
  # If the data filehas alread been saved, load the file, or get from web
  cat(freeway, "-", direction, " ")
  if (! file.exists(output.filename)) {
    tryCatch({
      # Get TSV file for the detector_health for chosen freeway and date
      r.url <- paste(base.url, '/?', page, '&fwy=', freeway, '&dir=', direction, '&_time_id=', 
                     s.time.id, '&_time_id_f=', sdate, '&pagenum_all=1&st_hv=on&st_ml=on', 
                     '&start_pm=', abspm_start, '&end_pm=', abspm_end, sep='')

  
      # Get TSV data file from website and store as a string in memory
      r = dynCurlReader()
      result.string <- getURL(url = r.url, curl = curl)
      
      # Write string to file
      writeLines(result.string, output.filename)
      
      # Read table from string into a dataframe
      freeway_data <- read.table(text=result.string, sep='\t', header=T, 
                           fill=T, quote='', stringsAsFactors=F)
    }, error=function(e) {
      cat("ERROR :",conditionMessage(e), "\n")
      return(data.frame(NULL))
    })
    
    return(sapply(freeway_data$ID, as.character))
  } else {
    # Read from file
    freeway_data <- read.table(output.filename, sep='\t', header=T, fill=T, 
                         quote='', stringsAsFactors=F)
    # Return ID's
    return(sapply(freeway_data$ID, as.character))
  }
  
  tryCatch({
    # Add variables to make this dataset unique from others and return
    if (nrow(freeway_data) > 0) freeway_data$search.date <- as.Date(search.date.str)
    return(freeway_data)
  }, error=function(e) {
    cat("ERROR :",conditionMessage(e), "\n")
    return(data.frame(NULL))
  })
}
  
getStationValues <- function(station_id, quantity, search.date.str, s.time.id, curl, base.url, granularity = 'hour', 
                             dow = c('on','on','on','on','on','on','on') , holidays = 'on', data.folder) {
  
  # Parse the search.date.str into a vector
  search.date.v <- unlist(strsplit(search.date.str, '-'))
  names(search.date.v) <- c("year", "month", "day")
  
  # Combine variables into a "start date" (sdate) string
  sdate <- paste(paste(search.date.v[['month']], 
                       search.date.v[['day']], 
                       search.date.v[['year']], 
                       sep='%2F'), 
                 '+00%3A00', sep = '')
  # valid "end date" is ~= 24 hours after start
  e.time.id <- toString(strtoi(s.time.id) + 86340)
  edate <- paste(paste(search.date.v[['month']], 
                       search.date.v[['day']], 
                       search.date.v[['year']], 
                       sep='%2F'), 
                 '+23%3A59', sep = '')
  
  
  # Combine variables into a "file date" (fdate) string
  fdate <- paste(search.date.v[['year']], 
                 search.date.v[['month']], 
                 search.date.v[['day']], 
                 sep='')

  
  # Page configuration - query specification for type of report page
  form.num <- '1'
  node.name <- 'VDS'
  content <- 'loops&tab=det_timeseries'
  export.type <- 'text'
  
  # Combine variables into a "page" (page) string
  page <- paste('report_form=', form.num, '&dnode=', node.name, '&content=', 
                content, '&export=', export.type, sep='')
  
  # Combine variables into a "output filename" (output.filename) string
  output.filename <- paste(data.folder, '/', node.name, '-', 
                           content, '-', station_id, '-', fdate, 
                           '.tsv', sep='')
  
  # If the data filehas alread been saved, load the file, or get from web
  cat(freeway, "-", direction, " ")
  # if (! file.exists(output.filename)) {
    tryCatch({
      # Get TSV file for the detector_health for chosen freeway and date
      r.url <- paste(base.url, '/?', page, '&station_id=', station_id,
                     '&s_time_id=', s.time.id, '&s_time_id_f=', sdate, 
                     '&e_time_id=', e.time.id, '&e_time_id_f=', edate,
                     '&tod=all&tod_from=0&tod_to=0',
                     '&dow_0=', dow[1],
                     '&dow_1=', dow[2],
                     '&dow_2=', dow[3],
                     '&dow_3=', dow[4],
                     '&dow_4=', dow[5],
                     '&dow_5=', dow[6],
                     '&dow_6=', dow[7],
                     '&holidays=', holidays,
                     '&q=', quantity, '&q2=&agg=on',
                     '&gn=', granularity,
                     # '&lane1=on&lane2=on&lane3=on&lane4=on&lane5=on',
                     '&pagenum_all=1', sep='')
      # Get TSV data file from website and store as a string in memory
      r = dynCurlReader()
      result.string <- getURL(url = r.url, curl = curl)
      
      # Write string to file
      writeLines(result.string, output.filename)
      
      # Read table from string into a dataframe
      freeway_data <- read.csv(text=result.string, sep='\t', header=T,
                                 fill=T, quote='', stringsAsFactors=F)
      
      
    }, error=function(e) {
      cat("ERROR :",conditionMessage(e), "\n")
      return(data.frame(NULL))
    })
    # } else {
  #   # Read from file
  #   freeway_data <- read.table(output.filename, sep='\t', header=T, fill=T, 
  #                              quote='', stringsAsFactors=F)
  #   # Return ID's
  #   return(freeway_data$ID)
  # }
  
  # tryCatch({
  #   # Add variables to make this dataset unique from others and return
  #   if (nrow(freeway_data) > 0) freeway_data$search.date <- as.Date(search.date.str)
  #   return(freeway_data)
  # }, error=function(e) {
  #   cat("ERROR :",conditionMessage(e), "\n")
  #   return(data.frame(NULL))
  # })
  return(freeway_data)
}


## Function subsetFreeways will subset freeways by those of interest
subsetFreeways <- function(freeways, freeways.of.interest.file) {
  # If there is a freeways_of_interest file, subset freeways by its contents.
  if (file.exists(freeways.of.interest.file)) {
    freeways.of.interest <- readLines(freeways.of.interest.file)
    
    # Remove quotation marks, if present
    freeways.of.interest <- gsub('["\\\']', '', freeways.of.interest)
    
    # Remove any which do not match the required format
    freeways.of.interest <- freeways.of.interest[
      grep('^(?:I|SR|US)\\d+[NSEW]?-[NSEW]{1}$', freeways.of.interest)]
    
    # Convert to dataframe and merge with "freeways" to perform subset
    freeways.of.interest <- data.frame(name=freeways.of.interest, 
                                       stringsAsFactors=F)
    freeways <- merge(freeways, freeways.of.interest, by = "name")
  }
  return(freeways)
}

indicatePostmiles <- function(freeways) {
  pmdf <- read.csv('postmiles.txt', FALSE, col.names = c('freeway','abspm_start','abspm_end'))
  indicators <- sapply(freeways$freeway, function(x) {
    a <- pmdf$abspm_start[which(x == pmdf$freeway)]
    b <- pmdf$abspm_end[which(x == pmdf$freeway)]
    cbind(c(a,b))
  })
  df <- suppressWarnings(cbind(freeways,t(indicators)))
  names(df)[4] <- 'abspm_start'
  names(df)[5] <- 'abspm_end'
  df
}


pmdf <- read.csv('postmiles.txt', FALSE, col.names = c('freeway','abspm_start','abspm_end'))
pmdf
  
## Function getFreeways() finds "freeway" choices in HTML select option tags.
# Note: You could also extract available cities and counties with this method.
getFreeways <- function(doc) {
  optValues <- xpathSApply(htmlParse(doc), 
                           '//form[@class="crossNav"]/select[@name="url"]/option', 
                           function(x) paste(xmlAttrs(x)["value"], 
                                             '&name=', xmlValue(x), sep=''))
  freeways <- optValues[grepl('dnode=Freeway', optValues)]
  freeways <- strsplit(gsub("[^&]*=", "", freeways), '&')
  freeways <- adply(.data=unname(freeways), .margins=c(1))
  freeways <- freeways[, c(3:5)]
  names(freeways) <- c("freeway", "direction", "name")
  return(freeways)
}

# --------------------------------------------------------------------------
# Main routine
# --------------------------------------------------------------------------

# Create the data folder if needed.
dir.create(file.path(data.folder), showWarnings = FALSE, recursive = TRUE)

# Load homepage, get a cookie, and parse output for a dataframe of freeways.
# From this dataframe, we can look-up the freeway number and lane directions.
# We can loop-through this dataframe (or a subset) to process many freeways.
formdata <- paste('redirect=&username=', username, '&password=', password, 
                  '&login=Login', sep='')

# Configure curl to use a cookie file and a custom user agent string.
curl <- getCurlHandle(cookiefile = cookies, cookiejar = cookies,
                      useragent = user.agent)

# Load the page into R as a string.
r = dynCurlReader()
res <- curlPerform(postfields = formdata, url = base.url, curl = curl,
                   post = 1L, writefunction = r$update)
result.string <- r$value() 


# Parse the page to get freeway data and write to a CSV file.
freeways <- getFreeways(result.string)

write.csv(freeways, paste(data.folder, "freeways.csv", sep="/"), row.names=F)

# Select only those freeways which are of interest to us.
freeways <- subsetFreeways(freeways, freeways.of.interest.file)
freeways


# Get detector health for each date and freeway and write to a CSV file.
search.date <- as.character(search.date)
search.date <- search.date[grep('^\\d{4}-\\d{2}-\\d{2}$', search.date)]



##########################
# Testing getting VDS ids
##########################
freeways <- indicatePostmiles(freeways)
freeway <- freeways$freeway[1]
direction <- freeways$direction[1]
start <- freeways$abspm_start[1]
end <- freeways$abspm_end[1]
s.time.id <- as.character(as.integer(as.POSIXct(search.date,
                                                origin="1970-01-01",
                                                tz = "GMT")))
stations <- getStationIDs(freeway, direction, search.date, s.time.id, curl, base.url, data.folder)
want_stations <- getStationIDs(freeway, direction, search.date, s.time.id, curl, base.url, data.folder, abspm_start = start, abspm_end = end)
want_stations
##########################
# Testing getting values (run testing gettings vds ids first)
##########################
#getting one station's values
# args(getStationValues)
# station_id <- want_stations[1]
# res <- getStationValues(station_id, 'speed', search.date, s.time.id, curl, base.url, data.folder = data.folder)
# res
# res2<- lapply(c('flow','occ'), function(x) {
#   getStationValues(station_id, x, search.date, s.time.id, curl, base.url, data.folder = data.folder)
# })
# 
# res[2]
# Reduce(merge,res2)

getStationMultiValues <- function(station_id, quantities, search.date, s.time.id, curl, base.url, data.folder = data.folder) {
  Reduce(merge,
         lapply(quantities, function(x) {
           getStationValues(station_id, x, search.date, s.time.id, curl, base.url, data.folder = data.folder)
         })
         )
}
  
getStationMultiValues(station_id, c('flow','occ'), search.date, s.time.id, curl, base.url, data.folder = data.folder)

getMultiStationMultiValues <- function(station_ids, quantities, search.date, s.time.id, curl, base.url, data.folder = data.folder) {
  rbindlist(lapply(station_ids, function(x) {
    getStationMultiValues(x, quantities, search.date, s.time.id, curl, base.url, data.folder = data.folder)
  }))
}

getMultiStationMultiValues(want_stations[1:2], c('flow','occ'), search.date, s.time.id, curl, base.url, data.folder = data.folder)


##########################


# Clean up. Cookies file will be written to disk. Memory will be freed.
rm(curl)
gc()

##########################################




