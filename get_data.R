# Script Purpose:
# Retrieve freeway detector data from the State of California PeMS website.
# Retrieve weather data from Dark Sky API
# Retrieve Bart Ridership Data
# Retrieve On-Ramp Flow data from the State of California PeMS website.

# PeMS data collection based off script for obtaining detector health data by Brian High (https://github.com/brianhigh) and Surakshya Dhakal
# Modified to obtain station level data and freeway level data (https://github.com/sssantos)
# License: GNU GPL v3 http://www.gnu.org/licenses/gpl.txt

# REQUIRES A TXT FILE TO INDICATE MAZE POSTMILES AND TXT TO INDICATE FREEWAYS OF INTEREST
#      A default (all) maze postmiles indicated in postmiles.txt
#      A default (all) all freeways indicated in freeways_of_interst.txt
# PLEASE SAVE DATA IN /160trafficdata folder and not in /160trafficproject

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
load.pkgs(c("RCurl", "XML", "plyr","data.table","tibble","tictoc","xlsx","stringr","rjson"))


# NOTE FROM COLIN
# PLEASE REMEMBER TO SET WORKING DIRECTORY 
# PLEASE REMEMBER TO INDICATE DATA DIRECTORIES, '/data' is for helper data and downloaded data, '/df' is for constructed data frames
data.folder      <- '/Users/sssantos/Documents/STA160/160trafficdata/data'
dataframe_folder <- '/Users/sssantos/Documents/STA160/160trafficdata/df'
# PLEASE REMEMBER TO CHANGE USERNAME AND PASSWORD

base.url   <- 'http://pems.dot.ca.gov'
user.agent <- 'Mozilla/5.0'         # https://en.wikipedia.org/wiki/User_agent
cookies    <- 'cookies.txt'            # https://en.wikipedia.org/wiki/HTTP_cookie

login_file <- '/Users/sssantos/Documents/STA160/credentials.txt'

if(file.exists(login_file)) {
  info <- read.csv(login_file, FALSE, stringsAsFactors = FALSE)
  username <- info[1,1]
  password <- info[2,1]
  mykey    <- info[3,1]
}

# If not using a login info file
## PeMS Login
# username <-
# password <-

## Dark Sky API Key
# mykey <-


############################################################################################
# Functions For Creating Freeway DF limited to MAZE
############################################################################################

# Function getFreeways receives all listed freeways from PeMS html
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

## Function subsetFreeways will subset freeways by those of interest
# Note: Requires freeways.of.interest.file
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

# Function indicatePostmiles appends desired postmile info to freeway dataframe
# NOTE: Requires postmiles.txt document in working directory
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

############################################################################################
# Functions for gathering data at Station node level (NOT RECOMMENDED FOR LARGE AMOUNTS OF DAYS/STATIONS)
############################################################################################
## Function getStationIDs will fetch page with station ID's for a freeway
# If no postmiles explicitly indicated, defaults to entire freeway
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
  
  # Form URL
  r.url <- paste(base.url, '/?', page, '&fwy=', freeway, '&dir=', direction, '&_time_id=', 
                 s.time.id, '&_time_id_f=', sdate, '&st_hv=on&st_ml=on', 
                 '&start_pm=', abspm_start, '&end_pm=', abspm_end, sep='')
  
  r = dynCurlReader()
  result.string <- getURL(url = r.url, curl = curl)
  
  # Write string to file
  writeLines(result.string, output.filename)
  
  # Read table from string into a dataframe
  freeway_data <- read.table(text=result.string, sep='\t', header=T, 
                             fill=T, quote='', stringsAsFactors=F)
  
  # Construct fuller data frame
  VDS  <- sapply(freeway_data$ID, as.character)
  Freeway  <- rep(freeway,   length(VDS))
  Direction  <- rep(direction, length(VDS))
  df <- data.frame(VDS,Freeway,Direction, stringsAsFactors = FALSE)
  list_df <- split(df, seq(nrow(df)))
  
}

# Get Station values gets specified quantity, such as 'speed', 'flow', 'occ'
# Please refer to a corresponding url query on pems for proper quantity names
# dow refers to day of week, from Sunday to Saturday
getStationValues <- function(station_df, quantity, search.date.str, s.time.id, curl, base.url, granularity = 'hour', 
                             dow = c('on','on','on','on','on','on','on') , holidays = 'on', data.folder) {
  
  station_id <- station_df[1]
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
  
  # Form URL
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

  
  r = dynCurlReader()
  result.string <- getURL(url = r.url, curl = curl)
  
  
  # Read table from string into a dataframe
  freeway_data <- read.csv(text=result.string, sep='\t', header=T,
                           fill=T, quote='', stringsAsFactors=F)
    
  # Form columns
  num_rows  <- nrow(freeway_data)
  Station   <- rep(station_df[1,1], num_rows)
  Freeway   <- rep(station_df[1,2], num_rows)
  Direction <- rep(station_df[1,3], num_rows)
  
  # Adding columns to data frame
  freeway_data <- add_column(freeway_data, Direction,  .after = 1)
  freeway_data <- add_column(freeway_data, Freeway,    .after = 1)
  freeway_data <- add_column(freeway_data, Station,    .after = 1)
  
  return(freeway_data)
}


# Gets multiple values indicated in a vector for a single station
getStationMultiValues <- function(station_id, quantities, search.date, s.time.id, curl, base.url, data.folder = data.folder) {
  Reduce(merge,
         lapply(quantities, function(x) {
           getStationValues(station_id, x, search.date, s.time.id, curl, base.url, data.folder = data.folder)
         })
  )
}

# Gets multiple values indicated ina  vector for multiple stations indicated in a vector
getMultiStationMultiValues <- function(station_ids, quantities, search.date, s.time.id, curl, base.url, data.folder = data.folder) {
  rbindlist(lapply(station_ids, function(x) {
    getStationMultiValues(x, quantities, search.date, s.time.id, curl, base.url, data.folder = data.folder)
  }))
}

# Build data frame of data from multiple stations, at station level
df_build <- function() {rbindlist(lapply(search.dates, function (y) { 
  search.date <- y
  s.time.id <- as.character(as.integer(as.POSIXct(search.date,
                                                  origin="1970-01-01",
                                                  tz = "GMT")))
  # Getting all sensor IDs
  all_sensor_ids <- lapply(data.frame(t(freeways)), function(x) {
    name  <- x[[2]]
    dir   <- x[[3]]
    start <- x[[4]]
    end   <- x[[5]]
    getStationIDs(name, dir, search.date, s.time.id,  curl, base.url, data.folder, abspm_start = start, abspm_end = end)
  })
  
  all_sensor_ids <- unlist(all_sensor_ids, recursive = FALSE)
  
  getMultiStationMultiValues(all_sensor_ids, c('flow','occ','speed'), search.date, s.time.id, curl, base.url, data.folder = data.folder)
}))
}

# Example Usage with timing
if(FALSE) {
  tic()
  example_df <- df_build()
  toc()
  write.csv(example_df, "/Users/sssantos/Documents/STA160/160trafficdata/data/example_df")
}
############################################################################################
# Functions for getting Data at Freeway Level
############################################################################################
# Get multistation data at Freeway level from spatial data option
# The data for each station of a freeway is averaged
getSpatial <- function(freeway, direction, abspm_start, abspm_end, quantity = 'flow', 
                            start_search.date.str = start, end_search.date.str = end,
                            curl, base.url, granularity = 'hour', data.folder) {
  
  s.time.id <- as.character(as.integer(as.POSIXct(start_search.date.str,
                                                  origin="1970-01-01",
                                                  tz = "GMT")))
  e.time.id <- as.character(as.integer(as.POSIXct(end_search.date.str,
                                                  origin="1970-01-01",
                                                  tz = "GMT")))
  
  
  # Parse the search.date.str into a vector
  start_search.date.v <- unlist(strsplit(start_search.date.str, '-'))
  names(start_search.date.v) <- c("year", "month", "day")
  
  # Combine variables into a "start date" (sdate) string
  sdate <- paste(paste(start_search.date.v[['month']], 
                       start_search.date.v[['day']], 
                       start_search.date.v[['year']], 
                       sep='%2F'), 
                 '+00%3A00', sep = '')
  
  # Parse the search.date.str into a vector
  end_search.date.v <- unlist(strsplit(end_search.date.str, '-'))
  names(end_search.date.v) <- c("year", "month", "day")
  
  # Combine variables into a "start date" (sdate) string
  edate <- paste(paste(end_search.date.v[['month']], 
                       end_search.date.v[['day']], 
                       end_search.date.v[['year']], 
                       sep='%2F'), 
                 '+00%3A00', sep = '')
  
  
  
  # Combine variables into a "file date" (fdate) string
  fdate <- paste(start_search.date.v[['year']], 
                 start_search.date.v[['month']], 
                 start_search.date.v[['day']], 
                 sep='')
  
  # Page configuration - query specification for type of report page
  form.num <- '1'
  node.name <- 'Freeway'
  content <- 'spatial&tab=mst'
  export.type <- 'xls'
  
  # Combine variables into a "page" (page) string
  page <- paste('report_form=', form.num, '&dnode=', node.name, '&content=', 
                content, '&export=', export.type, sep='')
 
  r.url <- paste(base.url, '/?', page, '&fwy=', freeway, '&dir=', direction,
                 '&s_time_id=', s.time.id, '&s_time_id_f=', sdate, 
                 '&e_time_id=', e.time.id, '&e_time_id_f=', edate,
                 '&q=', quantity, '&q2=&agg=on',
                 '&start_pm=', abspm_start, '&end_pm=', abspm_end,
                 '&gn=', granularity, '&ihv=on&html.x=50&html.y=12',
                 sep='')
  r.url <- str_replace_all(string=r.url, pattern=" ", repl="")
  ## Checking url
  # print(r.url)
  output.filename <- paste(data.folder, '/', node.name, '_',
                           quantity, '_', freeway, direction, '_', fdate,
                           '.xls', sep='')
  
  # Skip getting downloading file if already have
  if(!file.exists(output.filename)) {
    
    r = dynCurlReader()
    z <- getURLContent(url = r.url, curl = curl, binary = TRUE)
  

 
    con = file(output.filename, "wb")
    .Internal(writeBin(z, con, 1, FALSE, TRUE))
    close(con)
  }


  freeway_data <- read.xlsx2(output.filename,1)
  
  #Formatting data frame
  fd <- sapply(freeway_data, as.character)
  x <- ncol(fd)
  c <- fd[,-c(1,x-1,x)]
  c <- sapply(data.frame(c, stringsAsFactors = FALSE), as.numeric)
  avg_col <- round((rowSums(c)) / (x-3))
  df <- data.frame(fd[,1],avg_col,as.numeric(fd[,x-1]), as.numeric(fd[,x]))
  frwy_col <- rep(freeway,   nrow(df))
  dir_col  <- rep(direction, nrow(df))
  df <- add_column(df, dir_col, .after = 1 )
  df <- add_column(df, frwy_col,.after = 1 )
  averaged_value <- paste("Average ", quantity, sep = "")
  names(df) <- c("TimeStamp", "Freeway", "Direction", averaged_value,"Lane Points","Percent Observed")
  df
}

# Same as getSpatial but for multiple values ex: 'flow','occ', and 'speed'
getSpatialMulti <- function(freeway, direction, abspm_start, abspm_end, quantities = c('flow','occ','speed'), 
                                  start_search.date.str = start, end_search.date.str = end,
                                  curl, base.url, granularity = 'hour', data.folder) {

  Reduce(merge,
         lapply(quantities, function(x) {
           burl = base.url
           getSpatial(freeway, direction, quantity = x, abspm_start = abspm_start, abspm_end = abspm_end, 
                      start_search.date.str = start, end_search.date.str = end,
                      curl, base.url, granularity = 'hour', data.folder)
         })
  )
}

# Builds data frames of station data for multiple values on multiple freeways using the freeways_df
build_spatial_multistation_df <- function(freeways_df, start, end, quantities, base.url) {rbindlist(lapply(data.frame(t(freeways)), function(x) {
  name  <- x[[2]]
  dir   <- x[[3]]
  s_pm  <- x[[4]]
  e_pm  <- x[[5]]
  getSpatialMulti(name,dir, abspm_start = s_pm, abspm_end = e_pm, quantities, start_search.date.str = start, end_search.date.str = end,
             curl = curl, base.url = base.url, data.folder = data.folder)
}))
}

##############################################
# STARTING ROUTINE AND CONFIGURATION
#       This section gets you set up, please configure any parts you find necessary
##############################################
if(!FALSE){
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
# NOTE: These dates only matter for acquiring data at the Station Node
search.dates <- seq(as.Date("2018-04-20"), as.Date("2018-04-22"), "days")
search.dates <- as.character(search.dates)
search.dates <- search.dates[grep('^\\d{4}-\\d{2}-\\d{2}$', search.dates)]
# Read in configuration file. This file can contain the settings listed above.
if (file.exists("conf.R")) source("conf.R")

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

# Since we are intersted in a specific postmile range limited to the Maze
freeways <- indicatePostmiles(freeways)

}
##############################################
# Example Usage of getting Freeway > Spatial > Multistation Data
##############################################
# Example usage on one freeway
if(FALSE) {
  # MAX RANGE IS 1 MONTH
  # Dates need to be formatted as such:
  start <- "2018-04-20"
  end   <- "2018-04-22"
  start <- as.character(start)
  end   <- as.character(end)
  
  # Using the main function to build the df
  # NOTE: base.url always = base.url (issues when using as default)
  # To see the list of maze freeways, uncomment and run the following line
  # freeways
  # EX: Want just the first two freeways?
  #     Use this as your function argument: freeways[c(1,2),]
  # Other than that just change the start and end dates above ^^^^ and the list of desired quantities
  
  example_df <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed"), base.url = base.url)
  # Write the csv so that we can all access it 
  write.csv(example_df, paste(dataframe_folder, '/', 'example.csv', sep=''))
}


##########################################
# Data Collection
##########################################
# Getting Group's Requested Data
# NOTE: Max query time frame is 1 month
if(!FALSE){
  # 2007 Collapse Data
  start <- as.character("2007-04-01")
  end   <- as.character("2007-04-30")
  half_1 <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)

  start <- as.character("2007-05-01")
  end   <- as.character("2007-05-31")
  half_2 <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)

  write.csv(rbind(half_1, half_2), paste(dataframe_folder, '/', '2007.csv', sep=''))
  
  # Extending 2007 data
  start <- as.character("2007-03-01")
  end   <- as.character("2007-03-30")
  ant1 <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)
  
  start <- as.character("2007-06-01")
  end   <- as.character("2007-06-29")
  pos1  <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)
  
  start <- as.character("2007-07-01")
  end   <- as.character("2007-07-31")
  pos2 <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)
  
  write.csv(rbind(ant1, half_1, half_2, pos1, pos2), paste(dataframe_folder, '/', '2007extended.csv', sep=''))
  
  
  #2006 For Comparison
  start <- as.character("2006-04-01")
  end   <- as.character("2006-04-30")
  half_1 <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)

  start <- as.character("2006-05-01")
  end   <- as.character("2006-05-31")
  half_2 <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)

  write.csv(rbind(half_1, half_2), paste(dataframe_folder, '/', '2006.csv', sep=''))

  # Extending 2006 data
  start <- as.character("2006-03-01")
  end   <- as.character("2006-03-31")
  ant1 <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)
  
  start <- as.character("2006-06-01")
  end   <- as.character("2006-06-30")
  pos1  <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)
  
  start <- as.character("2006-07-01")
  end   <- as.character("2006-07-31")
  pos2 <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)
  
  write.csv(rbind(ant1, half_1, half_2, pos1, pos2), paste(dataframe_folder, '/', '2006extended.csv', sep=''))
  
  
  #2009 Emergency Bridge Closure
  start <- as.character("2009-10-13")
  end   <- as.character("2009-11-10")
  whole <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)

  write.csv(whole, paste(dataframe_folder, '/', '2009.csv', sep=''))

  #2008 Comparison
  start <- as.character("2008-10-13")
  end   <- as.character("2008-11-10")
  whole <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)

  write.csv(whole, paste(dataframe_folder, '/', '2008.csv', sep=''))

  #2013 Labor Day Bridge Closure
  start <- as.character("2013-8-17")
  end   <- as.character("2013-9-14")
  whole <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)

  write.csv(whole, paste(dataframe_folder, '/', '2013.csv', sep=''))

  #2012
  start <- as.character("2012-8-17")
  end   <- as.character("2012-9-14")
  whole <- build_spatial_multistation_df(freeways, start, end, c("flow","occ","speed","del_60"), base.url = base.url)

  write.csv(whole, paste(dataframe_folder, '/', '2012.csv', sep=''))
}

###################################
# Functions for getting weather data
###################################
# Oakland used for weather data since Maze is primarily within Oakland
oakland_lat <- '37.8044' 
oakland_lon <- '-122.2711' 

# Gets data from DarkSky api and builds data frame for single date
getDarkSky <- function(lat = oakland_lat, lon = oakland_lon,query_type = 'forecast',
                       date, misc = "?exclude=currently,flags,minutely,daily", apikey = mykey) {
  
  time.id <- as.character(as.integer(as.POSIXct(date, origin="1970-01-01", tz = "GMT")))
  
  r.url <- paste("https://api.darksky.net/", query_type, "/", apikey, "/", lat, ",", lon, ",", time.id, misc, sep = "")
  r.url <- str_replace_all(string=r.url, pattern=" ", repl="")
  # Checking url
  # return(r.url)

  z <- fromJSON(getURLContent(url = r.url))
  rbindlist(lapply(z$hourly$data, data.frame), fill = TRUE)
}

# Gets data from DarkSky api and builds data frame for range of dates
getDarkSkyRange <- function(lat = oakland_lat, lon = oakland_lon,query_type = 'forecast',
                            start_date, end_date, misc = "?exclude=currently,flags,minutely,daily", apikey = mykey) {
  time_range <- seq(as.Date(start_date), as.Date(end_date), "days")
  rbindlist(lapply(time_range, function (x) {
    getDarkSky(lat = lat, lon = lon,query_type = query_type,
               x, misc = misc, apikey = apikey) 
  } ), fill = TRUE)
}

##########################################
# Collecting Weather Data
##########################################
if(!FALSE){
  # 2007 Collapse Data
  forecastdf <- getDarkSkyRange(start_date = "2007-04-01", end_date = "2007-05-31")
  write.csv(forecastdf, paste(dataframe_folder, '/', 'forecast2007.csv', sep=''))

  #2006 For Comparison
  forecastdf <- getDarkSkyRange(start_date = "2006-04-01", end_date = "2006-05-31")
  write.csv(forecastdf, paste(dataframe_folder, '/', 'forecast2006.csv', sep=''))

  #2009 Emergency Bridge Closure
  forecastdf <- getDarkSkyRange(start_date = "2009-10-13", end_date = "2009-11-10")
  write.csv(forecastdf, paste(dataframe_folder, '/', 'forecast2009.csv', sep=''))

  #2008 Comparison
  forecastdf <- getDarkSkyRange(start_date = "2008-10-13", end_date = "2008-11-10")
  write.csv(forecastdf, paste(dataframe_folder, '/', 'forecast2008.csv', sep=''))

  #2013 Labor Day Bridge Closure
  forecastdf <- getDarkSkyRange(start_date = "2013-08-13", end_date = "2013-09-14")
  write.csv(forecastdf, paste(dataframe_folder, '/', 'forecast2013.csv', sep=''))

  #2012
  forecastdf <- getDarkSkyRange(start_date = "2012-10-13", end_date = "2012-11-10")
  write.csv(forecastdf, paste(dataframe_folder, '/', 'forecast2012.csv', sep=''))
}

##########################################


#########################################
# Functions for Getting Bart Data
#########################################
# Extracts just bart line ridership data from an xlsx, mainly for sheet 1, probably 2 and 3
formBartLines <- function(file,sheet) {
  a <- (read.xlsx(file,sheet))
  values <- (data.frame(a[11:18,(ncol(a) - 7):ncol(a)]))
  rownames(values)  <- a[11:18,(ncol(a) - 8)]
  colnames(values)  <- t(a[10,(ncol(a) - 7):ncol(a)])
  values
}


getBartLines <- function(year, month, sheet = 1) {
  main_dir  <- paste(data.folder, paste('ridership', year,sep = ""),sep = "/")
  listed    <- list.dirs(main_dir)
  file_name <- grep(month,list.files(listed[length(listed)]), value = TRUE)
  file_dest <- paste(listed[length(listed)], file_name, sep = "/")
  df <- formBartLines(file_dest,sheet = sheet)
  day <- "Wkdy"
  if(sheet == 2) {
    day <- "Sat"
  }
  if(sheet == 3) {
    day <- "Sun"
  }
  df_name <- paste('ridership',month,year,day, sep = "")
  write.csv(df, paste(dataframe_folder, '/', df_name, ".csv", sep=''))
  df
} 

getTotalEE <- function(year, month, sheet) {
  main_dir  <- paste(data.folder, paste('ridership', year,sep = ""),sep = "/")
  listed    <- list.dirs(main_dir)
  file_name <- grep(month,list.files(listed[length(listed)]), value = TRUE)
  file_dest <- paste(listed[length(listed)], file_name, sep = "/")
  a <- read.xlsx(file_dest, sheetIndex = sheet) 
  if(year == "2012" || year == "2013") {
    return(as.numeric(as.character(a[46,46])))  
  } else if(year == "2011" && month != "January") {
    return(as.numeric(as.character(a[46,46]))) 
  } 
  else {
    return(as.numeric(as.character(a[45,45]))) 
  }
}

##########################################
# Getting Entry-Exit Bart Line Data
#########################################
years_of_interest <- seq("2004", "2013")

# Collecting and unzipping rider ship zip data
for(i in years_of_interest){
  file_name <- paste("ridership", i,".zip",  sep = "")
  full_dest <- paste(data.folder, file_name, sep ="/")
  # if(!file.exists(full_dest)) {
    url <- paste('https://www.bart.gov/sites/default/files/docs/ridership_',i,'.zip', sep = "")
    z = getURLContent(url, binary = TRUE)
    con = file(full_dest, "wb")
    .Internal(writeBin(z, con, 1, FALSE, TRUE))
    close(con)
  # }
  folder_name <- paste("ridership", i,  sep = "")
  folder_dest <- paste(data.folder, folder_name, sep ="/")
  unzip(full_dest, exdir = folder_dest)
}

# Getting line usage
# for(i in 1:3) {
#   getBartLines("2009", "October", sheet = i)
#   getBartLines("2009", "November",sheet = i)
#   getBartLines("2008", "October", sheet = i)
#   getBartLines("2008", "November",sheet = i)
#   
#   getBartLines("2013", "August",   sheet = i)
#   getBartLines("2013", "September",sheet = i)
#   getBartLines("2012", "August",   sheet = i)
#   getBartLines("2012", "September",sheet = i)
#   
#   # 2007 DOES NOT COME WITH LINE DATA!!!
#   # getBartLines("2007", "April",sheet = i)
#   # getBartLines("2007", "May",  sheet = i)
#   getBartLines("2006", "April",sheet = i)
#   getBartLines("2006", "May",  sheet = i)
# }

# November 2013 has issues, but we don't expect to use it for now anyways
month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

total_ridership <- lapply(month_names, function(x) {
  sapply(years_of_interest, getTotalEE, month = x, sheet = 1)
})
total_ridership <- as.data.frame(rbindlist(lapply((lapply(total_ridership, data.frame, stringsAsFactors = FALSE )),transpose)))
rownames(total_ridership) <- month_names
colnames(total_ridership) <- years_of_interest
total_ridership <- round(total_ridership)
write.csv(total_ridership, paste(dataframe_folder, '/', "yearly_ridership", ".csv", sep=''))



##########################################
# Clean up. Cookies file will be written to disk. Memory will be freed.
rm(curl)
gc()
##########################################



















# ###########################################################################
# # Functions To Collect On-Ramp Data (UNUSED BECAUSE ON-RAMP DATA DOESN'T EXIST FOR MAZE TILL LATE 2011)
# ###########################################################################
# ## Function getOnRampIDs will fetch page with station ID's for a freeway's on-ramps
# # If no postmiles explicitly indicated, defaults to entire freeway
# getOnRampIDs <- function(freeway, direction, search.date.str,
#                           s.time.id, curl, base.url, data.folder, abspm_start = -1, abspm_end = 100000000000) {
#   # Combine variables into a "lane" string
#   lane <- paste('fwy=', freeway, '&dir=', direction, sep='')
#   
#   # Parse the search.date.str into a vector
#   search.date.v <- unlist(strsplit(search.date.str, '-'))
#   names(search.date.v) <- c("year", "month", "day")
#   
#   # Combine variables into a "start date" (sdate) string
#   sdate <- paste(search.date.v[['month']], 
#                  search.date.v[['day']], 
#                  search.date.v[['year']], 
#                  sep='%2F')
#   
#   # Combine variables into a "file date" (fdate) string
#   fdate <- paste(search.date.v[['year']], 
#                  search.date.v[['month']], 
#                  search.date.v[['day']], 
#                  sep='')
#   
#   # Page configuration - query specification for type of report page
#   form.num <- '1'
#   node.name <- 'Freeway'
#   content <- 'elv'
#   export.type <- 'text'
#   
#   # Combine variables into a "page" (page) string
#   page <- paste('report_form=', form.num, '&dnode=', node.name, '&content=', 
#                 content, '&export=', export.type, sep='')
#   
#   # Combine variables into a "output filename" (output.filename) string
#   output.filename <- paste(data.folder, '/', node.name, '-', 
#                            content, '-', freeway, '-', direction, '-', fdate, 
#                            '.tsv', sep='')
#   
#   # Form URL
#   r.url <- paste(base.url, '/?', page, '&fwy=', freeway, '&dir=', direction, '&_time_id=', 
#                  s.time.id, '&_time_id_f=', sdate, '&st_or=on', 
#                  '&start_pm=', abspm_start, '&end_pm=', abspm_end, sep='')
#   
#   r = dynCurlReader()
#   result.string <- getURL(url = r.url, curl = curl)
#   
#   # Write string to file
#   writeLines(result.string, output.filename)
#   
#   # Read table from string into a dataframe
#   freeway_data <- read.table(text=result.string, sep='\t', header=T, 
#                              fill=T, quote='', stringsAsFactors=F)
#   
#   # Construct fuller data frame
#   VDS  <- sapply(freeway_data$ID, as.character)
#   Freeway  <- rep(freeway,   length(VDS))
#   Direction  <- rep(direction, length(VDS))
#   df <- data.frame(VDS,Freeway,Direction, stringsAsFactors = FALSE)
#   list_df <- split(df, seq(nrow(df)))
# }


