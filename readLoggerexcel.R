## Read info from excel logger files.  
## 
## Jim Laundre 2020-03-04
## Revised  
# load PACKAGES -------------------------------------------------------
library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(readr)
library(janitor)
library(rstudioapi)
#----------- FUNCTIONS ---------------------------------------------------------------

#***********Read yearly data logger excel files************
#   These files have named sheets "hourly".  For MAT89 the hourly data was changed to half
#   data in 201??

read.processedfile <- function(file_names,sheetname,var_names) {
  
 df<- map_dfr(file_names, function(x) {
    #get the column names of the current excel file
    file_col <-
      as.data.frame(names(read_excel(x, sheet = sheetname, n_max = 0)),
                    stringsAsFactors = FALSE) %>% setNames(nm = "col") #give it a better name
    # Match the type to each column name and add a column with matching types and replace any NA with skip
    file_col$type <-
      var_names$type[match(file_col$col, var_names$var)] %>% replace_na("skip")
    #Some excel sheets have blank column at the end; so limit to number of named columns
    cno <-
      c(1, length(file_col$col)) 
    temp_db <- read_excel(
      x,
      sheet = sheetname,
      col_names = T,
      col_types = file_col$type,
      cell_cols(cno)
    )
    # rename columns to standard names using the newname column of the var_names csv file
    name_match = match(var_names$var, names(temp_db))
    names(temp_db)[na.omit(name_match)] = var_names$newnames[!is.na(name_match)]
    temp_db
  }
  , .id = "file_name"  # Add the file names as a column using the names from the list col_names
  )
  
  return(df)
}
 
# Take an uneven list of lists and return a even list filling in the lists with NA
# return a tibble

cbind_dif <-function(x = list()){
  # Find max length
  max_length <- max(unlist(lapply(x, length)))
  
  # Set length of each list to maximun and fill expanded members with NA
  res <- lapply(x, function(x){
    x <- c(x, as.list(rep(NA, max_length - length(x) )))
    names(x)<- make.names(c(1:max_length))
    return(x)
  })
  
  return(as_tibble(res))
}

##  get the list of excel files

get_excelfile <- function(x){ 
  # Get the Excel files directory. >>>> NOTE: The dialog window may appear behind RStudio window
  proc_path <- rstudioapi::selectDirectory(caption = "Select Excel files Directory")

  # Get the list of filenames of from directory (recursive=F) without path (full.names=F)
  x <- list.files(proc_path, full.names= T, pattern='*.xls*', recursive=FALSE) %>% 
    setNames(nm= basename(.)) # set names for list
  return(x)
}



#  Run this to get all the column names from the excel files
# Edit the saved csv file to create a mapping of column names.
# Get all the column names for all the files
var_names_all <- function(excel_files, sheetname) {
  map_dfr(file_names, function(x) {
    
    # Get the column names of excel file x
    file_col <-
      if (any(sheetname %in% excel_sheets(x))) {
        as.data.frame(names(read_excel(
          x, sheet = sheetname[sheetname %in% excel_sheets(x)], n_max = 0
        )), stringsAsFactors = FALSE) %>%
          setNames(nm = "col") %>% #give the column a better name
          mutate(source_file = basename(x))
      }
  })  %>%
    distinct(col)
}
return()

# Read in the data from all the Excel files and rename the columns with standard names.

get_all_data <- function(excel_files, sheetname) {
  # Read in the mapped variable file
  var_names <- read_csv(
    rstudioapi::selectFile(caption = "Select variable names csv file", path =
                             dirname(file_names[1])),
    col_names = T
  )
  # Go through each file name and read the excel sheet using the col type to specify or exclude columns
   map_dfr(file_names, function(x) {
    # Get the column names of the current excel file
    file_col <-
      as.data.frame(names(read_excel(
        x, sheet = sheetname, n_max = 0
      )), stringsAsFactors = FALSE) %>% setNames(nm = "col") #give it a better name
    # Match the type to each column name and add a column with matching types and replace any NA with "skip"
    file_col$type <-
      var_names$type[match(file_col$col, var_names$var)] %>% replace_na("skip")
    #Some excel sheets have blank column at the end; so limit to number of named columns
    cno <- c(1, length(file_col$col))
    # Now ready to read the data
    temp_db <- read_excel(
      x,
      sheet = sheetname,
      col_names = T,
      col_types = file_col$type,
      cell_cols(cno)
    )
    # rename columns to standard names using the newname column of the var_names csv file
    name_match = match(var_names$var, names(temp_db))
    names(temp_db)[na.omit(name_match)] = var_names$newnames[!is.na(name_match)]
    temp_db
  }
  , .id = "file_name"  # Add the file names as a column using the names from the list col_names
  )
}

# 1990-2023 MAT89 ---------------------------------------------------------
#  For these years the data logger data was in array base files
# --- --- ---
# Data sheet
sheetname <- "Hourly"
station <- "MAT89_1990-recent_met"
# Get a csv file with the variable names for reading in data from an Excel sheet
# The csv file containing 3 variables: 
#   "var" = column names to be imported from excel
#   "type"= type to import as. Can use "skip" to exclude a column of data.
#   "newnames" = name to use. May be the same or a new name.
# Note that the column names in the csv file need to be quoted since some begin
# with numbers and others include banks
# Get list of all excel files in a directory.

file_names <-get_excelfile()
var_names <- read_csv(rstudioapi::selectFile(caption = "Select variable names csv file",
                                             path =dirname(file_names[1])), col_names=T )
# Go through each file name and read the excel sheet using the col type to specify or exclude columns

df_logger <- read.processedfile(file_names,sheetname,var_names)
  # For fields that were read in as dates change to "Etc/GMT+9"(Alaska standard time) without changing the value.
  # Array datalogger files only have Year,Julian day and hourminute.  Calculate the date and time from the YEAR, 
  # julian day  and hourminute, setting it to Alaska standard time. Note hourminute from logger does not have 
  # leading zeros so need to use sprintf to pad with zero
df_loggert <- df_logger %>% force_tz("Etc/GMT+9") %>%
     # fixed names with janitor library since I had strange results with spaces in names
     clean_names()%>%
      mutate (timestampcal = as.POSIXct(strptime(paste(as.Date(julian,paste(year-1,12,31,sep = "-")),
                         sprintf("%04d",as.integer(hour))),"%Y-%m-%d %H%M"),tz="Etc/GMT+9")) %>%
      mutate(timestamp = if_else(is.na(timestamp),timestampcal,timestamp)) %>%
  # Remove columns not needed or will be recalculated. Note date is an excel calculated variable so removing.
      select(-c(id,year,month, date)) %>% 
       mutate(year_rtm = year(timestamp),month_rtm = month(timestamp)) %>% 
      # re-order and rename columns then sort by timestamp
      select(file_name,timestamp,julian, everything()) %>% 
      arrange(timestamp)  
# timestamps with NA are from excel files with extra rows. For some excel files I did not put NA in all winter precipitation.
df_logger <-  df_loggert %>% subset(!is.na(timestamp)) %>%
      mutate(precipitation = ifelse(month_rtm>9 | month_rtm<5,NA,precipitation)) 

# Calculate full time series
# For MAT89 30 minute averages started on 2014-06-05 6:00
max_date_hour_mat <- ymd_hm("2014-06-05 6:00",tz="Etc/GMT+9")

daterange <- full_join(as_tibble_col(seq.POSIXt(min(df_logger$timestamp),max_date_hour_mat, 
                                            by = 60*60), column_name = "timestampjoin"),
                       as_tibble_col(seq.POSIXt(max_date_hour_mat,max(df_logger$timestamp),
                                            by = 60*30), column_name = "timestampjoin"))

df_logger <- df_logger %>%
  mutate(timestamp = if_else(timestamp < max_date_hour_mat,  # round any times where the logger clock was off
                             round_date(timestamp,unit = "hour"), timestamp)) %>% 
  mutate(timestampjoin =timestamp) %>%
  full_join(.,daterange) %>% 
  mutate(timestamp = timestampjoin) %>%
  select(file_name,timestamp,everything()) %>%
  .[order(.$timestampjoin),] %>%
  select(-c(timestampjoin,julian,hour,timestampcal))

dups <- get_dupes(df_logger,timestamp)

# remove any duplicates
df_logger <- df_logger %>% distinct()
path2save <-str_replace( dirname(file_names[1]), "[^\\/]+\\/?$", "")

write.csv(df_logger,paste0(path2save,station,".csv"),row.names =F)
save(df_logger,file = paste0(path2save,"df_",station,".RData"))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 2018 to 2021 MAT06 Block 1 Met station --------------------------------------------
#  Read in the 2018 to 2021 data.  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Choose the directory with excel files.
# The directory should only contain the files with data to be read in.

file_names <-get_excelfile()
var_names <- read_csv(rstudioapi::selectFile(caption = "Select variable names csv file",
                                             path =dirname(file_names[1])), col_names=T )
sheetname <- "Data"
station <- "MAT06_blk3_soil"

df_logger <- read.processedfile(file_names,sheetname,var_names) %>%
  force_tz("Etc/GMT+9") %>%
  # fixed names with janitor library since I had strange results with spaces in names
  clean_names()%>%
  select(file_name,timestamp, everything())
  #check for rows with all NA since some files may only have met data or soil
cols_to_check <- names(df_logger[5:32])
partial_records <- df_logger %>% filter(if_all(all_of(cols_to_check), ~ is.na(.x)))
  # filter them out.
df_logger <- df_logger %>% filter(if_all(all_of(cols_to_check), ~ !is.na(.x)))
# Calculate full time series
daterange <- as_tibble_col(seq.POSIXt(min(df_logger$timestamp),max(
  df_logger$timestamp), by = 60*30), column_name = "timestampjoin")
#Date range for soil data where averaging time changed
daterange <- full_join(as_tibble_col(seq.POSIXt(min(df_logger$timestamp),
                                    ymd_hm("2019-08-20 04:30",tz="Etc/GMT+9"), 
                                    by = 60*30), column_name = "timestampjoin"),
                       as_tibble_col(seq.POSIXt(ymd_hm("2019-08-20 06:00",tz="Etc/GMT+9"),
                                     max(df_logger$timestamp), by = 60*180),
                                     column_name = "timestampjoin"))
#join the full range of dates
df_logger<- df_logger %>% 
  mutate(timestampjoin =timestamp) %>%
  full_join(.,daterange) %>% 
  select(file_name,timestamp,timestampjoin,everything()) %>%
  .[order(.$timestampjoin),]

dups <- get_dupes(df_logger,timestampjoin)
#Remove duplicates if any
df_logger<- distinct(df_logger, timestampjoin, .keep_all = T)
path2save <-str_replace( dirname(file_names[1]), "[^\\/]+\\/?$", "")

write.csv(df_logger,paste0(path2save,station,".csv"),row.names =F)
save(df_logger,file = paste0(path2save,"df_",station,".RData"))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 2009 to 2020 Toolik Met station ---------------------------------------------------
#  Read in the 2009 to 2020 data downloaded from edc.  The downloaded data does not 
#  include a timestamp variable
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Choose the directory with excel files to get data from and read all the excel file into a list.
# The directory should only contain the files that data will be read from. 
file_names <-get_excelfile()
var_names <- read_csv(rstudioapi::selectFile(caption = "Select variable names csv file",
                                             path =dirname(file_names[1])), col_names=T )
sheetname <- "Hourly"
df_tfsmet_2009to2020 <- read.processedfile(file_names,sheetname,var_names)
df_tfsmet_2009to2020 <- df_tfsmet_2009to2020 %>% force_tz("Etc/GMT+9") %>%
  # fixed names with janitor library since I had strange results with spaces in names
  clean_names()%>%
  # For fields that were read in as dates change to "Etc/GMT+9"(Alaska standard time)
  # without changing the value. Add hour:minute to the date
  mutate (timestamp = as.POSIXct(strptime(paste(as.Date(date),
                                                sprintf("%04d",as.integer(hour))),
                                          "%Y-%m-%d %H%M"),tz="Etc/GMT+9"))%>%
  select(file_name,timestamp,date, everything())

save(df_tfsmet_2009to2020, file = "df_tfsmet_2009to2020.RData")
  
load("df_tfsmet_2009to2020.RData")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#  1989 to 2008 Toolik met data downloaded from excel files. -------- 
#  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Choose the directory with the data logger excel files and read all the excel files into a list.
# Next a csv file with the variable names, types and names to use read in.
file_names <-get_excelfile()
var_names <- read_csv(rstudioapi::selectFile(caption = "Select variable names csv file",
                                             path =dirname(file_names[1])), col_names=T )
sheetname <- "Hourly"
df_tfsmet_1989to2008 <- read.processedfile(file_names,sheetname,var_names)

# For fields that were read in as dates change to "Etc/GMT+9"(Alaska standard time) without changing the value.
  # Calculate the date and time from the date and the hourminute, setting it to Alaska standard time
  # Note hourminute from logger does not have leading zeros so need to use sprintf
df_tfsmet_1989to2008 <- df_tfsmet_1989to2008 %>% force_tz("Etc/GMT+9") %>%
  # fixed names with janior library since I had strange results with spaces in names
  clean_names()%>%
  mutate (timestamp = as.POSIXct(strptime(paste(as.Date(julian,paste(year-1,12,31,sep = "-")),
                                                sprintf("%04d",as.integer(hour))),"%Y-%m-%d %H%M"),tz="Etc/GMT+9")) %>%
  # Remove columns not needed or will be recalculated. Note date is an excel calculated variable so removing.
  select(-c(year,month, date)) %>% 
  mutate(year_rtm = year(timestamp),month_rtm = month(timestamp)) %>% 
  # re-order and rename columns then sort by timestamp
  select(file_name,timestamp,julian, year_rtm, month_rtm, everything()) %>% 
  arrange(timestamp)  
#
save(df_tfsmet_1989to2008, file= "df_tfsmet_1989to2008.RData")
# calculate vapor pressure (mbar)
df_tfsmet_1989to2008 <- df_tfsmet_1989to2008 %>% subset(!is.na(timestamp)) %>%
   mutate (t=air_temperature_5meter) %>%
   mutate(sat_vapor_press = ifelse((t <0), 6.1121*exp(17.966* t / (247.15 + t)), 6.1121*exp(17.368* t / (238.88 + t)))) %>%
   mutate(vapor_pressure_5meter= (rh_5meter/100)* sat_vapor_press) %>%
   mutate_at(vars(pyranometer,air_temperature_1meter,air_temperature_5meter,air_tc_1meter,air_tc_5meter,wind_speed_1meter,wind_speed_5meter),round,2) %>%
   mutate_at(vars(lake_par,rh_1meter,rh_5meter,wind_direction_5meter,par,hobo_logger_3_m_par),round,0)

path2save <-str_replace( dirname(file_names[1]), "[^\\/]+\\/?$", "")

write.csv(df_logger,paste0(path2save,station,".csv"),row.names =F)
save(df_logger,file = paste0(path2save,"df_",station,".RData"))

load("df_tfsmet_1989to2008.RData")
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 1989 to 20020 Toolik TSFMet ---- 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

df_tfsmet_1989to2020 <- merge(df_tfsmet_1989to2008,df_tfsmet_2009to2020, all = T)%>%
  mutate(timestamp = round_date(.$timestamp,unit = "hour"))  # round any times that are offset

# Calculate full time series
daterange <- as_tibble_col(seq.POSIXt(min(df_tfsmet_1989to2020$timestamp),max(
  df_tfsmet_1989to2020$timestamp), by = 60*60), column_name = "timestampjoin")

df_tfsmet_1989to2020 <- df_tfsmet_1989to2020 %>% 
  mutate(timestampjoin =timestamp) %>%
  full_join(.,daterange) %>%  
  mutate(timestamp = timestampjoin) %>%
  select(file_name,timestamp,everything()) %>%
  .[order(.$timestamp),] %>%
  subset(!is.na(timestamp))

dups <- get_dupes(df_tfsmet_1989to2020,timestamp)

path2save <-str_replace( dirname(file_names[1]), "[^\\/]+\\/?$", "")

write.csv(df_logger,paste0(path2save,station,".csv"),row.names =F)
save(df_logger,file = paste0(path2save,"df_",station,".RData"))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# WetSedge89 hourly ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Choose the directory with the data logger excel files and read all the excel files 
# into a list. Then  a csv file with the variable names, types and names to use.
station <- "WetSedge89-1994_recent"
file_names <-get_excelfile()
var_names <- read_csv(rstudioapi::selectFile(caption = "Select variable names csv file",
                                             path =dirname(file_names[1])), col_names=T )
sheetname <- "Hourly"
df_logger <- read.processedfile(file_names,sheetname,var_names)

# Note hourminute from logger does not have leading zeros so need to use sprintf
df_logger <- df_logger %>% force_tz("Etc/GMT+9") %>%
  # fixed names with janior library since I had strange results with spaces in names
  clean_names()%>%
  mutate (timestampcal = as.POSIXct(strptime(paste(as.Date(julian,paste(year-1,12,31,sep = "-")),
                                     sprintf("%04d",as.integer(hour))),"%Y-%m-%d %H%M"),tz="Etc/GMT+9")) %>%
  mutate(timestamp = if_else(is.na(timestamp),timestampcal,timestamp)) %>%
   # Remove columns not needed or will be recalculated. Note date is an excel calculated variable so removing.
  select(-c(year,month, date)) %>% 
  mutate(year_rtm = year(timestamp),month_rtm = month(timestamp)) %>% 
  # re-order and rename columns then sort by timestamp
  select(file_name,timestamp,julian, year_rtm, month_rtm, everything()) %>% 
  arrange(timestamp) 
df_logger <- df_logger %>% 
                 subset(!is.na(timestamp)) %>%
                 select(-timestampcal)
# Calculate full time series
daterange <- as_tibble_col(seq.POSIXt(min(df_logger$timestamp),max(
  df_logger$timestamp), by = 60*60), column_name = "timestampjoin")

df_logger <- df_logger %>% 
  mutate(timestampjoin =timestamp) %>%
  full_join(.,daterange) %>% 
  select(file_name,timestamp,timestampjoin,everything()) %>%
  .[order(.$timestampjoin),]
dups <- get_dupes(df_logger,timestampjoin)
# remove any duplicates
df_logger <- distinct(df_logger, timestampjoin, .keep_all = T) %>% subset(!is.na(timestampjoin))

path2save <-str_replace( dirname(file_names[1]), "[^\\/]+\\/?$", "")

write.csv(df_logger,paste0(path2save,station,".csv"),row.names =F)
save(df_logger,file = paste0(path2save,"df_",station,".RData"))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# MNT97 Met data ----
#     
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  

#  For these years the data logger data was in array base files
# --- --- ---
# Data sheet
sheetname <- "Hourly"

# Get a csv file with the variable names for reading in data from an Excel sheet
# The csv file containing 3 variables: 
#   "var" = column names to be imported from excel
#   "type"= type to import as. Can use "skip" to exclude a column of data.
#   "newnames" = name to use. May be the same or a new name.
# Note that the column names in the csv file need to be quoted since some begin
# with numbers and others include banks
# Get list of all excel files in a directory.
file_names <-get_excelfile()
var_names <- read_csv(rstudioapi::selectFile(caption = "Select variable names csv file",
                                             path =dirname(file_names[1])), col_names=T )
# Go through each file name and read the excel sheet using the col type to specify or exclude columns
df_MNT1999to2020 <- map_dfr(file_names, function(x) {
  # Get the column names of the current excel file
  file_col <-
    as.data.frame(names(read_excel(x, sheet = sheetname, n_max = 0)),
                  stringsAsFactors = FALSE) %>% setNames(nm = "col") #give it a better name
  # Match the type to each column name and add a column with matching types and replace any NA with "skip"
  file_col$type <-
    var_names$type[match(file_col$col, var_names$var)] %>% replace_na("skip")
  #Some excel sheets have blank column at the end; so limit to number of named columns
  cno <- c(1, length(file_col$col))
  # Now ready to read the data
  temp_db <- read_excel(
    x,
    sheet = sheetname,
    col_names = T,
    col_types = file_col$type,
    cell_cols(cno)
  )
  # rename columns to standard names using the newname column of the var_names csv file
  name_match = match(var_names$var, names(temp_db))
  names(temp_db)[na.omit(name_match)] = var_names$newnames[!is.na(name_match)]
  temp_db
}
, .id = "file_name"  # Add the filenames as a column using the names from the list col_names
)

# For fields that were read in as dates change to "Etc/GMT+9"(Alaska standard time) without changing the value.
# Array datalogger files only have Year,Julian day and hourminute.  Calculate the date and time from the YEAR, 
# julian day  and hourminute, setting it to Alaska standard time. Note hourminute from logger does not have 
# leading zeros so need to use sprintf to pad with zero
df_MNT1999to2020 <- df_MNT1999to2020 %>% force_tz("Etc/GMT+9") %>%
  # fixed names with janitor library since I had strange results with spaces in names
  clean_names()%>%
  mutate (timestamp = as.POSIXct(strptime(paste(as.Date(julian,paste(year-1,12,31,sep = "-")),
                                                sprintf("%04d",as.integer(hour))),"%Y-%m-%d %H%M"),tz="Etc/GMT+9")) %>%
  # Remove columns not needed or will be recalculated. Note date is an excel calculated variable so removing.
  select(-c(id,year,month, date)) %>% 
  mutate(year_rtm = year(timestamp),month_rtm = month(timestamp)) %>% 
  # re-order and rename columns then sort by timestamp
  select(file_name,timestamp,julian, everything()) %>% 
  arrange(timestamp)  
# NA are from excel files with extra rows and some excel I did not put NA in all winter precipitation.
df_MNT1999to2020 <-  df_MNT1999to2020 %>% subset(!is.na(timestamp))
write.csv(df_MNT1999to2020,"MNT1999to2020.csv",row.names =F)
save(df_MNT1999to2020,file = "df_MNT1999to2020.RData")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# DHT89 Met data ----
#     
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  

#  For these years the data logger data was in array base files
# --- --- ---
# Data sheet
sheetname <- "Hourly"
sheetname <- c("SoilsII","3hourly")

# Get a csv file with the variable names for reading in data from an Excel sheet
# The csv file containing 3 variables: 
#   "var" = column names to be imported from excel
#   "type"= type to import as. Can use "skip" to exclude a column of data.
#   "newnames" = name to use. May be the same or a new name.
# Note that the column names in the csv file need to be quoted since some begin
# with numbers and others include banks
# Get list of all excel files in a directory.
file_names <-get_excelfile()



## Read in the mapped variable file ----
var_names <- read_csv(rstudioapi::selectFile(caption = "Select variable names csv file",
                                             path =dirname(file_names[1])), col_names=T )
# Go through each file name and read the excel sheet using the col type to specify or exclude columns
df_DHT89_2001_2018 <- map_dfr(file_names, function(x) {
  # Get the column names of the current excel file
  file_col <-
    as.data.frame(names(read_excel(x, sheet = sheetname, n_max = 0)),
                  stringsAsFactors = FALSE) %>% setNames(nm = "col") #give it a better name
  # Match the type to each column name and add a column with matching types and replace any NA with "skip"
  file_col$type <-
    var_names$type[match(file_col$col, var_names$var)] %>% replace_na("skip")
  #Some excel sheets have blank column at the end; so limit to number of named columns
  cno <- c(1, length(file_col$col))
  # Now ready to read the data
  temp_db <- read_excel(
    x,
    sheet = sheetname,
    col_names = T,
    col_types = file_col$type,
    cell_cols(cno)
  )
  # rename columns to standard names using the newname column of the var_names csv file
  name_match = match(var_names$var, names(temp_db))
  names(temp_db)[na.omit(name_match)] = var_names$newnames[!is.na(name_match)]
    temp_db
}
, .id = "file_name"  # Add the filenames as a column using the names from the list col_names
)

df_DHT89_2001_2018 <- df_DHT89_2001_2018 %>% force_tz("Etc/GMT+9") %>%
  # fixed names with janitor library since I had strange results with spaces in names
  #clean_names()%>%
  mutate (timestamp = as.POSIXct(strptime(paste(as.Date(Julian,paste(Year-1,12,31,sep = "-")),
                                                sprintf("%04d",as.integer(HourMinute))),"%Y-%m-%d %H%M"),tz="Etc/GMT+9")) %>%
  # Remove columns not needed or will be recalculated. Note date is an excel calculated variable so removing.
  select(-c(ID,Year,Month, Date)) %>% 
  mutate(Year_rtm = year(timestamp),Month_rtm = month(timestamp)) %>% 
  # re-order and rename columns then sort by timestamp
  select(file_name,timestamp,Julian, HourMinute, Year_rtm, Month_rtm, contains("air"), RH_3M_CS500, Pyranometer, everything()) %>% 
  arrange(timestamp)  
# NA are from excel files with extra rows 
df_DHT89_2001_2018 <- df_DHT89_2001_2018 %>% 
  subset(!is.na(timestamp))
# Save files
write.csv(df_DHT89_2001_2018, file = paste0(dirname(file_names[1]),"/df_DHT89_2001_2018.csv"), row.names = FALSE)
write_rds(df_DHT89_2001_2018,file = paste0(dirname(file_names[1]),"/df_DHT89_2001_2018.rds"))
  
df_DH89_Met<- df_DHT89_2001_2018 %>% select(timestamp,Air_Temperature_3M_Corrected_107,Air_Temperature_3M_CS500,
                                            RH_3M_CS500,Pyranometer, Comments)
# Read in newer table base data
dx<-`df_DTH89_2019-01-01_2024-06-13` %>% select(timestamp,Air_Temperature_3M_Corrected_107=Air107cor_3M,
                                                Air_Temperature_3M_CS500=CS500_Air_3M,RH_3M_CS500=CS500_RH_3M,Pyranometer)
df_DHT89_Met <- df_DHT89_Met %>% full_join(dx)
# Check for duplicate timestamps. If any, review them.
dups <- get_dupes(df_DHT89_Met, timestamp)
view(dups)
df_DHT89_Met <- df_DHT89_Met %>% 
  group_by(timestamp) %>%
  fill(everything(), .direction = "downup") %>%
  slice(1)
write.csv(df_DHT89_Met, file = paste0(dirname(file_names[1]),"/df_DHT89_Met.csv"), row.names = FALSE)
write_rds(df_DHT89_Met,file = paste0(dirname(file_names[1]),"/df_DHT89_Met.rds"))
#*************************************************************************  
  ## error checks ----
#************************************************************************
  
df_tfsmet_1989to2008_with_hobo <- df_tfsmet_1989to2008 %>% subset(!is.na(`date_hobo_logger`))
  #t$timediff <- t$timestamp-round_date(t$`date_hobo_logger`,unit="minute")
  #t_timediff <- t[t$timediff!=0,]
  write.csv(df_tfsmet_1989to2008_with_hobo, file ="df_tfsmet_1989to2008_with_hobo.csv",row.names =False)
  
 
  
  
  n_hours <- t %>% group_by(year_rtm, month(`date_stowaway`),day(`date_stowaway`), add=T) %>% summarise(n())
  write.csv(n_hours, file= "stowaway hour count.csv")
 
  t<- df_tfsmet_2009to2020
  t<- df_tfsmet_1989to2008
  t <- df_tfsmet_1989to2020
  t<- df_MAT1990to2018
  t<- df_WSG89_1994to2020
  t<- df_MNT1999to2020
  
  t<- t %>% subset(!is.na(timestamp))
  # t$timediff <- t$timestamp-t$date
  # t_timediff <- t[t$timediff!=0,]
  # write.csv(t_timediff,file="t_timecheck.csv")
  # 
  daterange<- as.data.frame(seq.POSIXt(min(t$timestamp),max(t$timestamp), by = 60*60))
  
  # For MAT89 30 miunte averages started on 2014-06-05 10:00
      max_date_hour_mat <- ymd_hm("2014-06-05 10:00",tz="Etc/GMT+9")
      daterange<- as.data.frame(seq.POSIXt(min(t$timestamp),max_date_hour_mat, by = 60*60))
      names(daterange)[1] <- "timestampjoin"
      daterange2 <- as.data.frame(seq.POSIXt(max_date_hour_mat,max(t$timestamp), by = 60*30))
      names(daterange2)[1] <- "timestampjoin"
      daterange <- full_join(daterange,daterange2) 
      
  
  names(daterange)[1] <- "timestampjoin"
  
   #t<- t %>% mutate(timestampjoin = round_date(timestamp,"hour"))
  # missing_datetime <- anti_join(daterange,t, by="timestampjoin")
  t<- t %>% mutate(timestampjoin =timestamp)
  
  t_full <- full_join(t,daterange) %>% 
    select(file_name,timestamp,timestampjoin,everything()) %>%
    .[order(.$timestampjoin),]
  
  dups <- get_dupes(t_full,timestampjoin)
  # remove duplicates
  t_full <- t_full %>% distinct()
  
  # plot 
  x_axis <- "timestampjoin"
  y_axis <- "ct_air_temp"
  p1 <- ggplot(t_full) +
    geom_line(aes(x=timestampjoin, y=ct_air_temp, 
                  color = "Air Temperature")) +
 
  #geom_line(aes(x=timestamp, y=hobo_logger_3_m_air, color = "Hobo 3M Air")) +
  scale_x_datetime()+
    coord_cartesian(ylim = c(-40,40)) +
    labs(title ="Air Temperatures",
         x = "Date",
         y = "celsius ",
         color = '')+
    theme_bw() 
  p1
  
   ##Select just temp, rh ----
  df_WSG89_1994to2020_temp_rh <- df_WSG89_1994to2020 %>% 
    select(timestamp = timestampjoin,WSG89_ct_temp_3m = ct_temp_3m,
           WSG89_ct_air107_3m = ct_air107_3m,WSG89_ct_rh = ct_rh_3m)
  write.csv(df_WSG89_1994to2020_temp_rh,file = "WSG89_1994to2020_temp_rh.csv", row.names = F)
  
  df_tfsmet_1989to2020_temp_rh <-df_tfsmet_1989to2020 %>% 
    select(timestamp,air_temperature_1meter, air_temperature_3meter,
           air_temperature_5meter,rh_1meter, rh_3meter,
           rh_5meter,air_tc_1meter,air_tc_5meter,hobo_logger_3_m_air, 
           hobo_logger_3_m_rh, air_stowaway_1meter, precipitation) %>%
    mutate(timestamp = round_date(.$timestamp,unit = "hour")) %>%
    group_by(timestamp) %>%
    summarise(across(air_temperature_1meter:air_stowaway_1meter, ~ mean(.x, na.rm = F)),
              precipitation = sum(precipitation))%>%
    ungroup()
  save(df_tfsmet_1989to2020_temp_rh,file = "df_tfsmet_1989to2020_temp_rh.RData")
  
  df_logger_temp_rh <- df_logger %>% 
    mutate(timestamp = round_date(.$timestamp,unit = "hour")) %>%
    select(timestamp,MAT89_ctemp_3m = ctemp_3m,MAT89_stemp =stemp,
           MAT89_crh_3m = crh_3m,MAT89_srh = srh, MAT89_precipitation = precipitation)%>%
    group_by(timestamp) %>%
    summarise(across(MAT89_ctemp_3m:MAT89_srh,~ mean(.x, na.rm = F)),
              MAT89_precipitation = sum(MAT89_precipitation))%>%
    ungroup()
  
  save(df_logger_temp_rh,file = "df_MAT1990to2018_temp_rh.RData")
  
  write.csv(df_logger_temp_rh,file = "MAT1990to2018_temp_rh.csv",row.names = F)
  
  # Join the temperature and RH data frames
  df_tfsmet_1989to2020_WSG89_MAT89_temp_rh <- full_join(df_tfsmet_1989to2020_temp_rh,
                                                  df_WSG89_1994to2020_temp_rh, by = "timestamp") %>%
    full_join(.,df_logger_temp_rh, by= "timestamp") %>%
    .[order(.$timestamp),]
  
  
  save(df_tfsmet_1989to2020_WSG89_MAT89_temp_rh,file = "df_tfsmet_1989to2020_WSG89_MAT89_temp_rh.RData")
  write.csv(df_tfsmet_1989to2020_WSG89_MAT89_temp_rh, file ="df_tfsmet_1989to2020_WSG89_MAT89_temp_rh.csv")
  
 
  # find missing temps
  t_missing_temps <- t_full %>% subset(is.na(air_temperature_5meter))
  write.csv(t_full, file =" tfsmet_missing_temp.csv")
  t_missing_rh <- t_full %>% subset(is.na(rh_5meter))
  write.csv(t_full, file =" tfsmet_missing_rh.csv")
  
  # getting RH averages
  t1 <- df_tfsmet_1989to2008 %>% mutate(day_rtm =day(timestamp))
 
  
  #Compute average and standard error of mean
  t1avg<- df_tfsmet_1989to2008 %>% 
    group_by(year_rtm,month_rtm, julian) %>% summarise(rh5m.avg= 100*(mean(vapor_pressure_5meter)/ mean(sat_vapor_press,na.rm=T)),
                                       airtemp5m.avg=mean(air_temperature_5meter,na.rm=T),
                                       air.no =sum(!is.na(air_temperature_5meter)),
                                       airtemp5m.max=max(air_temperature_5meter,na.rm = T),
                                       airtemp5m.min=min(air_temperature_5meter,na.rm = T)) 
  
  t1avg <-t1avg %>%  mutate_at(vars(rh5m.avg),round) %>% mutate_at(vars(airtemp5m.avg,airtemp5m.max,airtemp5m.min),round,2) 
 
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Waterplots Met data ----
  #     
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  
  #  For these years the data logger data was in array base files
  # --- --- ---
  # Data sheet
   sheetname <- c("SoilTemp")
  
  ## Get list of all excel files in a directory.----
  file_names <-get_excelfile()
  
  # Get a csv file with the variable names for reading in data from an Excel sheet
  # The csv file containing 3 variables: 
  #   "var" = column names to be imported from excel
  #   "type"= type to import as. Can use "skip" to exclude a column of data.
  #   "newnames" = name to use. May be the same or a new name.
  # Note that the column names in the csv file need to be quoted since some begin
  # with numbers and others include banks

  
  ##  Step one Get all the column names for all the files ----
  all_columns_names <- var_names_all(file_names,sheetname)
  # save the file for editing
  write_csv(all_columns_names,file = paste0(dirname(file_names[1]),"/waterplot_soiltemp_columns.csv"))
  
  ## Read in the mapped variable file ----
  var_names <- read_csv(rstudioapi::selectFile(caption = "Select variable names csv file",
                                               path =dirname(file_names[1])), col_names=T, show_col_types = FALSE )
  ## Get all the data and rename the columns ----
  df_logger_WP <- read.processedfile(file_names,sheetname,var_names) %>%
    force_tz("Etc/GMT+9") %>%
    mutate (timestamp = as.POSIXct(strptime(paste(as.Date(Julian,paste(Year-1,12,31,sep = "-")),
                                                  sprintf("%04d",as.integer(HourMinute))),"%Y-%m-%d %H%M"),
                                   tz="Etc/GMT+9")) %>%
    # Remove columns not needed or will be recalculated. Note date is an excel calculated variable so removing.
    select(-c(ID,Year,Month, Date)) %>% 
    mutate(Year_rtm = year(timestamp),Month_rtm = month(timestamp)) %>% 
    # re-order and rename columns then sort by timestamp
    select(file_name,timestamp,Julian, HourMinute, Year_rtm, Month_rtm,everything()) %>% 
    arrange(timestamp) %>%  
  # NA are from excel files with extra rows 
    subset(!is.na(timestamp))
  
  ## Save files----
  station <- "WaterPlots1998"
  range_data <- paste0(date(min(df_logger_WP$timestamp)),"_",date(max(df_logger_WP$timestamp)))
  write_rds(df_logger_WP, file = paste0(dirname(file_names[1]),"/", station, "_", range_data,".rds"))
  write.csv(df_logger_WP,
            file = paste0(dirname(file_names[1]),"/",station, "_", range_data,".csv"),
            row.names = F)

#------EXTRA Code--------
  df_2004<- df_1989to2008[df_1989to2008$file_name == "2004dltl.xlsx"]
t1<- subset(t, select = -YEAR)
  sheetslist <- map(file_names, function (x) excel_sheets(x))
  
  df_1989to2008 <- map_df(file_names,function(x){
    header_names <- names(read_excel(x,sheet = "Hourly", n_max = 0))
    
    read_excel(x,sheet = "Hourly", col_names = T,  col_types="text")})
  llNms <- unique(unlist(lapply(header_lines, names)))
  path<- file_names
 #------------------------------------------------------- 
  #REad all the header lines from excel files sheet
  header_lines <- map_dfr(file_names,function(x){read_excel(x,sheet = "Hourly", col_names = T, n_max=1, col_types="text")
    }, .id = "file_name")  # Add the filenames as a column using the names from the list col_names
 #------------------------------------------------------- 
  
  
  test <- select(df_1989to2008[[1]], DATE, HOUR, RAIN,QUANTUM)
  
  sheetn <-  path %>% map(function (x) excel_sheets(x) %>% 
                            str_subset("^Hourly$"))
  hourly_sheet <- path %>% map_df(function(x) {read_excel(x,sheet = "Hourly", col_names = T, n_max=1, col_types="text")})
  
  which(var_names %in% names(read_excel(file_names[1],sheet = "Hourly", n_max = 0)))
