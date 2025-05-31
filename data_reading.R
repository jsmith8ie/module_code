#read in libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(Hmisc)
library(tools)
library(flux)
library(pracma)
library(readr)
library(readtext)

#set wd

#read in data from txt files
##reading in only mouse and control data for anions - data of interest
#read in data
file_list <- list.files(pattern = "\\.txt$")
data_list <- list()

# Loop through each file, process
for (file in file_list) {
  
  # Read read lines of file
  lines <- readLines(file)
  
  # Extract metadata from specific lines in each file and text title
  read <- readtext(file)
  title <- read$doc_id
  pattern <- "(\\d{1,2})[.#](\\d{1,2})[.#](\\d{2})" #two patterns for date in title
  match <- regexpr(pattern, title, perl=TRUE) #match file name to pattern
  date <- NA
  if(match!=-1){extract <- regmatches(title, match)
  if(grepl("#", extract)) {
    tidy_extract <- gsub("#", ".", extract)
  } else {
    tidy_extract <- extract
  }
  }
  #extract date pattern if matches
  date <- format(as.Date(tidy_extract,format ="%d.%m.%y"), "%Y-%m-%d") 
  
  #function to extract date from text file titles
  ident <- lines[2]              # identification - mouse etc. 
  ion <- lines[5]                      # Anion or cation
  
  # Read data from line 8 onwards
  data <- read.table(file, sep=";", header=FALSE, skip=7, fill=TRUE)
  
  # Convert the data to a character vector to search for the word 'Pressure'
  data_lines <- apply(data, 1, paste, collapse = " ")  # Combine rows into a single string
  
  # Find the index of the first row that contains the word 'Pressure'
  pressure_index <- which(grepl("Pressure", data_lines, ignore.case = TRUE))
  
  # If 'Pressure' is found, only keep the data before that row
  if (!is.na(pressure_index)) {
    data <- data[1:(pressure_index - 3), ]
  } else {
    data <- data
  }
  
  # Now, 'data' contains all the rows up until the word 'Pressure'
  
  #column names
  colnames(data) <- c("Minutes", "Conductivity")
  
  # Add metadata columns to data
  data$Date <- date
  data$Ident <- ident
  data$Ion <- ion
  
  # Store data in list
  data_list[[length(data_list) + 1]] <- data
}

# Combine all files into single data frame
data <- do.call(rbind, data_list)


#read in other data (primarily interested in standards)
#reading in standards etc

file_list <- list.files(pattern = "\\.txt$")
data_list <- list()

# Loop through each file, process
for (file in file_list) {
  
  # Read read lines of file
  lines <- readLines(file)
  
  #extract date
  dateline <- lines[1]
  datepattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  match <- regmatches(dateline, regexpr(datepattern, dateline))
  
  #metadata
  date <- if (length(match) > 0) match else NA  #date
  ident <- lines[2]              # identification - mouse etc. 
  ion <- lines[5]                      # Anion or cation
  
  # Read data from line 8 onwards
  std_data <- read.table(file, sep=";", header=FALSE, skip=7, fill=TRUE)
  
  # Convert the data to a character vector to search for the word 'Pressure'
  data_lines <- apply(std_data, 1, paste, collapse = " ")  # Combine rows into a single string
  
  # Find the index of the first row that contains the word 'Pressure'
  pressure_index <- which(grepl("Pressure", data_lines, ignore.case = TRUE))
  
  # If 'Pressure' is found, only keep the data before that row
  if (!is.na(pressure_index)) {
    std_data <- std_data[1:(pressure_index - 3), ]
  } else {
    std_data <- std_data
  }
  
  # Now, 'data' contains all the rows up until the word 'Pressure'
  
  #column names
  colnames(std_data) <- c("Minutes", "Conductivity")
  
  # Add metadata columns to data
  std_data$Date <- date
  std_data$Ident <- ident
  std_data$Ion <- ion
  
  # Store data in list
  data_list[[length(data_list) + 1]] <- std_data
}

# Combine all files into single data frame
std_data <- do.call(rbind, data_list) 
