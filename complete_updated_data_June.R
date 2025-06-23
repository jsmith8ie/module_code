#read in libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(tools)
library(flux)
library(pracma)
library(readr)
library(readtext)

#set wd NOTE different WD for test data vs standard. Two different txt file folders

#read in data from txt files
##reading in only mouse and control data for anions - data of interest
#read in data

setwd("C:\\Users\\jessi\\OneDrive\\Desktop\\Project\\Data\\text_files_anions_cations")
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
#CHANGE WD

setwd("C:\\Users\\jessi\\OneDrive\\Desktop\\Project\\Data\\text_files_standards_etc")
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


#data tidying
#tidy anion data
head(data)

#investigate class of variables, ensure correct
#no consistency between standards' formatting
#need to make consistent
#make copy of data to work with
data2 <- c()
data2 <- data
list(unique(data2$Ident))


#consistency in capital letters - all capitalised
data2$Ident <- stringr:: str_to_title((data2$Ident))

#underscore instead of space
data2$Ident <- gsub(" ", "_", data2$Ident)

#fix mouse1 vs mouse_1

data2$Ident <- str_replace_all(data2$Ident, c("Mouse1" = "Mouse_1",
                                              "Mouse2" = "Mouse_2",
                                              "Mouse3" = "Mouse_3",
                                              "Mouse4" = "Mouse_4",
                                              "Mouse5" = "Mouse_5",
                                              "Mouse6" = "Mouse_6",
                                              "Control2" = "Control_2",
                                              "Control1" = "Control_1",
                                              "Control3" = "Control_3"))               
#when happy with changes
#copy over to dataset
data <- data2


##############################################################################
#check that the data is in correct variable types
#Minutes
class(data$Minutes) #character, want it as numeric
data$Minutes <- as.numeric(data$Minutes)

#Conductivity
is.numeric(data$Conductivity) # false, so change
data$Conductivity <- as.numeric(data$Conductivity)


#Date
class(data$Date)
data$Date <- as.Date(data$Date)

#Day
data2<- data
#form new column, where dates recoded to day 1 etc
#set start date as day before to get correct days in df
#this will change when correct dates implemented
#start date was 24 November 2021
startdate <- as.Date("2021-11-24", "%Y-%m-%d")


unique(data2$Date)
#two spurious dates "2020-02-17" "2021-11-17"
#do not include in analysis, happens well before analysis

#filter out days before start date
data2 <- data2 %>% dplyr::filter(data2$Date >= startdate)
data2$Day <- difftime(data2$Date, startdate, unit="days")

class(data2$Day)
#change to numeric
data2$Day<- as.numeric(data2$Day)

#copy over to data
data <- data2
unique(data$Day)



#subset data - concerned primarily with mice data (not control)
#Select anion samples only
#anions#
anions <- data %>% dplyr::filter(data$Ion=="Anions")
#remove ion type (redundant as anion only dataset)
anions <- anions %>% select(-Ion)


#check anion Idents

unique(anions$Ident)
############################################################################
#controls - ANIONS
control_data <- anions %>% dplyr::filter(grepl("Control", anions$Ident))
#check
unique(control_data$Ident)
unique(control_data$Date)
#############################################################################
#Mice - ANIONS
mice_data <- anions %>% dplyr::filter(grepl("Mouse", anions$Ident))
#check
unique(mice_data$Ident)
unique(mice_data$Date)
unique(mice_data$Day)

#handle multiple runs in days 0, 1, 2 (find avg values for each)
mice_data2 <- mice_data

mice_data2 <- mice_data2 %>% group_by(Ident, Day, Minutes) %>%
  summarise(Conductivity = mean(Conductivity), .groups='drop')

unique(mice_data2$Ident)

#copy to mice data
mice_data <- mice_data2

#####################################################################
#tidy std data
std_data

#tidy standard data - remove equilibration
std_data <- std_data[std_data$Ident!="Equilibration - 40 mins",] 
std_data <- std_data[std_data$Ident!= "Equilibration - 10 mins",]
#remove blanks
std_data <- std_data[std_data$Ident!= "Blank",]

#tidy - idents
data2 <- c()
data2 <- std_data

list(unique(data2$Ident))
#fix 'strandard' typo - this is important
data2$Ident <- str_replace(data2$Ident, "Strandard", "Standard")
#consistency in capital letters - all capitalised
data2$Ident <- stringr:: str_to_title((data2$Ident))






data2$Ident <- str_replace_all(data2$Ident, c("Nitrite Standard 0.1"="Nitrite_Std_4",
                                              "Nitrite Standard 0.075" = "Nitrite_Std_3",
                                              "Nitrite Standard 0.05"="Nitrite_Std_2",
                                              "Nitrite Standard 0.025"="Nitrite_Std_1"))


data2$Ident <- str_replace_all(data2$Ident, c("Anion Standard 3.75ml" = "Anion_Std_3",
                                              "Anion Standard 2.5ml" = "Anion_Std_2",
                                              "Anion Standard 1.25ml" = "Anion_Std_1",
                                              "Standard 0.1" = "Anion_Std_4",
                                              "Standard 0.075" = "Anion_Std_3",
                                              "Standard 0.05" = "Anion_Std_2",
                                              "Standard 0.025" = "Anion_Std_1"))


data2$Ident <- str_replace_all(data2$Ident, c("Anion Standard" = "Anion_Std_4"))
data2$Ident <- str_replace_all(data2$Ident, c("Standard" = "Anion_Std_4"))

#choose only standards
data2 <- data2[grepl("_Std_",data2$Ident),]

#check variable types
class(data2$Minutes)
data2$Minutes <- as.numeric(data2$Minutes)

class(data2$Conductivity)
data2$Conductivity <- as.numeric(data2$Conductivity)

class(data2$Date)
data2$Date <- as.Date(data2$Date)


#when happy with changes
#copy over to dataset
std_data <- data2

#subset by std type
std1 <- std_data %>% dplyr::filter(std_data$Ident == "Anion_Std_1")
std2 <- std_data %>% dplyr::filter(std_data$Ident == "Anion_Std_2")
std3 <- std_data %>% dplyr::filter(std_data$Ident == "Anion_Std_3")
std4 <- std_data %>% dplyr::filter(std_data$Ident == "Anion_Std_4")

nitrite1 <- std_data %>% dplyr::filter(std_data$Ident == "Nitrite_Std_1")
nitrite2 <- std_data %>% dplyr::filter(std_data$Ident == "Nitrite_Std_2")
nitrite3 <- std_data %>% dplyr::filter(std_data$Ident == "Nitrite_Std_3")
nitrite4 <- std_data %>% dplyr::filter(std_data$Ident == "Nitrite_Std_4")


#bind all together
std <- rbind(std1, std2, std3, std4, nitrite1, nitrite2, nitrite3, nitrite4)

#data exploration
#mice data
#important variables for for loops/functions
mice <- sort(unique(mice_data$Ident)) 
days <- sort(unique(mice_data$Day)) 


#separate datasets per mouse
m1 <- mice_data %>% dplyr::filter(Ident=="Mouse_1")
m2 <- mice_data %>% dplyr::filter(Ident=="Mouse_2")
m3 <- mice_data %>% dplyr::filter(Ident=="Mouse_3")
m4 <- mice_data %>% dplyr::filter(Ident=="Mouse_4")
m5 <- mice_data %>% dplyr::filter(Ident=="Mouse_5")
m6 <- mice_data %>% dplyr::filter(Ident=="Mouse_6")


#some checks
unique(m4$Day)
unique(m5$Day)
unique(m6$Day)

############################################################################
#plots 
p1 <- m1 %>% ggplot(aes(x=Minutes, y=Conductivity)) + geom_line() +
  labs(title="Mouse 1") +
  facet_wrap(~Day) +
  theme_bw()

p2 <- m2 %>% ggplot(aes(x=Minutes, y=Conductivity)) + geom_line() +
  labs(title="Mouse 2") +
  facet_wrap(~Day) +
  theme_bw()

p3 <- m3 %>% ggplot(aes(x=Minutes, y=Conductivity)) + geom_line() +
  labs(title="Mouse 3") +
  facet_wrap(~Day)  +
  theme_bw()


p4 <- m4 %>% ggplot(aes(x=Minutes, y=Conductivity)) + geom_line() +
  labs(title="Mouse 4") +
  facet_wrap(~Day) +
  theme_bw()

p5 <- m5 %>% ggplot(aes(x=Minutes, y=Conductivity)) + geom_line() +
  labs(title="Mouse 5") +
  facet_wrap(~Day) +
  theme_bw()

p6 <- m6 %>% ggplot(aes(x=Minutes, y=Conductivity)) + geom_line() +
  labs(title="Mouse 6") +
  facet_wrap(~Day) +
  theme_bw()

p1
p2
p3
p4
p5
p6

#compare peaks per day, for each mouse
#days variable
days

for (day in days) {
  data <- mice_data %>% filter(Day==day)
  
  p <- data %>% ggplot(aes(x=Minutes, y = Conductivity)) + 
    geom_line() + facet_wrap(~Ident) + labs(title = paste("Day", day))
  
  print(p)
}


#standards
#examine standard by date

testplot <- nitrite4 %>%
  ggplot(aes(x=Minutes, y=Conductivity))+
  geom_line() + 
  labs(title = "Nitrite Standard 4",
       x = "Time (Minutes)",
       y = "Conductivity (μSiemens)") +
  scale_x_continuous(limits = c(0, NA), breaks= seq(from=0, to=22, by=2)) +
  theme_bw() +
  facet_wrap(~Date)



#select one date only to obtain one standard run (2022-02-11 only date with accurate looking peaks
#and only date nitrite was run)
std_data <- std_data %>% dplyr::filter(Date=="2022-02-11")
#select relevant columns
std_data <- std_data[c("Minutes", "Conductivity", "Ident")]

#re-subset with date selected:
#subset by std type
std1 <- std_data %>% dplyr::filter(std_data$Ident == "Anion_Std_1")
std2 <- std_data %>% dplyr::filter(std_data$Ident == "Anion_Std_2")
std3 <- std_data %>% dplyr::filter(std_data$Ident == "Anion_Std_3")
std4 <- std_data %>% dplyr::filter(std_data$Ident == "Anion_Std_4")

nitrite1 <- std_data %>% dplyr::filter(std_data$Ident == "Nitrite_Std_1")
nitrite2 <- std_data %>% dplyr::filter(std_data$Ident == "Nitrite_Std_2")
nitrite3 <- std_data %>% dplyr::filter(std_data$Ident == "Nitrite_Std_3")
nitrite4 <- std_data %>% dplyr::filter(std_data$Ident == "Nitrite_Std_4")



#ggplot
pstd1 <- std1 %>%
  ggplot(aes(x=Minutes, y=Conductivity))+
  geom_line() + 
  labs(title = "Anion Standard 1",
       x = "Time (Minutes)",
       y = "Conductivity (μSiemens)") +
  scale_x_continuous(limits = c(0, NA), breaks= seq(from=0, to=22, by=2)) +
  theme_bw()

pstd2 <- std2 %>%
  ggplot(aes(x=Minutes, y=Conductivity))+
  geom_line() + 
  labs(title = "Anion Standard 2",
       x = "Time (Minutes)",
       y = "Conductivity (μSiemens)") +
  scale_x_continuous(limits = c(0, NA), breaks= seq(from=0, to=22, by=2)) +
  theme_bw()

pstd3 <- std3 %>%
  ggplot(aes(x=Minutes, y=Conductivity))+
  geom_line() + 
  labs(title = "Anion Standard 3",
       x = "Time (Minutes)",
       y = "Conductivity (μSiemens)") +
  scale_x_continuous(limits = c(0, NA), breaks= seq(from=0, to=22, by=2)) +
  theme_bw()


pstd4 <- std4 %>%
  ggplot(aes(x=Minutes, y=Conductivity))+
  geom_line() + 
  labs(title = "Anion Standard 4",
       x = "Time (Minutes)",
       y = "Conductivity (μSiemens)") +
  scale_x_continuous(limits = c(0, NA), breaks= seq(from=0, to=22, by=2)) +
  theme_bw()


pn1 <- nitrite1 %>%
  ggplot(aes(x=Minutes, y=Conductivity))+
  geom_line() + 
  labs(title = "Nitrite Standard 1",
       x = "Time (Minutes)",
       y = "Conductivity (μSiemens)") +
  scale_x_continuous(limits = c(0, NA), breaks= seq(from=0, to=22, by=2)) +
  theme_bw()

pn2 <- nitrite2 %>%
  ggplot(aes(x=Minutes, y=Conductivity))+
  geom_line() + 
  labs(title = "Nitrite Standard 2",
       x = "Time (Minutes)",
       y = "Conductivity (μSiemens)") +
  scale_x_continuous(limits = c(0, NA), breaks= seq(from=0, to=22, by=2)) +
  theme_bw()

#potentially contaminated? many peaks....omit
pn3 <- nitrite3 %>%
  ggplot(aes(x=Minutes, y=Conductivity))+
  geom_line() + 
  labs(title = "Nitrite Standard 3",
       x = "Time (Minutes)",
       y = "Conductivity (μSiemens)") +
  scale_x_continuous(limits = c(0, NA), breaks= seq(from=0, to=22, by=2)) +
  theme_bw()

pn4 <- nitrite4 %>%
  ggplot(aes(x=Minutes, y=Conductivity))+
  geom_line() + 
  labs(title = "Nitrite Standard 4",
       x = "Time (Minutes)",
       y = "Conductivity (μSiemens)") +
  scale_x_continuous(limits = c(0, NA), breaks= seq(from=0, to=22, by=2)) +
  theme_bw()


pstd1
pstd2
pstd3
pstd4

pn1
pn2
pn3
pn4


#FUNCTIONS
#standard peak widths

#require pracma

require(pracma)

#anion peaks (separate from Nitrite)
#function to identify peaks (minutes: start, end, and RT of max of each peak)

std_peak_widths <- function(data) {
  #Peak identification - standards
  anion_std <- c("Anion_Std_1",
                 "Anion_Std_2",
                 "Anion_Std_3",
                 "Anion_Std_4")
  std_peaks <- list()
  data <- data
  for(i in anion_std){
    data <- data
    data1 <- data[data$Ident==i,]
    #identifying peaks in the data
    peaks <- findpeaks(data1$Conductivity, minpeakdistance=300, npeaks=6)
    
    #observations for min and max of peak, use to select rows from larger dataset, to identify minutes
    obs1 <- c(peaks[,3]) 
    obs2 <- c(peaks[,4])
    obsmax <- c(peaks[,2])
    start <- data1[obs1,]
    end <- data1[obs2,]
    max <- data1[obsmax,]
    #order by index
    start<- start[order(as.numeric(rownames(start))),]
    end <- end[order(as.numeric(rownames(end))),]
    max <- max[order(as.numeric(rownames(max))),]
    start_end <- rbind(start, end)
    
    #ensure pairs of obs - start and end
    if(length(start_end$Minutes) %%2!=0){
      stop("You need a start and end for each peak")
    } else{
      #obtains start and end point of each peak (in minutes)
      df <- data.frame(
        Start = start$Minutes,
        End = end$Minutes,
        Max = max$Minutes,
        Ident = i,
        Ion = c("Fluoride", "Chloride", "Bromide", "Nitrate", "Phosphate", "Sulfate")
      )
      
      
    }
    name <- paste(i)
    std_peaks[[name]] <- df
    
  }
  return(std_peaks)
}


#nitrite peaks
nitrite_peaks <- function(data){
  std_peaks <- list()
  nitrite_std <- c("Nitrite_Std_1",
                   "Nitrite_Std_2",
                   #leaving out nitrite 3 as a lot of unexplained peaks
                   "Nitrite_Std_4")
  data <- data
  for(i in nitrite_std){
    data <- data
    data1 <- data[data$Ident==i,]
    #identifying peaks in the data
    peaks <- findpeaks(data1$Conductivity, minpeakdistance=300, npeaks=1)
    
    #observations for min and max of peak, use to select rows from larger dataset, to identify minutes
    obs1 <- c(peaks[,3]) 
    obs2 <- c(peaks[,4])
    obsmax <- c(peaks[,2])
    start <- data1[obs1,]
    end <- data1[obs2,]
    max <- data1[obsmax,]
    #order by index
    start<- start[order(as.numeric(rownames(start))),]
    end <- end[order(as.numeric(rownames(end))),]
    max <- max[order(as.numeric(rownames(max))),]
    start_end <- rbind(start, end)
    
    #ensure pairs of obs - start and end
    if(length(start_end$Minutes) %%2!=0){
      stop("You need a start and end for each peak")
    } else{
      #obtains start and end point of each peak (in minutes)
      df <- data.frame(
        Start = start$Minutes,
        End = end$Minutes,
        Max = max$Minutes,
        Ident = i,
        Ion =  "Nitrite")
      
    }    
    name <- paste(i)
    std_peaks[[name]] <- df
    
  }
  return(std_peaks)
  
}


#anions

require(MESS)
#function to detect peaks of anions
detect_peaks <- function(data){
  #initialise list
  peak_results <- list()
  data <- data
  auc_value <- c()
  
  #preprocess data (accounts for noise/calibration of instrument)
  data <- data[data$Minutes>=3,]
  
  
  #for loop - iterate across all mice, for all days 
  for (mouse in mice){
    peak_data <- data[data$Ident==mouse,]
    
    for (day in days){
      peak_data1 <- peak_data[peak_data$Day==day,]
      
      ##peak should be 3x baseline - 4.5 
      #min peak height of 3, based on graphs - optimal way to omit bumps/noise
      #min peak height of 2.1, 0.5 above baseline- optimising number of peaks obtained
      peaks <- findpeaks(peak_data1$Conductivity, minpeakdistance=250, minpeakheight=2.1)
      
      #index of start, end and max of each peak
      obs1 <- c(peaks[,3]) 
      obs2 <- c(peaks[,4])
      obsmax <- c(peaks[,2])
      start <- peak_data1[obs1,]
      end <- peak_data1[obs2,]
      max <- peak_data1[obsmax,]
      #order by index
      start<- start[order(as.numeric(rownames(start))),]
      end <- end[order(as.numeric(rownames(end))),]
      max <- max[order(as.numeric(rownames(max))),]
      start_end <- rbind(start, end)
      
      #ensure pairs of obs - start and end
      if(length(start_end$Minutes) %%2!=0){
        stop("You need a start and end for each peak")
      } else{
        #obtains start and end point of each peak (in minutes)
        df <- data.frame(
          Start = start$Minutes,
          End = end$Minutes,
          Max = max$Minutes
        )
        
        
        #AUC added to dataframe
        if(nrow(df) > 0){
          for (i in 1:nrow(df)) {
            start <- df$Start[i]
            end <- df$End[i]
            
            #subset data - only want conductivity values between start and end of peaks, in mins
            subset_data <- peak_data1 %>% 
              dplyr::filter(Minutes >= start & Minutes <=end)
            if (nrow(subset_data) > 1 &&
                all(is.finite(subset_data$Minutes)) &&
                all(is.finite(subset_data$Conductivity))) {
              
              auc_value <- MESS::auc(subset_data$Minutes, subset_data$Conductivity, absolutearea=TRUE)
            } else {
              auc_value <- NA
            }
            df$AUC[i] <- auc_value
          }
          
          
          #add in identifier
          df$Mouse = rep(mouse, nrow(df))
          df$Day = rep(day, nrow(df))
          
          ##########################################################################
          
          name <- paste0(mouse,"_Day_",day)
          peak_results[[name]] <- df
          
          
        }
      }
      
    }
  }
  return(peak_results)
}

#search function -searching Retention time window for mice data
name_anion <- function(df, rt_search) {
  df <- df 
  rt_search <- rt_search
  
  #initialise new column in dataframe
  df$Ion <- NA
  
  for (ion in vec_anions){
    rt_sub <- rt_search %>% dplyr::filter(rt_search$Ion==ion)
    search <- rt_sub %>% select(3,4)
    
    for(i in 1:nrow(df)){ 
      if (isTRUE(df$Max[i] >= search[,1] & df$Max[i] <= search[,2])){
        df$Ion[i] <- ion
        
      }
      
      
    }
  }
  return(df)
}
#compiling standards using functions
#anion standards AND Nitrite


#standard peaks
std_peaks <- std_peak_widths(std_data)

#returns list of df per std.
#want to bind these stds lists into one df
std_peaks_df <- do.call(rbind, std_peaks)


#nitrite peaks
nitrite_std_peaks <-nitrite_peaks(std_data)
#bind lists into df
nitrite_peaks_df <- do.call(rbind, nitrite_std_peaks)

#bind both together
#make copy to ensure it works
std_peaks_df1 <- rbind(std_peaks_df, nitrite_peaks_df)

#copy over
std_peaks_df <- std_peaks_df1

#remove rownames - looks messy
rownames(std_peaks_df) <- NULL


#find average RT (Max) of each ion, across all standards
#use this to write function to search mice samples

vec_anions <- unique(std_peaks_df$Ion)

#initialise dataframe
df_anion <- data.frame(Ion = c(),
                       RT = c(),
                       Lower = c(),
                       Upper = c())

#loop to add lower and upper limits

#find average RT for each ion
RT_anions <- std_peaks_df %>%
  group_by(Ion) %>%
  dplyr::summarise(RT = mean(Max))

#find upper and lower limits (+/- 5%) 


for(anion in vec_anions){
  df1 <- RT_anions
  df1$Lower = (df1$RT-(0.05*df1$RT))
  df1$Upper = (df1$RT+(0.05*df1$RT))
  
  
  return(df1)
}

#copy across
RT_anions <- df1

#anions
#find peaks/auc in mice data
mice_peaks <- detect_peaks(mice_data)
#apply ion detection function to each df returned from mice_peaks (detect_peaks data)
mice_peaks_list_ion <- lapply(mice_peaks, function(df) name_anion(df, RT_anions))

#bind list on named peaks into df
mice_peaks_df <- bind_rows(mice_peaks_list_ion)

#must model pmsi day using peak aucs
#must label all peaks - unidentified ones too
#Label non-classified peaks


#identifying non-labelled peaks
nonlabelled <- mice_peaks_df[is.na(mice_peaks_df$Ion),]


#label mice_peaks_list_ion
#labelling nonlabelled peaks
#use df_anion search ranges (peaks that fall in between upper end of one and lower end of other)
#so identify new consistent peaks
#note slight overlap between Nitrate and phosphate search ranges


#examining peaks that don't fall within RT windows
#peak 1 -> between Fluoride and Chloride 
peak1 <- mice_peaks_df[(mice_peaks_df$Max > 3.374061 & mice_peaks_df$Max < 4.277300),] 
#examine peak
peak1 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak1$Max) #3.5344
sort(peak1$Max) 
#examining this and plots, it does not look like there is a peak different from fluoride and chloride
#fluoride may be contaminated, which results in a flatter peak, with varying max heights
#split between fluoride and chloride



#peak 2 -> between Chloride and Nitrite 
peak2 <- mice_peaks_df[(mice_peaks_df$Max > 4.727542 & mice_peaks_df$Max < 5.057241),] 
peak2 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak2$Max)
sort(peak2$Max) 
#upper two (5.011033 and 5.051050 belong in nitrite)
#rest belong in Chloride

#will be omitted in filtration steps, as only one value
#peak3 -> between Nitrite and Bromide
peak3 <- mice_peaks_df[(mice_peaks_df$Max > 5.589582 & mice_peaks_df$Max < 6.183463),] 
peak3 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak3$Max)
sort(peak3$Max) 
#1 value - will be filtered out, leave as NA

#no gap between nitrate and phosphate


#peak4 -> between Phosphate and Sulfate 
peak4 <- mice_peaks_df[(mice_peaks_df$Max > 8.397502 & mice_peaks_df$Max < 8.827463),]
peak4 %>% ggplot()+ geom_boxplot(aes(y=Max)) 
median(peak4$Max) #8.66
sort(peak4$Max)
#based on identification steps, likely these are part of phosphate and sulfate
#natural divide point 8.789733 - after this, evident from plots sulfate - early days only, spread across mice
#further evidence - when selected as its own peak, phosphate and/or sulfate never identified in the same mouse/time point
#early days trend of earlier eluting sulfate
#gets later as days go on in general - seems to be the case in plots


#peak 5 -> >sulfate, less than 13
peak5 <- mice_peaks_df[(mice_peaks_df$Max > 9.828 & mice_peaks_df$Max < 13),] 
peak5 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak5$Max) 
sort(peak5$Max) #too great of a range to be one peak
#inspect plots 
#some values attributable to sulfate
#cutoff at ~10.07 (below this is sulfate)
#above is its own peak - peak A


#peak 5.1 #sulfate
peak5.1 <- mice_peaks_df[(mice_peaks_df$Max >9.828 & mice_peaks_df$Max < 10.065),]
median(peak5.1$Max)
sort(peak5.1$Max)
#based on plots, attribute to sulfate


#A
peak5.2 <- mice_peaks_df[(mice_peaks_df$Max > 10.064 & mice_peaks_df$Max < 13),]
median(peak5.2$Max)
sort(peak5.2$Max)

#B
#peak 6 -> greater than 13. Definitely a different peak
peak6 <- mice_peaks_df[(mice_peaks_df$Max > 13),] 
peak6 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak6$Max)



####################################################################################
#Handling of assigning peaks
######################################################################################
mice_peaks_df2 <- c()
mice_peaks_df2 <- mice_peaks_df
#make copy of reduced data


#working with this (compare to nonlabelled)
nonlabelled2 <- mice_peaks_df2[is.na(mice_peaks_df2$Ion),]



#peak 1: assign either to fluoride or chloride
#divide point - 3.85 - based on visually inspecting plots/potential contaminant 
#and observing values within peak 1
#this point is around where outliers are present in boxplot
#fluoride may be omitted from analysis later on
mice_peaks_df2 <- mice_peaks_df2 %>% 
  mutate(Ion = if_else(
    Max >= min(peak1$Max) & Max <= 3.85,"Fluoride", Ion
  ))

#assign upper values to chloride
mice_peaks_df2 <- mice_peaks_df2 %>% 
  mutate(Ion = if_else(
    Max >= 3.85 & Max <= max(peak1$Max),"Chloride", Ion
  ))


#peak 2 - split between Chloride and Nitrite

#assign lower peak 2 values to Chloride
mice_peaks_df2 <- mice_peaks_df2 %>% 
  mutate(Ion = if_else(
    Max >= min(peak2$Max) & Max <= 4.75,"Chloride", Ion
  ))

#upper peak 2 values to Nitrite
mice_peaks_df2 <- mice_peaks_df2 %>% 
  mutate(Ion = if_else(
    Max >= 4.75 & Max <= max(peak2$Max),"Nitrite", Ion
  ))

#peak 4 - between phosphate and sulfate
mice_peaks_df2 <- mice_peaks_df2 %>% 
  mutate(Ion = if_else(
    Max >= min(peak4$Max) & Max <= 8.789734,"Phosphate", Ion
  ))

mice_peaks_df2 <- mice_peaks_df2 %>% 
  mutate(Ion = if_else(
    Max >= 8.78975 & Max <= max(peak4$Max),"Sulfate", Ion
  ))




#assigning peak 5.1 to sulfate
range(peak5.1$Max)
mice_peaks_df2 <- mice_peaks_df2 %>% 
  mutate(Ion = if_else(
    Max >= min(peak5.1$Max) & Max <= max(peak5.1$Max),"Sulfate", Ion
  ))


#happy it worked, copy over to main dataset
mice_peaks_df <- mice_peaks_df2

#assigning names to unknown peaks

#assign A 
range(peak5.2$Max)
#choose the RTs associated with range of peak1 heights
mice_peaks_df2 <- mice_peaks_df2 %>%
  mutate(Ion = if_else(
    Max >= min(peak5.2$Max)  & Max <= max(peak5.2$Max), "A", Ion
  ))

#assign B 
range(peak6$Max)
mice_peaks_df2 <- mice_peaks_df2 %>%
  mutate(Ion = if_else(
    Max >= min(peak6$Max) & Max <= max(peak6$Max), "B", Ion
  ))




nonlabelled2 <- mice_peaks_df2[is.na(mice_peaks_df2$Ion),]
#zero...
#happy it worked, copy over
mice_peaks_df <- mice_peaks_df2

##################################################################################
#remove NAs
####################################################################################
mice_peaks_df2 <- mice_peaks_df2 %>% filter(!is.na(Ion))
#copy over
mice_peaks_df <- mice_peaks_df2

#####################################################################################
#Filtering AUC that is likely noise - AUC that falls below q1 - 1.5*IQR
#potentially not going to use this
################################################################################
#log transform as AUC varies across orders of magnitude
mice_peaks_df$log_auc <- log10(mice_peaks_df$AUC)
log_q1 <- quantile(mice_peaks_df$log_auc, 0.25)
log_IQR <- IQR(mice_peaks_df$log_auc)
#cut off range from Tukey 1977
lower_log_cutoff <- log_q1 - (1.5)*(log_IQR)

#filter out those below  the cut off range
mice_peaks_df <- mice_peaks_df[mice_peaks_df$log_auc >= lower_log_cutoff,]


###################################################################################
#further filtration: consider removal of peaks that don't appear in at least 5 mice.
######################################################################################

#group by mouse
#check frequency of each ion in each mouse
library(dplyr)
check <- mice_peaks_df %>% group_by(Mouse) %>% summarise(ions = unique(Ion),
                                                         .groups = "drop")

check.table <- table(check$ions)
#nitrite only appears in 1 mouse - filter
mice_peaks_df2 <- mice_peaks_df2 %>% filter(Ion!= "Nitrite")

#check the unique ion values
unique(mice_peaks_df2$Ion)
#copy over
mice_peaks_df <- mice_peaks_df2

######################################################################################################
#next step:
######################################################################################################
#check that variable appears in at least 50% of time points
total <- length(unique(mice_peaks_df$Day))

# Count number of unique days each ion appears in
library(dplyr)

ion_day_counts <- mice_peaks_df %>%
  distinct(Day, Ion) %>%
  count(Ion, name = "no_days_present") %>%
  mutate(perc_days = no_days_present / total,
         appears_50 = perc_days >= 0.5)


#all appear in at least 60% of time points
p

mod.data <- pivot_wider(mice_peaks_df, id_cols=c(Mouse, Day), names_from = Ion, values_from=AUC)

#issue with some duplication of labels
##all occur at split peaks.
#since small amount of this occurs, remove smaller of the split peak
#would get filtered out anyway, so no need to relabel

#removing part of a double peak for fluoride and A
mice_peaks_df2 <- mice_peaks_df2[-c(394, 379, 321, 92, 93),]

#test mod
mod.data <- pivot_wider(mice_peaks_df2, id_cols=c(Mouse, Day), names_from = Ion, values_from=AUC)


#examine filtered variables
#boxplots by mouse showing distribution of auc for each variable
p_chloride <- ggplot(mod.data) + geom_boxplot(aes(x=Chloride), na.rm=TRUE) +facet_wrap(~Mouse)+ 
  labs(title="Chloride AUC Boxplots",
       x="AUC")

p_sulfate <- ggplot(mod.data) + geom_boxplot(aes(x=Sulfate), na.rm=TRUE) +facet_wrap(~Mouse) +
  labs(title="Sulfate AUC Boxplots",
       x="AUC")

p_fluoride <- ggplot(mod.data) + geom_boxplot(aes(x=Fluoride), na.rm=TRUE) +facet_wrap(~Mouse) +
  labs(title="Fluoride AUC Boxplots",
       x="AUC")


p_a <- ggplot(mod.data) + geom_boxplot(aes(x=A), na.rm=TRUE) +facet_wrap(~Mouse) +
  labs(title = "Peak A AUC Boxplots",
       x="AUC")

p_b <- ggplot(mod.data) + geom_boxplot(aes(x=B), na.rm=TRUE) +facet_wrap(~Mouse) +
  labs(title="Peak B AUC Boxplots",
       x="AUC")

p_chloride
p_sulfate
p_fluoride
p_a
p_b
##some variation shown between mice with above boxplots




#NAs handling
#pmm imputation may be inappropriate, as if there is no peak, it may just be that the peak grows/goes away
library(mice)
library(naniar)

#examine missing data pattern
#include this plot
vis_miss(mod.data)
#sulfate 10%
#A 43%
#Fluoride 22%
#Phosphate 10%
#B 64%



#examine plots/data to see if peaks present
#if not, then NA does actually mean 0

mod.data[is.na(mod.data)] <-0

##########################################################################################
##modelling##
############################################################################################
#exploring potential models


#linear
full.model  <- lm(Day ~ Fluoride + Chloride + Phosphate + Sulfate + A + B, data=mod.data)
summary(full.model)



####################################################################
#backwards selection
drop1(full.model , test="F", scope= ~ Fluoride + Chloride + Phosphate + Sulfate + A + B)
#all sig

######################################################################################
#random effect of mouse

library(lme4)

#mixed full model
mixed.full.mod <- lmer(Day ~ Fluoride + Chloride + Phosphate + +Sulfate + A + B +(1|Mouse), data=mod.data)
summary(mixed.full.mod)


#examine models using cross validation
#use training and test data
#80/20 split

#80 training
#20 test
mod.data$Mouse <- as.character(mod.data$Mouse)
set.seed(122)
n <- nrow(mod.data)

training_indices <- sample(n, size=floor(0.8*n))

#split data
training <- mod.data[training_indices,]
test <- mod.data[-training_indices,]

#linear model using training data
linear.training <- lm(Day ~ Fluoride + Chloride + Phosphate + Sulfate + A + B, data=training)
summary(linear.training)

#backwards selection for most parsimonious model
drop1(linear.training , test="F", scope= ~ Fluoride + Chloride + Phosphate + Sulfate + A + B)
#A is non-sig, drop (note that this actually increases RSS)

linear.training1 <- lm(Day ~ Fluoride + Chloride + Phosphate + Sulfate+ B, data=training)
summary(linear.training1)
#all sig, proceed

#random model using training data
require(lme4)
#full mixed mod
random.training <- lmer(Day ~ Fluoride + Chloride + Phosphate + A + B + (1|Mouse), data=training)
summary(random.training)

#based on linear model, consider removing A as predictor
random.training1 <- lmer(Day ~ Fluoride + Chloride + Phosphate + B + (1|Mouse), data=training)
summary(random.training1)

#compare both random models using anova - this is important, equivalent to drop 1
anova(random.training, random.training1)
#very similar, but as non sig p-value and lower AIC, prefer smaller model 
#although deviance is higher, unsure


####check models using plot and qqnorm(resid(mixed.mod)) to check assumptions
#random.training
#random.training1

#examine model performance for both mixed models and simple models
AIC(linear.training)
AIC(random.training)
AIC(linear.training1)
AIC(random.training1)


#model performance - random effects (conditional and marginal R2, training set)
require(performance)

model_performance(random.training)
model_performance(random.training1)

compare_performance(random.training, random.training1)


#Pearson's correlation
#random full model
test$pred.ran <- predict(random.training, newdata=test)
R0 <- cor(test$Day, test$pred.ran, method="pearson")
#random smaller model
test$pred.ran1 <- predict(random.training1, newdata=test)
R1 <- cor(test$Day, test$pred.ran1, method="pearson")

#examine as a regression to examine bias
reg <- lm(test$pred.ran ~ test$Day)
summary(reg)
#intercept ~1.5, systematically predicting too high of a PMSI day
#slope pretty close to 1, good scaling

reg1 <- lm(test$pred.ran1 ~ test$Day)
summary(reg1)
#pretty much exactly the same


#plots - potentially not use - more info in the rmse/ mae plots
#slope
perfect_cor <- data.frame(
  actual = range(test$Day),
  predicted = range(test$pred.ran),
  line = "Correlation"
)

cor0 <- ggplot(test, aes(x=Day, y=pred.ran)) + 
  geom_point(color="blue", alpha=0.6) + 
  geom_smooth(aes(color="Model Fit"), method = "lm", se = FALSE, linetype = "solid") + 
  geom_line(data = perfect_cor, aes(x = actual, y = predicted, color = line), size = 1.2) +
  scale_color_manual(
    name = "Line",
    values = c("Model Fit" = "red", "Perfect Correlation" = "darkgreen")
  ) +
  labs(title="Full Model (Mixed.Mod)",
       x = "Days",
       y = "Predicted Days") +
  theme_bw()

cor1 <- ggplot(test, aes(x=Day, y=pred.ran1)) + 
  geom_point(color="blue", alpha=0.6) + 
  geom_smooth(aes(color="Model Fit"), method = "lm", se = FALSE, linetype = "solid") + 
  geom_line(data = perfect_cor, aes(x = actual, y = predicted, color = line), size = 1.2) +
  scale_color_manual(
    name = "Line",
    values = c("Model Fit" = "red", "Perfect Correlation" = "darkgreen")
  ) +
  labs(title="Reduced Model (Mixed.Mod1)",
       x = "Days",
       y = "Predicted Days") +
  theme_bw()

#put together
library(patchwork)
cor0 + cor1



#Test data RMSE and MAE values
#from performance::model_performance
rmse0 <- 4.826
rmse1 <- 4.786

library(Metrics)
mae0 <- mae(test$Day, test$pred.ran)
mae1 <- mae(test$Day, test$pred.ran1)



#this is essentially previous plot with correlation, with more info
#full mod
ggplot(test, aes(x = Day, y = pred.ran)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Full Model: Actual vs Predicted PMSI",
       subtitle = paste0("RMSE = ", round(rmse0, 2),
                         ", MAE = ", round(mae0, 2),
                         ", IQR PMSI = ", round(IQR.day, 2)),
       x="PMSI Day (Actual)",
       y="PMSI Day (Predicted)") 

ggplot(test, aes(x = Day, y = pred.ran1)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Full Model: Actual vs Predicted PMSI",
       subtitle = paste0("RMSE = ", round(rmse1, 2),
                         ", MAE = ", round(mae1, 2),
                         ", IQR = ", round(IQR.day, 2)),
       x="PMSI Day (Actual)",
       y="PMSI Day (Predicted)") 


#examining IQR of days - contextualise RMSE and MAE
IQR.day <- IQR(mod.data$Day) #~13 
median <- median(mod.data$Day) #14

#context - ratio of rmse/mae to iqr
#full model 
rmse0_norm <-rmse0/IQR.day #~0.37
mae0_norm <- mae0/IQR.day #0.29

#reduced model
rmse1_norm <-rmse1/IQR.day ##~0.36
mae1_norm <- mae1/IQR.day #~0.28
#all perform well in the context of the data
#very little difference between models, smaller model performs slightly better

#maybe - anova comparing random models

###plot predictions using ggeffects and some error around it






















###idk###
#compare models

#examine PRESS
#lower random PRESS, better model
#random model
h0 <- hatvalues(random.training)

# Get residuals
e0 <- residuals(random.training)

# Calculate PRESS
press0 <- sum((e0 / (1 - h0))^2)
press0

#linear model
h1 <- hatvalues(linear.training)

# Get residuals
e1 <- residuals(linear.training)

# Calculate PRESS
press1 <- sum((e1 / (1 - h1))^2)
press1



#similar AIC for both
AIC(linear.training)
AIC(random.training)
AIC(linear.training1)
AIC(random.training1)
#actually slightly disimproves AIC with smaller model

#more info:
install.packages("stargazer")
require(stargazer)

#this shows sig of coefficients in random training model - tabulate in report
stargazer(random.training1, type="text",
          digits = 3, star.cutoffs = c(0.05, 0.01, 0.002),
          digit.separator = "")
stargazer(random.training, type="text",
          digits = 3, star.cutoffs = c(0.05, 0.01, 0.002),
          digit.separator = "")



#save data

#ran script from scratch for all data saved with 190625 suffix
write.csv(anions, "C:/Users/jessi/OneDrive/Desktop/Project/1_saved_datasets/anions_190625.csv", row.names=FALSE)

write.csv(data, "C:/Users/jessi/OneDrive/Desktop/Project/1_saved_datasets/tidy_data_190625.csv", row.names=FALSE)

write.csv(mice_data, "C:/Users/jessi/OneDrive/Desktop/Project/1_saved_datasets/mice_data_190625.csv", row.names=FALSE)

#note- filtered df
write.csv(mice_peaks_df, "C:/Users/jessi/OneDrive/Desktop/Project/1_saved_datasets/mice_peaks_df_190625.csv", row.names=FALSE)


write.csv(RT_anions, "C:/Users/jessi/OneDrive/Desktop/Project/1_saved_datasets/RT_anions_190625.csv", row.names=FALSE)

#filtered to one day
write.csv(std_data, "C:/Users/jessi/OneDrive/Desktop/Project/1_saved_datasets/std_data_190625.csv", row.names=FALSE)

write.csv(std_peaks_df, "C:/Users/jessi/OneDrive/Desktop/Project/1_saved_datasets/std_peaks_df_190625.csv", row.names=FALSE)


#read in data
data <- read.csv("data.csv")
mice_data <- read.csv("mice_data.csv")
mice_peaks_df <- read.csv("mice_peaks_df.csv")
mice_peaks_df2 <- read.csv("mice_peaks_df2.csv")
RT_anions <- read.csv("RT_anions.csv")
std_data <- read.csv("std_data.csv")
std_peaks_df <- read.csv("std_peaks_df.csv")

remove(mice_peaks_df)
