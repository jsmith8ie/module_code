#tidy anion data
data

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