#standard peak widths

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
  
  #preprocess - remove baseline threshold of ~1.6 (based on day 0)
  data <- data[data$Conductivity>=1.5,]
  
  #for loop - iterate across all mice, for all days 
  for (mouse in mice){
    peak_data <- data[data$Ident==mouse,]
    
    for (day in days){
      peak_data1 <- peak_data[peak_data$Day==day,]
      
      ##peak should be 3x baseline - 4.5 (don't include yet)
      #min peak height of 3, based on graphs - optimal way to omit bumps/noise
      peaks <- findpeaks(peak_data1$Conductivity, minpeakdistance=300, minpeakheight=2.5)
      
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
              
              auc_value <- auc(subset_data$Minutes, subset_data$Conductivity)
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