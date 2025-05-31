#compiling standards using functions
#anion standards AND Nitrite

#standard peaks
std_peaks <- std_peak_widths(std)

#returns list of df per std.
#want to bind these stds lists into one df
std_peaks_df <- do.call(rbind, std_peaks)


#nitrite peaks
nitrite_std_peaks <-nitrite_peaks(std)
#bind lists into df
nitrite_peaks_df <- do.call(rbind, nitrite_std_peaks)

#bind both together
#make copy to ensure it works
std_peaks_df1 <- std_peaks_df
std_peaks_df1 <- rbind(std_peaks_df, nitrite_peaks_df)

#copy over
std_peaks_df <- std_peaks_df1

#remove rownames - looks messy
rownames(std_peaks_df) <- NULL

#use df of standards (all of them, cycle through)
#returns list of df per std.
std_peaks <- std_peak_widths(std)

#want to bind these stds lists into one df
std_peaks_df <- do.call(rbind, std_peaks)


#nitrite peaks
nitrite_std_peaks <-nitrite_peaks(std)
#bind lists into df
nitrite_peaks_df <- do.call(rbind, nitrite_std_peaks)

#bind both together
#make copy to ensure it works
std_peaks_df1 <- std_peaks_df
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

#edit - may loosen +/- 5% to +/- 7.5% to see if that helps

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
#apply function to each df returned from mice_peaks (detect_peaks data)
mice_peaks_list_ion <- lapply(mice_peaks, function(df) name_anion(df, RT_anions))

#bind list on named peaks into df
mice_peaks_df <- bind_rows(mice_peaks_list_ion) 
