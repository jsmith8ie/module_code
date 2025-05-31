#must model pmsi day using peak aucs
#must label all peaks - unidentified ones too
#Label non-classified peaks


#identifying non-labelled peaks
nonlabelled <- mice_peaks_df[is.na(mice_peaks_df$Ion),]


#labelling nonlabelled peaks
#use df_anion search ranges (peaks that fall in between upper end of one and lower end of other)
#so identify new consistent peaks
#note slight overlap between Nitrate and phosphate search ranges

#happy that peak 1 and peak 5 are their own peaks
#pretty sure peak 4 needs to be divided
#peak 3 needs some outliers put into sulfate
#peak 2 is chloride 

#A
#peak 1 -> between Fluoride and Chloride 
peak1 <- mice_peaks_df[(mice_peaks_df$Max > 3.374061 & mice_peaks_df$Max < 4.277300),] 
#examine peak
peak1 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak1$Max) #3.530233 (4 outliers)
sort(peak1$Max)

#is it possible this is fluoride?^
#some upper outliers?

#################################################################################
#executive decision that this is chloride, way too close not to be
################################################################################
#peak 2 -> between Chloride and Nitrite (maybe just chloride)
peak2 <- mice_peaks_df[(mice_peaks_df$Max > 4.727542 & mice_peaks_df$Max < 5.057241),] 
peak2 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak2$Max) #4.7367 
range(peak2$Max)
sort(peak2$Max)
#Likely that this is chloride, as it is extremely close to the upper end of the RT search window
#also very narrow range
#median is 0.009 above upper end
#test model as both a separate peak, and as chloride

#no peaks found between nitrite and nitrate
#no gap between nitrate and phosphate

#B
#peak3 -> between Phosphate and Sulfate (some might belong in sulfate?)
peak3 <- mice_peaks_df[(mice_peaks_df$Max > 8.397502 & mice_peaks_df$Max < 8.827463),]
peak3 %>% ggplot()+ geom_boxplot(aes(y=Max)) #potentially outliers belong in sulfate? 
median(peak3$Max) 
sort(peak3$Max)
#considerable number of outliers between 8.76 and 8.81 - very close to sulfate range

#highest 3 (outliers) should be attributed to sulfate
#as closer to sulfate range than others in this peak category
#also happen all in day 2, and 2 outliers in day 1, visually inspect plots, looks same as sulfate

#peak 4 -> >sulfate, less than 13 (may belong in sulfate?)
peak4 <- mice_peaks_df[(mice_peaks_df$Max > 9.756670 & mice_peaks_df$Max < 13),] 
peak4 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak4$Max) #mostly ~9.9 with a few on the upper end... maybe some belong in sulfate? -up to 10.8? maybe? idk
#this is too great of a range to be one peak
sort(peak4$Max)

#using plots and looking by day
#two different peaks visually

#10.66 is the cutoff
#C
#peak 4.1
peak4.1 <- mice_peaks_df[(mice_peaks_df$Max > 9.756670 & mice_peaks_df$Max < 10.66),]
median(peak4.1$Max)
#D
peak4.2 <- mice_peaks_df[(mice_peaks_df$Max > 10.66 & mice_peaks_df$Max < 13),]
median(peak4.2$Max)

#E
#peak 5 -> greater than 13. Definitely a different peak
peak5 <- mice_peaks_df[(mice_peaks_df$Max > 13),] 
peak5 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak5$Max)

#results in a total of 5 unknown peaks
#A - median =  3.530233
#B - median =  8.655433
#C - median = 9.905317
#D - median = 10.81412
#E - median = 13.05617


#to handle:
#assigning peak 2 to chloride
#assigning peak 3 outliers to sulfate

mice_peaks_df2 <- c()
mice_peaks_df2 <- mice_peaks_df
#peak 2 
peak2$Ion <- "Chloride"
range(peak2$Max)
min <- min(peak2$Max)
max <- max(peak2$Max)

mice_peaks_df2 <- mice_peaks_df2 %>% 
  mutate(Ion = if_else(
    Max >= min & Max <= max,"Chloride", Ion
  ))

#chose nonlabelled2 to compare number of observations to initial nonlabelled dataset
nonlabelled2 <- mice_peaks_df2[is.na(mice_peaks_df2$Ion),]
#happy it worked, copy over

mice_peaks_df <- mice_peaks_df2

#peak 3 outliers to sulfate
sort(peak3$Max)
#8.769 -8.804750

mice_peaks_df2 <- mice_peaks_df2 %>%
  mutate(Ion = if_else(
    Max >= 8.768 & Max <= 8.805, "Sulfate", Ion
  ))

nonlabelled2 <- mice_peaks_df2[is.na(mice_peaks_df2$Ion),]
#happy it worked, copy over
mice_peaks_df <- mice_peaks_df2


#must edit peak data now after updating Chloride and sulfate outliers
#A
#peak 1 -> between Fluoride and Chloride 
maxpeak1 <- mice_peaks_df[which(mice_peaks_df$Ion=="Chloride"),]
maxpeak1 <- min(maxpeak1$Max)
peak1 <- mice_peaks_df[(mice_peaks_df$Max > 3.374061 & mice_peaks_df$Max < maxpeak1),] 
#examine peak
peak1 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak1$Max) #3.530233 (4 outliers)
sort(peak1$Max)

#B
#peak3 -> between Phosphate and Sulfate (some might belong in sulfate?)
minpeakb <- mice_peaks_df[which(mice_peaks_df$Ion=="Phosphate"),]
minpeakb <- max(minpeakb$Max)

maxpeakb <- mice_peaks_df[which(mice_peaks_df$Ion=="Sulfate"),]
maxpeakb <- min(maxpeakb$Max)
peak3 <- mice_peaks_df[(mice_peaks_df$Max > minpeakb & mice_peaks_df$Max < maxpeakb),]
peak3 %>% ggplot()+ geom_boxplot(aes(y=Max)) #potentially outliers belong in sulfate? 
median(peak3$Max)#8.647967



#10.66 is the cutoff
#C
#peak 4.1
minpeakc <- mice_peaks_df[which(mice_peaks_df$Ion=="Sulfate"),]
minpeakc <- max(minpeakc$Max)
peak4.1 <- mice_peaks_df[(mice_peaks_df$Max > minpeakc & mice_peaks_df$Max < 10.66),]
median(peak4.1$Max)
#D
peak4.2 <- mice_peaks_df[(mice_peaks_df$Max > 10.66 & mice_peaks_df$Max < 13),]
median(peak4.2$Max)

#E
#peak 5 -> greater than 13. Definitely a different peak
peak5 <- mice_peaks_df[(mice_peaks_df$Max > 13),] 
peak5 %>% ggplot()+ geom_boxplot(aes(y=Max))
median(peak5$Max)

#5 unidentified peaks 
#A - median = 3.50233
peak1
#B - median =  8.647967
peak3
#C - median = 9.905317
peak4.1
#D - median = 10.81412
peak4.2
#E - median = 13.05702
peak5


#assign A 
range(peak1$Max)
#choose the RTs associated with range of peak1 heights
mice_peaks_df2 <- mice_peaks_df2 %>%
  mutate(Ion = if_else(
    Max >= 3.435  & Max <= 3.784, "A", Ion
  ))

#assign B (beneath outlier removal)
range(peak3$Max)
mice_peaks_df2 <- mice_peaks_df2 %>%
  mutate(Ion = if_else(
    Max >= 8.6  & Max <= 8.677, "B", Ion
  ))

#assign C
range(peak4.1$Max)
mice_peaks_df2 <- mice_peaks_df2 %>%
  mutate(Ion = if_else(
    Max >= 9.86  & Max <= 10.461, "C", Ion
  ))

#assign D
range(peak4.2$Max)
mice_peaks_df2 <- mice_peaks_df2 %>%
  mutate(Ion = if_else(
    Max >= 10.6657  & Max <= 11.255, "D", Ion
  ))

#assign E
range(peak5$Max)
mice_peaks_df2 <- mice_peaks_df2 %>%
  mutate(Ion = if_else(
    Max >= 13.04  & Max <= 14.64, "E", Ion
  ))

nonlabelled2 <- mice_peaks_df2[is.na(mice_peaks_df2$Ion),]
#happy it worked, copy over

mice_peaks_df <- mice_peaks_df2



