#further filtration: consider removal of peaks that don't appear in at least 5 mice.
#same as appearing in only one mouse
#group by mouse
#check frequency of each ion in each mouse
library(dplyr)
check <- mice_peaks_df %>% group_by(Mouse) %>% summarise(ions = unique(Ion),
                                                         .groups = "drop")

check.table <- table(check$ions)
#Fluoride appears in only one mouse - filter.
#Nitrite only appears in one mouse - filter
#All other ions appear in all 6 mice. 

#filter steps
mice_peaks_df2 <- mice_peaks_df2 %>% filter(Ion!= "Fluoride")

mice_peaks_df2 <- mice_peaks_df2 %>% filter(Ion!= "Nitrite")
#check
unique(mice_peaks_df2$Ion)
#copy over
mice_peaks_df <- mice_peaks_df2

#next step:
#check that variable appears in at least 50% of time points
total <- length(unique(mice_peaks_df$Day))

# Count number of unique days each ion appears in
library(dplyr)

ion_day_counts <- mice_peaks_df %>%
  distinct(Day, Ion) %>%
  count(Ion, name = "no_days_present") %>%
  mutate(perc_days = no_days_present / total,
         appears_50 = perc_days >= 0.5)

#A, Chloride, E, Sulfate all appear >50% time points in total
#B, C, D and phosphate do not (note, phosphate very close @47%)

#omit B, C, D and phosphate
mice_peaks_df <- mice_peaks_df %>% filter(Ion!= "B")
mice_peaks_df <- mice_peaks_df %>% filter(Ion!= "C")
mice_peaks_df <- mice_peaks_df %>% filter(Ion!= "D")
mice_peaks_df <- mice_peaks_df %>% filter(Ion!= "Phosphate")
#use mice_peaks_df2 to test out this reduced dataset

mod.data <- pivot_wider(mice_peaks_df , names_from = Ion, values_from=AUC)
#select relevant columns
mod.data <- mod.data[,(4:9)]

#examine filtered variables
#boxplots by mouse showing distribution of auc for each variable
p_chloride <- ggplot(mod.data) + geom_boxplot(aes(x=Chloride), na.rm=TRUE) +facet_wrap(~Mouse)+ 
  labs(title="Chloride AUC Boxplots",
       x="AUC")

p_sulfate <- ggplot(mod.data) + geom_boxplot(aes(x=Sulfate), na.rm=TRUE) +facet_wrap(~Mouse) +
  labs(title="Sulfate AUC Boxplots",
       x="AUC")


p_a <- ggplot(mod.data) + geom_boxplot(aes(x=A), na.rm=TRUE) +facet_wrap(~Mouse) +
  labs(title = "Peak A AUC Boxplots",
       x="AUC")

p_e <- ggplot(mod.data) + geom_boxplot(aes(x=E), na.rm=TRUE) +facet_wrap(~Mouse) +
  labs(title="Peak E AUC Boxplots",
       x="AUC")

p_chloride
p_sulfate
p_a
p_e

##some variation shown between with above boxplots
#however very similar median across mice, at each variable
#except peak E mouse 6

#NAs handling
mod.data <- replace(mod.data, is.na(mod.data), 0)

#check class
class(mod.data$Day)
class(mod.data$Chloride)
