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
