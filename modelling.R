##modelling##

full.model  <- lm(Day ~ Chloride + Sulfate + A + E, data=mod.data)
summary(full.model)

####################################################################
#backwards selection
drop1(full.model , test="F", scope= ~ Chloride + Sulfate +  A + E)
#peak E most non significant, drop

#mod1, no E
mod.1 <- lm(Day ~ Chloride + Sulfate + A, data=mod.data)

drop1(mod.1, test="F", scope= ~ Chloride + Sulfate + A)
#peak A non significant, drop. Note - disimproves AIC and RSS

#mod 2, no A
mod.2  <- lm(Day ~ Chloride + Sulfate, data=mod.data)

drop1(mod.2 , test="F", scope= ~ Chloride + Sulfate)
#all sig, 
######################################################################################

#make mixed models of all three models. 
#random effect of mouse

library(lme4)

#mixed full model
mixed.full.mod <- lmer(Day ~ Chloride + Sulfate + A+ E + (1|Mouse), data=mod.data)
summary(mixed.full.mod)

#mixed mod 1
mixed.mod.1 <- lmer(Day ~ Chloride + Sulfate + A + (1|Mouse), data=mod.data)
summary(mixed.mod.1)

#mixed mod 2
mixed.mod.2 <- lmer(Day ~ Chloride + Sulfate + (1|Mouse), data=mod.data)
summary(mixed.mod.2)

##note, error for all mixed models. Random effect variance is zero. 
#also note, mixed and simple linear give same model in all instances. 
###could imply no variability between mice. Doesn't seem to be the case based on boxplots
###look deeper - plot mouse as a factor/fixed effect to examine it more closely

mod.data$Mouse <- as.factor(mod.data$Mouse)

#fixed model with moue as a factor
fixed.mod <- lm(Day ~ Chloride + Sulfate + Mouse, data=mod.data)
summary(fixed.mod)
#interestingly:
##relatively small estimates, (absolute value mostly <1)
##larger std errors (~2-3)
##implies that estimate for each mouse not sig diff from zero
##and high amount of within groups variation
#makes sense that this forces mixed model to estimate variation due to mouse as 0
#and high resiudal error


#examine and compare models using cross validation

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


#linear models
##modelling - training and test data##
training.model <- lm(Day ~ Chloride + Sulfate + A + E, data=training)


#backwards selection
drop1(training.model, test="F", scope= ~ Chloride + Sulfate + A + E)
#peak E most non significant, drop

#remove E
training.mod.1 <- lm(Day ~ Chloride + Sulfate + A, data=training)

drop1(training.mod.1, test="F",, scope= ~ Chloride + Sulfate+ A)
#peak A non significant, drop. However, similar AIC dropping, but lower RSS if not dropped

#remove A
training.mod.2 <- lm(Day ~ Chloride + Sulfate, data=training)

drop1(training.mod.2, test="F",, scope= ~ Chloride + Sulfate)
#all sig


#random models
##modelling - training and test data##

#try mouse as character
training$Mouse <- as.character(training$Mouse)
require(lme4)
#full mixed mod
r.training.model <- lmer(Day ~ Chloride + Sulfate + A + E + (1|Mouse), data=training)

#mixed mod 1
r.training.mod.1 <- lmer(Day ~ Chloride + Sulfate + A + (1|Mouse), data=training)

#mixed mod 2
r.training.mod.2 <- lmer(Day ~ Chloride + Sulfate + (1|Mouse), data=training)


#explore each model
#linear
summary(training.model)
summary(training.mod.1)
summary(training.mod.2)

#random
summary(r.training.model)
summary(r.training.mod.1)
summary(r.training.mod.2)

#examine AIC 
#linear
AIC(training.model)
AIC(training.mod.1)
AIC(training.mod.2)
#extremely similar AIC for all three
#random
AIC(r.training.model)
AIC(r.training.mod.1)
AIC(r.training.mod.2) #best (by a small margin)

#examine PRESS
#linear
#full model
h0 <- hatvalues(training.model)

# Get residuals
e0 <- residuals(training.model)

# Calculate PRESS
press0 <- sum((e0 / (1 - h0))^2)
press0

#mod.1
h1 <- hatvalues(training.mod.1)

# Get residuals
e1 <- residuals(training.mod.1)

# Calculate PRESS
press1 <- sum((e1 / (1 - h1))^2)
press1

#mod.2
h2 <- hatvalues(training.mod.2)

# Get residuals
e2 <- residuals(training.mod.2)

# Calculate PRESS
press2 <- sum((e2 / (1 - h2))^2)
press2
#full model has best PRESS, smallest model had worst

#random
#full model
h0r <- hatvalues(r.training.model)

# Get residuals
e0r <- residuals(r.training.model)

# Calculate PRESS
press0r <- sum((e0r / (1 - h0r))^2)
press0r

#mod.1
h1r <- hatvalues(r.training.mod.1)

# Get residuals
e1r <- residuals(r.training.mod.1)

# Calculate PRESS
press1r <- sum((e1r / (1 - h1r))^2)
press1r

#mod.2
h2r <- hatvalues(r.training.mod.2)

# Get residuals
e2r <- residuals(r.training.mod.2)

# Calculate PRESS
press2r <- sum((e2r / (1 - h2r))^2)
press2r
#linear and random exact same

#test set - predictions
#linear

#full model
test$predictions0 <- predict(training.model, newdata=test)
#evaluate R squared
r2_full <- cor(test$predictions0, test$Day)^2

#mod.1
test$predictions1 <- predict(training.mod.1, newdata=test)
#evaluate R squared
r2_1 <- cor(test$predictions1, test$Day)^2

#mod.2
test$predictions2 <- predict(training.mod.2, newdata=test)
#evaluate R squared
r2_2 <- cor(test$predictions2, test$Day)^2

#random
#full model
test$predictions0ran <- predict(r.training.model, newdata=test)
#evaluate R squared
r2_fullran <- cor(test$predictions0ran, test$Day)^2

#mod.1
test$predictions1r <- predict(r.training.mod.1, newdata=test)
#evaluate R squared
r2_1r <- cor(test$predictions1, test$Day)^2

#mod.2
test$predictions2r <- predict(r.training.mod.2, newdata=test)
#evaluate R squared
r2_2r <- cor(test$predictions2r, test$Day)^2

##########################################################################################
#examine mod.data.test
mod.data.test <- read.csv("mod.data.test.csv")

identical(mod.data, mod.data.test)

x <- anti_join(mod.data, mod.data.test)


mod.data <- mod.data[order(mod.data$Chloride),]
mod.data.test <- mod.data.test[order(mod.data.test$Chloride),]

summary(mod.data)
mod.data$Mouse <- as.character(mod.data$Mouse)
summary(mod.data.test)

x <- compare_df(mod.data, mod.data.test)
x$change_count
x$change_summary

summary(x)
table <- x$comparison_table_diff

x$comparison_table_diff_numbers
x$comparison_table_diff
x$change_markers
x$change_summary


mice_peaks_df


mice_peaks_df2 <- read.csv("mice_peaks_df2.csv")

identical(mice_peaks_df, mice_peaks_df2)

