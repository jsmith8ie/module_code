##modelling##

full.model.  <- lm(Day ~ Chloride + Sulfate + A + E, data=mod.data)
summary(full.model. )

#backwards selection
drop1(full.model , test="F", scope= ~ Chloride + Sulfate +  A + E)
#peak E most non significant, drop

mod.1 <- lm(Day ~ Chloride + Sulfate + A, data=mod.data)

drop1(mod.1, test="F",, scope= ~ Chloride + Sulfate + A)
#peak A non significant, drop

mod.2  <- lm(Day ~ Chloride + Sulfate, data=mod.data)

drop1(mod.2 , test="F",, scope= ~ Chloride + Sulfate)
#all sig, 


#explore
summary(mod.2 )
AIC(mod.2 ) 
AIC(full.model )  
#mod.2  slightly disimproved the AIC

###random effect of mouse
library(lme4)

class(mod.data$Mouse)


filtered.mod <- lmer(Day ~ Chloride + Sulfate + (1|Mouse), data=mod.data)
filtered.mod


#final
summary(mod.2)
 filtered.mod



#training and test#

#80/20 split

#80 training, 20 test

set.seed(122)
n <- nrow(mod.data)

training_indices <- sample(n, size=floor(0.8*n))

#split data
training <- mod.data[training_indices,]
test <- mod.data[-training_indices,]



##modelling - training and test data##

training.model <- lm(Day ~ Chloride + Sulfate + A + E, data=training)


#backwards selection
drop1(training.model, test="F", scope= ~ Chloride + Sulfate + A + E)
#peak E most non significant, drop

training.mod.1 <- lm(Day ~ Chloride + Sulfate + A, data=training)

drop1(training.mod.1, test="F",, scope= ~ Chloride + Sulfate+ A)
#peak A non significant, drop

training.mod.2 <- lm(Day ~ Chloride + Sulfate, data=training)

drop1(training.mod.2, test="F",, scope= ~ Chloride + Sulfate)
#all sig, and #AIC lowest if no more removed, mod.2 final model


#explore
summary(training.mod.2)
AIC(training.mod.2)
AIC(training.model) #about the same AIC

###random effect of mouse
library(lme4)

class(mod.data$Mouse)

#set as factor
mod.data$Mouse <- as.factor(mod.data$Mouse)

random.training <- lmer(Day ~ Chloride + Sulfate + (1|Mouse), data=training)
random.training #warning
summary(random.training)


test$predictions <- predict(random.training, newdata=test)

#evaluate using R squared

r2 <- cor(test$predictions, test$Day)^2
r2 #almost 33% is not bad!!!!

#linear model exact same result