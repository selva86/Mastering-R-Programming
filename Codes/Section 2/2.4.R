# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# DataPrep CODE URL: https://goo.gl/AazGLg
# Alternate DataPrep CODE URL: http://ow.ly/JDZa305qq3P

data(Prestige, package="car")
Prestige <- na.omit(Prestige)
set.seed(100)
train_rows<- sample(1:nrow(Prestige), size=0.7*nrow(Prestige))
training <- Prestige[train_rows, ]
test <- Prestige[-train_rows, ]

# Best Subsets ---------------------------------------------------------
library(leaps)
regsubsMod = regsubsets(prestige ~ education + income + type + women, 
                        data = Prestige)

# plot
par(oma=c(2,0,0,0))
plot(regsubsMod, scale = "bic")


regsubsMod2 = regsubsets(prestige ~ education * income * type * women, 
                         data = Prestige)
plot(regsubsMod2, scale = "adjr2")


# Stepwise -------------------------------------------------------------
base.mod <- lm(prestige ~ 1 , data= training)  
all.mod <- lm(prestige ~ ., data= training) 

stepMod<- step(base.mod, 
               scope = list(lower = base.mod, upper = all.mod), 
               direction = "both", trace = 1, steps = 1000)  

# ANOVA -----------------------------------------------------------------
mod1 <- lm(prestige ~ education, data= training)
mod2 <- lm(prestige ~ education + income , data= training)
mod3 <- lm(prestige ~ education + income + type, data= training)
mod4 <- lm(prestige ~ education + income + type + women, data= training)
anova(mod1, mod2, mod3, mod4)


# ----------------------------------------------------------------------
# Answer
library(car)
base.mod <- lm(prestige ~ 1 , data= training)  
all.mod <- lm(prestige ~ education * income * type * women, 
              data= training) 
stepMod<- step(base.mod, 
               scope = list(lower = base.mod, upper = all.mod), 
               direction = "both", 
               trace = 1, 
               steps = 1000) 
stepMod
vif(stepMod)

stepMod2 <- update(stepMod, . ~ . - education:type, data=training)


vif(stepMod2)
