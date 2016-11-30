# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# doParallel
# install.packages("broom")
# Load data from car package
data(Prestige, package = "car")  # data url: "http://goo.gl/UAVQc9"
system.time({
  output <- matrix(0, 0, 7)
  for(i in 1:5000){
    rownums <- sample(102, 70, replace=TRUE)
    mod <- glm(formula = prestige ~ education + type + income, data = Prestige[rownums, ])
    output <- rbind(output, broom::glance(mod))
  }
})


library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

getDoParWorkers()  # Check the number of workers registered.

# Foreach (Parallel Loop)
system.time({
  output_par <- foreach(i=1:5000, .combine=rbind, .options.snow=list(preschedule=TRUE)) %dopar% {
    rownums <- sample(102, 70, replace=TRUE)
    mod <- glm(formula = prestige ~ education + type + income, data = Prestige[rownums, ])
    out <- broom::glance(mod)
    return(out)
  }
})

# Caveat: Prestive and glance not exposed to foreach()
modperf <- function(){
  foreach(i=1:1000, .combine=rbind, .options.snow=list(preschedule=TRUE)) %dopar% {
    rownums <- sample(102, 70, replace=TRUE)
    mod <- glm(formula = prestige ~ education + type + income, data = Prestige[rownums, ])
    out <- broom::glance(mod)
    return(out)
  }
}
system.time(modperf())


# How to overcome?
modperf <- function(){
  r <- foreach(i=1:5000, .combine=rbind, .options.snow=list(preschedule=TRUE), 
               .export = c("Prestige"), .packages=c("broom")) %dopar% {
                 rownums <- sample(102, 70, replace=TRUE)
                 mod <- glm(formula = prestige ~ education + type + income, data = Prestige[rownums, ])
                 out <- broom::glance(mod)
                 return(out)
               }
}
r <- modperf()

stopCluster(cl)  # stop




# Answer
cl <- makeCluster(2)
registerDoParallel(cl)
getDoParWorkers()  # check number of workers

data(Prestige, package = "car")  # data url: "http://goo.gl/UAVQc9"
system.time({
  output_coeff <- foreach(i=1:5000, .combine = rbind, .options.snow=list(preschedule=TRUE)) %dopar% {
    rownums <- sample(102, 70, replace=TRUE)
    mod <- glm(formula = prestige ~ education + income, data = Prestige[rownums, ])
    out <- coefficients(mod)
    return(out)
  }
})
