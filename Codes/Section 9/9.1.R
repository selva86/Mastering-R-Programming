# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(microbenchmark)

# Create dataframe ----------------------------------------------------
set.seed(100)
col1 <- runif (12^4, 0, 2)
col2 <- rnorm (12^4, 0, 2)
col3 <- rpois (12^4, 3)
col4 <- rchisq (12^4, 2)
df <- data.frame (col1, col2, col3, col4)
head(df)

# For-loop ------------------------------------------------------------
original <- microbenchmark({
  for (i in 1:nrow(df)) { # for every row
    if ((df[i, 'col1'] + df[i, 'col2'] + df[i, 'col3'] + df[i, 'col4']) > 4) { # check if > 4
      df[i, 5] <- "greater_than_4" # assign 5th column
    } else {
      df[i, 5] <- "lesser_than_4" # assign 5th column
    }
  }
}, times=10L)
# 3.919379 sec


# after vectorization and pre-allocation -------------------------------
output <- character(nrow(df)) # initialize output vector
preallocate <- microbenchmark({
  for (i in 1:nrow(df)) {
    if ((df[i, 'col1'] + df[i, 'col2'] + df[i, 'col3'] + df[i, 'col4']) > 4) {
      output[i] <- "greater_than_4"
    } else {
      output[i] <- "lesser_than_4"
    }
  }
  df$output <- output
}, times=10L)


# apply family ----------------------------------------------------------
apply_output <- microbenchmark({
  myfunc <- function(x) {
    if ((x['col1'] + x['col2'] + x['col3'] + x['col4']) > 4) {
      "greater_than_4"
    } else {
      "lesser_than_4"
    }
  }
  output <- apply(df[, c(1:4)], 1, FUN=myfunc)  # apply 'myfunc' on every row
  df$output <- output
}, times = 10L)

apply_output
# 118.0031 MS

# byte code compilation ------------------------------------------------------
library(compiler)
myFuncCmp <- cmpfun(myfunc)
bytecode_output <- microbenchmark({
  output <- apply(df[, c (1:4)], 1, FUN=myFuncCmp)
})

bytecode_output
# 96.50268 MS


# Taking the condition checking outside the loop ------------------------------
output <- character (nrow(df))
condition <- (df$col1 + df$col2 + df$col3 + df$col4) > 4  # condition check outside the loop
head(condition)
conditions_outside <- microbenchmark({
  for (i in 1:nrow(df)) {
    if (condition[i]) {
      output[i] <- "greater_than_4"
    } else {
      output[i] <- "lesser_than_4"
    }
  }
  df$output <- output
}, times = 10L)

conditions_outside
# 22.3288 MS


# Run for only TRUE conditions ----------------------------------------------------
output <- rep("lesser_than_4", nrow(df))
condition <- (df$col1 + df$col2 + df$col3 + df$col4) > 4
only_trues <- microbenchmark({
  for (i in (1:nrow(df))[condition]) {  # run loop only for true conditions
    if (condition[i]) {
      output[i] <- "greater_than_4"
    }
  }
  df$output <- output })

only_trues
# 15.53198MS


# Using ifelse() ------------------------------------------------------------------
ifelse_output <- microbenchmark({
  output <- ifelse ((df$col1 + df$col2 + df$col3 + df$col4) > 4, "greater_than_4", 
                    "lesser_than_4")
  df$output <- output
}, times = 10L)

ifelse_output
# 4.842585MS

# Using which() -------------------------------------------------------------------
which_smart <- microbenchmark({
  want = which(rowSums(df) > 4)
  output = rep("less than 4", times = nrow(df))
  output[want] = "greater than 4"
}, times = 10L) 

which_smart

# 1.582123MS

# Using data.table::set() --------------------------------------------------------
# Create dataframe
set.seed(100)
col1 <- runif (12^4, 0, 2)
col2 <- rnorm (12^4, 0, 2)
col3 <- rpois (12^4, 3)
col4 <- rchisq (12^4, 2)
df <- data.frame (col1, col2, col3, col4)
df$output <- ""

library(data.table)
library(microbenchmark)
set_output <- microbenchmark({
  for (i in 1:nrow(df)) { # for every row
    if ((df[i, 'col1'] + df[i, 'col2'] + df[i, 'col3'] + df[i, 'col4']) > 4){
      set(df, i, 5L, "greater_than_4")
    } else {
      set(df, i, 5L, "lesser_than_4")
    }
  }
}, times=10L)

set_output
# 1.264554


# Answer  ----------------------------------------------------------------------
m <- matrix(0, nrow = 10000, ncol=10000)
m <- as.data.frame(m)
set_out <- microbenchmark({
  for(i in 1:nrow(m)){
    set(m, i, i, i)
  }
}, times = 10L) 











































