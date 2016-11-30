# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(Rcpp)

# Compile C++ functions and makes it available in R ----------------------------------------------------
sourceCpp("rcppFuncs.cpp")
timesTwo(15)

# Define C++ function as from within .R file itself ----------------------------------------------------
cppFunction('NumericVector timesTwoCPP(NumericVector x) {
  return x * 2;
}')

# Call the function from R.
timesTwoCPP(21)

str(timesTwo(2))
str(timesTwo(2L))

# Handles multiple datatypes ---------------------------------------------------------------------------
str(timesTwoNew(as.numeric(2)))
str(timesTwoNew(as.integer(2)))
timesTwoNew("Selva")

# Subset a Numeric Vector ------------------------------------------------------------------------------
vecsub(1:10, 2, 5)

# Create A Data.Frame with columns as factors (by default) ---------------------------------------------
df1 <- makeDataFrame(x=letters[1:10], y=letters[11:20])
df1
str(df1)

# Create A Data.Frame with columns as characters --------------------------------------------------------
df2 <- makeDataFrameChar(x=c(1:5), y=c(11:15), to_char = T)
str(df2)

# Create A Data.Frame with columns as Numeric ----------------------------------------------------------
df2 <- makeDataFrameChar(x=c(1:5), y=c(11:15), to_char = F)
str(df2)

# Convert list to a Data.Frame --------------------------------------------------------------------------
l <- list(p=1:5, q=6:10)
sourceCpp("rcppFuncs.cpp")
makeListToDf(l)

# Get a specific row from a Data.Frame ------------------------------------------------------------------
# reference: http://stackoverflow.com/questions/24390581/rcpp-extract-row-of-a-dataframe
sourceCpp("rcppFuncs.cpp")
getRowFromDf(USArrests, 10)
getRowFromDf(USArrests, 12)

# Get a specific row from a Data.Frame along with rowname -----------------------------------------------
sourceCpp("rcppFuncs.cpp")
getRowFromDfWithRowname(USArrests, 10)
getRowFromDfWithRowname(mtcars, 10)

# Benchmark Problem from video 1 ------------------------------------------------------------------------
set.seed(100)
col1 <- runif (12^4, 0, 2)
col2 <- rnorm (12^4, 0, 2)
col3 <- rpois (12^4, 3)
col4 <- rchisq (12^4, 2)
df <- data.frame (col1, col2, col3, col4)
df$output <- ""

sourceCpp("rcppFuncs.cpp")
microbenchmark::microbenchmark(df$output <- myFunc(df), times = 100L)


# 250K rows
set.seed(100)
col1 <- runif (12^5, 0, 2)
col2 <- rnorm (12^5, 0, 2)
col3 <- rpois (12^5, 3)
col4 <- rchisq (12^5, 2)
df <- data.frame (col1, col2, col3, col4)
df$output <- ""
dim(df)
microbenchmark::microbenchmark(df$output <- myFunc(df), times = 100L)

# 3M Rows
set.seed(100)
col1 <- runif (12^6, 0, 2)
col2 <- rnorm (12^6, 0, 2)
col3 <- rpois (12^6, 3)
col4 <- rchisq (12^6, 2)
df <- data.frame (col1, col2, col3, col4)
df$output <- ""

dim(df)
microbenchmark::microbenchmark(df$output <- myFunc(df), times = 100L)

# 36 Million Rows
set.seed(100)
col1 <- runif (12^7, 0, 2)
col2 <- rnorm (12^7, 0, 2)
col3 <- rpois (12^7, 3)
col4 <- rchisq (12^7, 2)
df <- data.frame (col1, col2, col3, col4)
df$output <- ""
dim(df)
microbenchmark::microbenchmark(df$output <- myFunc(df), times = 10L)


# Answer ------------------------------------------------------------------------
sourceCpp("rcppFuncs.cpp")
m <- matrix(1:100, ncol=10)
getRowsFromMat(m, 2:3)
getRowsFromMat(m, c(1,3,5))


