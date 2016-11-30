# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Import
ratingsDf <- read.csv("https://goo.gl/HyQcAX")

## URL: https://raw.githubusercontent.com/selva86/datasets/master/movie_ratings.csv
ratingsMat <- as.matrix(ratingsDf)
View(ratingsMat)

# install.packages("recommenderlab")
library(recommenderlab)
ratings <- as(ratingsMat, "realRatingMatrix")
ratings
class(ratings)


# method available: "cosine", "pearson", "jaccard"
usr_sim <- recommenderlab::similarity(ratings[1:10, ], method="cosine", 
                                      which="users")  
class(usr_sim)

usr_sim[is.na(usr_sim)] <- 0  # replace NA with 0's
usr_sim <- round(usr_sim, 2)  # round off
plot(hclust(usr_sim))


# Find similar items
item_sim <- recommenderlab::similarity(ratings[, 1:15], method="cosine", 
                                       which="items")
item_sim[is.na(item_sim)] <- 0  # replace NAs
item_sim <- round(item_sim, 2)  # round off
plot(hclust(item_sim))

# Methods available
recommenderRegistry$get_entries()


# Methods for realRatingMatrix
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models
names(recommender_models)

recommender_models$UBCF_realRatingMatrix

recommender_models$UBCF_realRatingMatrix$parameters

# Training and Test
set.seed(100)
train_rows <- sample(1:nrow(ratings), size=0.9*nrow(ratings), replace = F)
ratings_train <- ratings[train_rows, ]
ratings_test <- ratings[-train_rows, ]

rec_model <- Recommender(data = ratings_train, method = "UBCF")  # build the UBCF
rec_model
getModel(rec_model)  # get the model specifications as a list

# test
n_reco <- 15
recommendations <- predict(object = rec_model, newdata = ratings_test, n = n_reco)
recommendations

recommendations@ratings
recommendations@items
recommendations@itemLabels

reco_out <- as(recommendations, "list")
reco_out

# smaller set
top3 <- bestN(recommendations, 3)
top3
as(top3, "list")





# Answer
# Import
ratingsDf <- read.csv("https://goo.gl/HyQcAX")
## URL: https://raw.githubusercontent.com/selva86/datasets/master/movie_ratings.csv

ratingsMat <- as.matrix(ratingsDf)

# install.packages("recommenderlab")
library(recommenderlab)
ratings <- as(ratingsMat, "realRatingMatrix")
class(ratings)

# prepare training and test datasets.
set.seed(100)
train_rows <- sample(1:nrow(ratings), size=0.9*nrow(ratings), replace = F)
ratings_train <- ratings[train_rows, ]
ratings_test <- ratings[-train_rows, ]

# Popular
# model
rec_model_POPULAR <- Recommender(data = ratings_train, method = "POPULAR")  # build the UBCF
rec_model_POPULAR

# predict popular movies
n_reco <- 5
recommendations <- predict(object = rec_model_POPULAR, 
                           newdata = ratings_test, n = n_reco)
recommendations
as(recommendations, "list")

# Assoc mining
# Binarize the ratings with minimum rating of 3 to be positive
ratings_bin <- binarize(ratings, minRating=3)

# Create training and test samples.
set.seed(100)
train_rows <- sample(1:nrow(ratings_bin), size=0.9*nrow(ratings_bin), replace = F)
ratings_train <- ratings_bin[train_rows, ]
ratings_test <- ratings_bin[-train_rows, ]

# Check AR model parameters
recommender_models <- recommenderRegistry$get_entries(dataType="binaryRatingMatrix")
recommender_models$AR_binaryRatingMatrix$parameters

# Recommender model
rec_model_AR <- Recommender(data = ratings_train, method = "AR", parameter=list(maxlen=5))  # build the UBCF
rec_model_AR

# Predict
recommendations <- predict(object = rec_model_AR, newdata = ratings_test, n = n_reco)
recommendations

as(recommendations, "list")

