# Recommendation System


library(recommenderlab)
library(ggplot2)
data(MovieLense)
MovieLense

class(MovieLense)


# Finding similarity 
similarity_users <- similarity(MovieLense[1:4, ], method = "cosine", which = "users")

as.matrix(similarity_users)

image(as.matrix(similarity_users), main = "User similarity")

# Finding similarity in model
similarity_items <- similarity(MovieLense[, 1:4], method = "cosine", which = "items")
as.matrix(similarity_items)


recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")

names(recommender_models)


# exploring the rating data

vector_ratings <- as.vector(MovieLense@data)

unique(vector_ratings)

table(vector_ratings)

summary(vector_ratings)


# view count per moview

views_per_movie <- colCounts(MovieLense)

table_views <- data.frame(
  movie = names(views_per_movie),
  views = views_per_movie
)
table_views <- table_views[order(table_views$views, decreasing = TRUE), ]

ggplot(table_views[1:6, ], aes(x = movie, y = views)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Number of views of the top movies")
vector_ratings <- vector_ratings[vector_ratings != 0]

vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + ggtitle("Distribution of the ratings")

views_per_movie <- colCounts(MovieLense)

average_ratings <- colMeans(MovieLense)

average_ratings_relevant <- average_ratings[views_per_movie > 100]

ratings_movies <- MovieLense[rowCounts(MovieLense) > 50, colCounts(MovieLense) > 100]


average_ratings_per_user <- rowMeans(ratings_movies)


qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) + ggtitle("Distribution of the average rating per user")

# normalize the data
ratings_movies_norm <- normalize(ratings_movies)


min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)


# binarize the data:
# means 1 where user rated the movie
ratings_movies_watched <- binarize(ratings_movies, minRating = 1)

# if user rated movie more than 3:
ratings_movies_good <- binarize(ratings_movies, minRating = 3)


# IBCF - item based collaborative filtering

# Getting parameters of IBCF :
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters

# Train and Test Data:
which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies), replace = TRUE, prob = c(0.8, 0.2))
head(which_train)
recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30))

model_details <- getModel(recc_model)
model_details$description

col_sums <- colSums(model_details$sim > 0)

qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of the column count")

##Let's see which are the movies with the most elements:
which_max <- order(col_sums, decreasing = TRUE)[1:6]
rownames(model_details$sim)[which_max]

# Now extract the recommendations for users:
n_recommended <- 5


recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
recc_predicted

slotNames(recc_predicted)


recc_predicted@items[[1]]

recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})

dim(recc_matrix)

## TOp 4 movies recommended in IBCF
recc_matrix[, 1:4]



### Now start the UBCF:
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters


recc_model_UBCF <- Recommender(data = recc_data_train, method = "UBCF")
recc_model_UBCF


model_details_UBCF <- getModel(recc_model_UBCF)

names(model_details_UBCF)

# apply prediction :
n_recommended <- 6
recc_predicted_UBCF <- predict(object = recc_model_UBCF, newdata = recc_data_test, n = n_recommended)


# function to get movies wise users :
recc_matrix <- sapply(recc_predicted_UBCF@items, function(x){colnames(ratings_movies)[x]
})
dim(recc_matrix)

#Look at top 4 users
recc_matrix[, 1:4]


