library('tidyverse')

library(rsample)
library(caret)  # For RMSE calculation
library(forcats)


movies <- read_csv("datasets/IMDB_movies.csv")

movies_clean <- 
  movies %>% 
  mutate(budgetM = budget/1000000,
         grossM = gross/1000000,
         profitM = grossM - budgetM,
         ROI = profitM/budgetM,
         genre_main = as.factor(unlist(map(strsplit(as.character(movies$genres),"\\|"),1))) %>% fct_lump(12),
         rating_simple = fct_lump(content_rating, n = 6)
  ) %>%
  filter(budget < 400000000, 
         content_rating != "", 
         content_rating != "Not Rated",
         language == "English") %>% 
  mutate(rating_simple = rating_simple %>% fct_drop()) %>% 
  rename(director = director_name, 
         title = movie_title,
         year = title_year) %>% 
  select(-c(actor_1_name, actor_2_name,actor_1_facebook_likes, actor_2_facebook_likes, 
            budget, gross, aspect_ratio, num_voted_users,num_user_for_reviews)) %>% 
  relocate(title, year, country, director, budgetM, grossM, profitM, ROI, imdb_score, genre_main, rating_simple, language, duration)

# part b

set.seed(310)
movies_split <- initial_split(movies_clean, prop = 0.8)
movies_train <- training(movies_split)
movies_test <- testing(movies_split)

# part c

movies_train$rating_simple <- relevel(movies_train$rating_simple, ref = "R")

model <- lm(grossM ~ imdb_score + budgetM + rating_simple, data = movies_train)
summary(model)

# part d.   A higher budget indicates a higher gross profit as the coefficient is positive, a budget  increase of one unit will increase gross return by .97 million $.It is significant as P is < 0.05

# part e. A movie of G rating will have the largest impact on the gross return, with on average a return of 33.74 million dollars grossing, compared to pg13 which is only 17.14 million. The G rating is also significant.

# part F. p value is 2e-16, so an incredibly small number, way smaller than 0.05. This means that the null hypothesis being imbd score does not affect gross return can be rejected. It is almost with 100% certainty that the higher the imdb score,
#the more grossing a film will make.


# part g

train_predictions <- predict(model, movies_train)
test_predictions <- predict(model, movies_test)

# Combine actual and predicted values in the test set for plotting
test_results <- movies_test %>%
  mutate(predicted_grossM = test_predictions)

# part h,    Plotting
ggplot(test_results, aes(x = grossM, y = predicted_grossM)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)  # Adding a line for perfect predictions


# part I
rmse_train <- RMSE(train_predictions, movies_train$grossM)
rmse_test <- RMSE(test_predictions, movies_test$grossM)

rmse_train
rmse_test

# The model is not overfit, as the test set does not have a higher or significantly higher rmse than the training set, our model is decent because the training / testing sets have similar rmse
# The model does have a pretty big error though, on average its wrong by 50 million dollars in predicting gross score, but that can be improved by adding more independent variables.