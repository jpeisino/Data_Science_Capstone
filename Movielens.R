
#######################################################
### 1.1-Install all packages necessary for this project
#######################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(knitr)
library(gridExtra)

###################################################################
#### 1.2- Download database and split in edx(training set) validation (test set)
###################################################################

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <-  movielens[-test_index,]
temp <-  movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#########################################
### 2.1.1 - Exploring databases
##########################################

#-----------------------------------------
##### **Training Set (edx)**
#-----------------------------------------

##Analyze structure of edx
str(edx)
##Analyze the range of ratings
paste0("Movie rating goes from ",round(min(edx$rating),2)," to ",round(max(edx$rating),2)) 
##Analyze if there are NA
sum(is.na(edx$userId),is.na(edx$movieId),is.na(edx$genres),is.na(edx$timestamp),is.na(edx$rating),is.na(edx$title))
##Number of users
paste0("there are ", n_distinct(edx$userId)," diferent users")
##Number of movies
paste0("there are ",n_distinct(edx$movieId)," diferent movies")
##Number of titles
paste0("there are ",n_distinct(edx$title)," diferent titless")

#-----------------------------------
##### **Test Set (validation)**
#-----------------------------------

##Structure of validation set
str(validation)
##Range of ratings 
paste0("Movie rating goes from ",round(min(validation$rating),2)," to ",round(max(validation$rating),2))  
#Check if there are NA
sum(is.na(validation$userId),is.na(validation$movieId),is.na(validation$genres),is.na(validation$timestamp),is.na(validation$rating),is.na(validation$title))
#number of users
paste0("there are ", n_distinct(validation$userId)," diferent users")
#number of movies
paste0("there are ",n_distinct(validation$movieId)," diferent movies")
#number of titles
paste0("there are ",n_distinct(validation$title)," diferent titles")

####################################################################
### 2.1.2 - Cleaning databases
####################################################################

#to save all cleaning we will create new data frames

edx_y<-edx


#### Creating new columns
#year= Year movie was released
#rate_date= date when user rated the movie
#yr_rate= year the user rated the movie

###-----Edx Set---------------------

##generating new column for year and extracting it from title
year<-str_extract(edx$title,"\\((\\d{4})\\)")
year<-str_replace(str_replace(year,"[(]",""),"[)]", "")
year<-as.numeric(year)
##generating extra columns for year of rating, and date of rating
edx_y<-edx%>%mutate(year=year,rate_date=as_datetime(timestamp),
                    yr_rate=year(rate_date))

###-----Validation Set--------------

##generating new column for year and extracting it from title
yearv<-str_extract(validation$title,"\\((\\d{4})\\)")
yearv<-str_replace(str_replace(yearv,"[(]",""),"[)]", "")
yearv<-as.numeric(yearv)
##generating extra columns for year of rating, and date of rating
validation<-validation%>%mutate(year=yearv,rate_date=as_datetime(timestamp),
                                  yr_rate=year(rate_date))
###checking ranges
##Check movie years in correct range
range(edx_y$year)
range(edx_y$yr_rate)
##Check year of rating in corect range
range(validation$year)
range(validation$yr_rate)

################################################################
### 2.1.3 Analyzing impact of different variables in the ratings
################################################################

#---------------------------------------------------------------
#### 2.1.3.1 Rating
#---------------------------------------------------------------

##Rating distribution

edx_y%>%ggplot(aes(rating))+
  geom_bar(fill="steelblue")+
  geom_vline(xintercept = mean(edx_y$rating), lty = 2, color= "Red")+
  labs(title = "Rating Distribution",x = "Ratings", y = "times")

##Mean rating
mean_rating<-mean(edx_y$rating)
paste0("Average rating is ",round(mean(edx_y$rating),2))

#---------------------------------------------------------------
#### 2.1.3.2 Rating vs MovieID
#---------------------------------------------------------------

r_mov<-edx_y%>%group_by(title)%>%mutate(n=n())
#Movie with more ratings
paste0("The movie with more ratings is: ", r_mov$title[which.max(r_mov$n)])

#Average number of ratings per movie
mean_r_movie<-mean(r_mov$n)
paste0("Average quantity of ratings per movie is: ",round(mean_r_movie,0))

#Median number of ratings per movie
paste0("Median quantity of ratings per movie is: ",round(median(r_mov$n),0))

##Frequency of rating distribution
edx_y%>%group_by(movieId)%>%mutate(n=n())%>%ggplot(aes(n))+
  geom_histogram(bins = 30,fill="steelblue")+
  geom_vline(xintercept = mean_r_movie, lty = 2, color= "Red")+
  geom_vline(xintercept = median(r_mov$n), lty = 2, color= "Black")+
  labs(title = "Frequency Distribution",x = "times rated", y = "qty of movies")

###Rating Distribution per movie

edx_y%>%group_by(movieId)%>%mutate(avg_rat=mean(rating))%>%
  ggplot(aes(avg_rat))+geom_histogram(bins = 20,fill="steelblue")+ 
  labs(title = "Rating Distribution per movie",x = "Ratings", y = "users")+geom_vline(xintercept = mean_rating, lty = 2, color= "Black")

#---------------------------------------------------------------
#### 2.1.3.3 Rating vs UserID
#---------------------------------------------------------------

ratings_per_user<-edx_y%>%group_by(userId)%>%mutate(n_rating=n())

#range of number of rating per user
paste0("max and min qty of ratings are: ",range(ratings_per_user$n_rating))

#max number of rating per user
paste0("the user that rated more movies was: ", ratings_per_user$userId[which.max(ratings_per_user$n_rating)])

##Mean number ofrating per user
mean_rating_per_user<-mean(ratings_per_user$n_rating)
paste0("Average qty of ratings per user is: ",mean_rating_per_user)

##Median of ratings per user
paste0("Median qty of ratings per user is: ",median(ratings_per_user$n_rating))

##distribution of number of rating per user
edx_y%>%group_by(userId)%>%mutate(n=n())%>%ggplot(aes(n))+
  geom_histogram(bins = 30,fill="steelblue")+
  geom_vline(xintercept = mean_rating_per_user, lty = 2, color= "Red")+
  labs(title = "Rating Distribution",x = "times rated", y = "qty users")

##Distribution of how users rate

edx_y%>%group_by(userId)%>%mutate(avg_rat=mean(rating))%>%ggplot(aes(avg_rat))+
  geom_histogram(bins = 20,fill="steelblue")+ 
  labs(title = "Rating Distribution per user",x = "Ratings", y = "users")


#---------------------------------------------------------------
#### 2.1.3.4 Rating vs genres
#---------------------------------------------------------------

#numbers of genres
n_distinct(edx_y$genres)

##Split genres
genres<-edx_y%>%separate_rows(genres, sep = "\\|")%>%group_by(genres)%>%summarize(n=n(),avg_rat=mean(rating))%>%arrange(desc(avg_rat))
genres%>%kable()
##Quantity of movies per genre
genres%>%ggplot(aes(reorder(genres,-n),n))+geom_bar(fill="steelblue",stat="identity")+coord_flip()
##Average rating per genre
genres%>%arrange(desc(avg_rat))%>%ggplot(aes(reorder(genres,-avg_rat),avg_rat))+geom_bar(fill="steelblue",stat="identity")+coord_flip()

#---------------------------------------------------------------
#### 2.1.3.4 Rating vs year of release
#---------------------------------------------------------------

##Mean rating per year of release
edx_y%>%group_by(year)%>%summarize(avg_rat=mean(rating))%>%
  ggplot(aes(year,avg_rat))+geom_point()+
  labs(title = "Average rating per year of release")+geom_smooth()

##Quantity of ratings per year of release
edx_y%>%group_by(year)%>%summarize(n_movies=n_distinct(movieId))%>%
  ggplot(aes(year,n_movies))+geom_line()+
  labs(title = "Quantity of released per year")

##Quantity of ratings per year of release
edx_y%>%group_by(year)%>%
  summarize(n=n(),n_movies=n_distinct(movieId),
            n_ratings_per_movie=n/n_movies)%>%
  ggplot(aes(year,n_ratings_per_movie))+
  geom_point()+labs(title = "Quantity of rating per year of release")+
  geom_smooth()
#---------------------------------------------------------------
#### 2.1.3.5 Rating vs date of rating
#---------------------------------------------------------------

##Average ratings per year of rating
yr1<-edx_y%>%group_by(yr_rate)%>%summarize(avg_rt=mean(rating))%>%
  ggplot(aes(yr_rate,avg_rt))+geom_point()+geom_smooth()
##Quantity of ratings per year of rating
yr2<-edx_y%>%group_by(yr_rate)%>%summarize(n=n())%>%
  ggplot(aes(yr_rate,n))+geom_bar(fill="steelblue",stat="identity")
##present charts in a grid
grid.arrange(yr1,yr2, ncol = 2)


##Average ratings per year of rating excl 1995 and 2009
edx_y%>%filter(yr_rate>1995&yr_rate<2009)%>%group_by(yr_rate)%>%summarize(avg_rt=mean(rating))%>%ggplot(aes(yr_rate,avg_rt))+geom_point()+geom_smooth()

##Average ratings per year of rating
edx_y%>%mutate(day=weekdays(rate_date))%>%group_by(day)%>%
  summarize(avg_rating=mean(rating))%>%kable()

##Average ratings per month of rating
edx_y%>%mutate(month=month(rate_date))%>%group_by(month)%>%summarize(avg_rating=mean(rating))%>%ggplot(aes(month,avg_rating))+geom_point()+geom_smooth()



#####################################################################
### 3-Results
#####################################################################


##Crete RMSE function to evaluate all models

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
###divide training set in training and test to use when tuning parameters
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx_y$rating, times = 1, p = 0.1, list = FALSE)
train<-edx_y[-test_index,]
temp<-edx_y[test_index,]

# Make sure userId and movieId in test set are also in train set
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from validation set back into train set
removed <- anti_join(temp, test)
train <- rbind(train, removed)
rm(test_index, temp,removed)

#-------------------------------------------------------------------
### MODEL 1: asume same rating for all:  Yu,i= mu + erru,i
#-------------------------------------------------------------------

##Calculate mean rating
mu_hat<-mean(train$rating)
##evaluate RMSE of the model that assumes all ratings the same
RMSE_model1<-RMSE(test$rating,mu_hat)
##Save information in a table
RMSE_1<-data.frame(Method= "Just mean", RMSE= RMSE_model1)
knitr::kable(RMSE_1)

#-------------------------------------------------------------------
### MODEL 2: considering movie effect Yu,i= mu + bi + erru,i
#-------------------------------------------------------------------

## we group by movie and extract mean to actual rating and we get the movie effect 
movie_effect<-train%>%group_by(movieId)%>%
  summarize(b_i=mean(rating-mu_hat))

##Movie effect chart to observe dispersion
qplot(movie_effect$b_i,bins=20)

#we predict values of test set
predicted_rating_model2 <-test %>% 
  left_join(movie_effect, by='movieId') %>%
  mutate(pred = mu_hat + b_i)%>%
  pull(pred)

#Calculate RMSE for this method
RMSE_model2<-RMSE(test$rating,predicted_rating_model2)
RMSE_2<-data.frame(Method= "mean + movie effect", RMSE= RMSE_model2)
RMSE_results<-bind_rows(RMSE_1,RMSE_2)
knitr::kable(RMSE_results)

#-------------------------------------------------------------------
### MODEL 3: Consider user effect Yu,i=mu+ bi+ + bu + erru,i
#-------------------------------------------------------------------

##Calculate user effect
user_effect<-train%>%group_by(userId)%>%
  left_join(movie_effect, by='movieId') %>%
  summarize(b_u=mean(rating-mu_hat-b_i))

##Plot histogram of user effect to observe variability
qplot(user_effect$b_u,bins=20)

##Predict values
predicted_rating_model3 <-test %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  mutate(pred = mu_hat + b_i +b_u)%>%
  pull(pred)
##evaluate RMSE
RMSE_model3<-RMSE(test$rating,predicted_rating_model3)
RMSE_3<-data.frame(Method= "mean + movie effect + user effect", RMSE= RMSE_model3)
##Print result
RMSE_results<-bind_rows(RMSE_1,RMSE_2,RMSE_3)
knitr::kable(RMSE_results)

#-------------------------------------------------------------------
### MODEL 4 Consider aging effect Y u,i= =mu + b_i + b_u + b_y + err_u,i
#-------------------------------------------------------------------

##Calculate aging effect
year_effect<-train%>%
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  group_by(year)%>%
  summarize(b_y=mean(rating-mu_hat-b_i-b_u))

#Plot aging efect to se variability

year_effect%>%ggplot(aes(year,b_y))+geom_point()+geom_smooth()

##Predict ratings using all the effects
predicted_rating_model4 <- test %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(year_effect, by='year') %>%
  mutate(pred = mu_hat + b_i +b_u+b_y)%>%
  pull(pred)

#calculate RMSE
RMSE_model4<-RMSE(test$rating,predicted_rating_model4)
RMSE_4<-data.frame(Method= "mean + movie effect + user effect + aging", RMSE= RMSE_model4)
#Print result
RMSE_results<-bind_rows(RMSE_1,RMSE_2,RMSE_3,RMSE_4)
knitr::kable(RMSE_results)

#-------------------------------------------------------------------
###   MODEL 5: Consider genre Y u,i= mu+b_i + b_u +b_y + b_g + err_u,i
#-------------------------------------------------------------------


##Calculate genre effect
genre_effect<-train%>%
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(year_effect, by='year') %>%
  group_by(genres)%>%
  summarize(b_g=mean(rating-mu_hat-b_i-b_u-b_y))

##Plot to see variability
genre_effect%>%ggplot(aes(genres,b_g))+geom_point()+geom_smooth()

#Predict values using all effects
predicted_rating_model5 <- test %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(year_effect, by='year') %>%
  left_join(genre_effect, by='genres') %>%
  mutate(pred = mu_hat + b_i +b_u+b_y+b_g)%>%
  pull(pred)

##Calculate RMSE
RMSE_model5<-RMSE(test$rating,predicted_rating_model5)
RMSE_5<-data.frame(Method= "mean + movie effect + user effect + aging + genre", RMSE= RMSE_model5)

#Print result
RMSE_results<-bind_rows(RMSE_1,RMSE_2,RMSE_3,RMSE_4,RMSE_5)
knitr::kable(RMSE_results)

#-------------------------------------------------------------------
#REGULARIZATION
#-------------------------------------------------------------------

#-------------------------------------------------------------------
### MODEL 6: REGULARIZED Consider movie effect 
#-------------------------------------------------------------------

##First find best tuning of lambda
lambdas<-seq(1,2,0.1)
##For all lambdas calculate rmses  
rmses <- sapply(lambdas, function(l){
  #calculate movie effect
  movie_effect_reg <- train %>%
    group_by(movieId)%>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  #Predict regularized movie effect
  predicted_ratings_model_6 <- test %>% 
    left_join(movie_effect_reg, by = "movieId") %>%
    mutate(pred = mu_hat + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings_model_6, test$rating))
})
##Select lambda with lower RMSES
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
##Calculate model using optimum lambda
lambda<-lambdas[which.min(rmses)]
##movie effect
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda))
##predict values
predicted_rating_model6 <- test  %>% 
  left_join(movie_effect_reg, by = "movieId")%>%
  mutate(pred = mu_hat + b_i) %>%
  pull(pred)
#calculate RMSE
RMSE_model6<-RMSE(test$rating,predicted_rating_model6)
RMSE_6<-data.frame(Method= "mean + movie effect REGULARIZED", RMSE= RMSE_model6)
#Print values
RMSE_results<-bind_rows(RMSE_1,RMSE_2,RMSE_6,RMSE_3,RMSE_4,RMSE_5)
knitr::kable(RMSE_results)
#-------------------------------------------------------------------
### MODEL 7: REGULARIZED Consider movie effect and user effect 
#-------------------------------------------------------------------

##First find best tuning of lambda
lambdas<-seq(4,6,0.2)
##For all lambdas calculate rmses
rmses <- sapply(lambdas, function(l){
  ##movie effect
  movie_effect_reg <- train %>%
    group_by(movieId)%>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  #user effect
  user_effect_reg <- train %>%
    left_join(movie_effect_reg, by="movieId") %>%
    group_by(userId)%>%
    summarize(b_u = sum(rating - b_i- mu_hat)/(n()+l))
  #predict calues
  predicted_ratings_model_7 <- test %>% 
    left_join(movie_effect_reg, by = "movieId") %>%
    left_join(user_effect_reg, by = "userId") %>%
    mutate(pred = mu_hat + b_i + b_u) %>%
    pull(pred)
  #save all RMSES
  return(RMSE(predicted_ratings_model_7, test$rating))
})
#plot lambda vs accuracy
qplot(lambdas, rmses)  
#select lambda that optimizes RMSE
lambdas[which.min(rmses)]

##calculate effects with optimum lambda
lambda<-lambdas[which.min(rmses)]
##movie effect
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda))
#user effect
user_effect_reg <- train %>%
  left_join(movie_effect_reg, by="movieId") %>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - b_i- mu_hat)/(n()+lambda))
#prediction
predicted_rating_model7 <- test %>% 
  left_join(movie_effect_reg, by = "movieId") %>%
  left_join(user_effect_reg, by = "userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
#RMSE final calculation
RMSE_model7<- RMSE(predicted_rating_model7, test$rating)
RMSE_model7<-RMSE(test$rating,predicted_rating_model7)
##Print values
RMSE_7<-data.frame(Method= "mean + movie effect + user_effect REGULARIZED", RMSE= RMSE_model7)
RMSE_results<-bind_rows(RMSE_1,RMSE_2,RMSE_6,RMSE_3,RMSE_7,RMSE_4,RMSE_5)
knitr::kable(RMSE_results)   

#-------------------------------------------------------------------
###MODEL 8:REGULARIZED Consider movie effect, user effect, aging effect 
#-------------------------------------------------------------------

##first select optimum lambda
lambdas<-seq(4,5,.1)
##create function to calculate rmses for all lambdas
rmses <- sapply(lambdas, function(l){
  #Movie effect
  movie_effect_reg <- train %>%
    group_by(movieId)%>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  #User effect
  user_effect_reg <- train %>%
    left_join(movie_effect_reg, by="movieId") %>%
    group_by(userId)%>%
    summarize(b_u = sum(rating - b_i- mu_hat)/(n()+l))
  #Aging effect
  year_effect_reg<-train%>%
    left_join(movie_effect_reg, by='movieId') %>%
    left_join(user_effect_reg, by='userId') %>%
    group_by(year)%>%
    summarize(b_y=sum(rating-mu_hat-b_i-b_u)/(n()+l))
  #predict values
  predicted_ratings_model_8 <- test %>% 
    left_join(movie_effect_reg, by = "movieId") %>%
    left_join(user_effect_reg, by = "userId") %>%
    left_join(year_effect_reg, by = "year") %>%
    mutate(pred = mu_hat + b_i + b_u + b_y) %>%
    pull(pred)
  ##Save lambda
  return(RMSE(predicted_ratings_model_8, test$rating))
})
#plot lambda vs accuracy
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
##Calculate final prediction with optimum lambda
lambda<-lambdas[which.min(rmses)]

##Movie effect
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda))
##User effect
user_effect_reg <- train %>%
  left_join(movie_effect_reg, by="movieId") %>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - b_i- mu_hat)/(n()+lambda))
##Year effect
year_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  group_by(year)%>%
  summarize(b_y=sum(rating-mu_hat-b_i-b_u)/(n()+lambda))
##Prediction
predicted_rating_model8 <- test %>% 
  left_join(movie_effect_reg, by = "movieId") %>%
  left_join(user_effect_reg, by = "userId") %>%
  left_join(year_effect_reg, by = "year") %>%
  mutate(pred = mu_hat + b_i + b_u +b_y) %>%
  pull(pred)
##RMSE
RMSE_model8<- RMSE(predicted_rating_model8, test$rating)
##Save ad print value
RMSE_8<-data.frame(Method= "mean + movie effect + user_effect + aging_effect REGULARIZED", RMSE= RMSE_model8)
RMSE_results<-bind_rows(RMSE_1,RMSE_2,RMSE_6,RMSE_3,RMSE_7,RMSE_4,RMSE_8,RMSE_5)
knitr::kable(RMSE_results)

#-------------------------------------------------------------------
###MODEL 9: Consider movie effect, user effect, year effect and genre effect REGULARIZED
#-------------------------------------------------------------------

##First calculate lambda
lambdas<-seq(4,5,.1)
##Calculate rmses for all lambdas
rmses <- sapply(lambdas, function(l){
  #Movie effect
  movie_effect_reg <- train %>%
    group_by(movieId)%>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  ##User effect
  user_effect_reg <- train %>%
    left_join(movie_effect_reg, by="movieId") %>%
    group_by(userId)%>%
    summarize(b_u = sum(rating - b_i- mu_hat)/(n()+l))
  ###Year effect
  year_effect_reg<-train%>%
    left_join(movie_effect_reg, by='movieId') %>%
    left_join(user_effect_reg, by='userId') %>%
    group_by(year)%>%
    summarize(b_y=sum(rating-mu_hat-b_i-b_u)/(n()+l))
  ##Genre effect
  genre_effect_reg<-train%>%
    left_join(movie_effect_reg, by='movieId') %>%
    left_join(user_effect_reg, by='userId') %>%
    left_join(year_effect_reg, by='year') %>%
    group_by(genres)%>%
    summarize(b_g=sum(rating-mu_hat-b_i-b_u-b_y)/(n()+l))
  ##Predict values
  predicted_ratings_model_9 <- test %>% 
    left_join(movie_effect_reg, by = "movieId") %>%
    left_join(user_effect_reg, by = "userId") %>%
    left_join(year_effect_reg, by = "year") %>%
    left_join(genre_effect_reg, by = "genres") %>%
    mutate(pred = mu_hat + b_i + b_u + b_y + b_g) %>%
    pull(pred)
  #Return RMSES
  return(RMSE(predicted_ratings_model_9, test$rating))
})
#Plot lambda vs RMSES
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
##Select optimum lambda
min(rmses)

##FInal calculation

l=lambdas[which.min(rmses)]
##Movie effect
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+l))
##User effect
user_effect_reg <- train %>%
  left_join(movie_effect_reg, by="movieId") %>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - b_i- mu_hat)/(n()+l))
##Year effect
year_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  group_by(year)%>%
  summarize(b_y=sum(rating-mu_hat-b_i-b_u)/(n()+l))
##Genre effect
genre_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  left_join(year_effect_reg, by='year') %>%
  group_by(genres)%>%
  summarize(b_g=sum(rating-mu_hat-b_i-b_u-b_y)/(n()+l))
##Predictions
predicted_ratings_model_9 <- test %>% 
  left_join(movie_effect_reg, by = "movieId") %>%
  left_join(user_effect_reg, by = "userId") %>%
  left_join(year_effect_reg, by = "year") %>%
  left_join(genre_effect_reg, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_y + b_g) %>%
  pull(pred)

##Calculate RMSES
RMSE_model9<-RMSE(predicted_ratings_model_9, test$rating)
RMSE_9<-data.frame(Method= "mean + movie effect + user_effect + aging_effect + genre_effect  REGULARIZED", RMSE= RMSE_model9)
RMSE_results<-bind_rows(RMSE_1,RMSE_2,RMSE_6,RMSE_3,RMSE_7,RMSE_4,RMSE_8,RMSE_5,RMSE_9)

knitr::kable(RMSE_results)

#-------------------------------------------------------------------
###MODEL 10: Consider movie effect, user effect, year effect, rating date effect, genre effect and year of rating REGULARIZED
#-------------------------------------------------------------------

##First select optimum lambda      

lambdas<-seq(4.7,5,.1)
##Calculate rmses for all lambdas
rmses <- sapply(lambdas, function(l){
  ##Movie effect
  movie_effect_reg <- train %>%
    group_by(movieId)%>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  ##User effect
  user_effect_reg <- train %>%
    left_join(movie_effect_reg, by="movieId") %>%
    group_by(userId)%>%
    summarize(b_u = sum(rating - b_i- mu_hat)/(n()+l))
  ##Aging effect
  year_effect_reg<-train%>%
    left_join(movie_effect_reg, by='movieId') %>%
    left_join(user_effect_reg, by='userId') %>%
    group_by(year)%>%
    summarize(b_y=sum(rating-mu_hat-b_i-b_u)/(n()+l))
  ##Genre effect
  genre_effect_reg<-train%>%
    left_join(movie_effect_reg, by='movieId') %>%
    left_join(user_effect_reg, by='userId') %>%
    left_join(year_effect_reg, by='year') %>%
    group_by(genres)%>%
    summarize(b_g=sum(rating-mu_hat-b_i-b_u-b_y)/(n()+l))
  ##Date of rating effect
  daterating_effect_reg<-train%>%
    left_join(movie_effect_reg, by='movieId') %>%
    left_join(user_effect_reg, by='userId') %>%
    left_join(year_effect_reg, by='year') %>%
    left_join(genre_effect_reg, by = "genres") %>%
    group_by(yr_rate)%>%
    summarize(b_dr=sum(rating-mu_hat-b_i-b_u-b_y-b_g)/(n()+l))
  ##Prediction
  predicted_ratings_model_10 <- test %>% 
    left_join(movie_effect_reg, by = "movieId") %>%
    left_join(user_effect_reg, by = "userId") %>%
    left_join(year_effect_reg, by = "year") %>%
    left_join(genre_effect_reg, by = "genres") %>%
    left_join(daterating_effect_reg, by = "yr_rate") %>%
    mutate(pred = mu_hat + b_i + b_u + b_y + b_g + b_dr) %>%
    pull(pred)
  return(RMSE(predicted_ratings_model_10, test$rating))
})
#Plot lambda vs value and select optimum
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
min(rmses)

##Final calculation
l=lambdas[which.min(rmses)]
##Calculate Movie effect
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+l))
##Calculate user effect
user_effect_reg <- train %>%
  left_join(movie_effect_reg, by="movieId") %>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - b_i- mu_hat)/(n()+l))
#calculate year effect
year_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  group_by(year)%>%
  summarize(b_y=sum(rating-mu_hat-b_i-b_u)/(n()+l))
##calculate genre effect
genre_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  left_join(year_effect_reg, by='year') %>%
  group_by(genres)%>%
  summarize(b_g=sum(rating-mu_hat-b_i-b_u-b_y)/(n()+l))
##Calculate date of rating effect
daterating_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  left_join(year_effect_reg, by='year') %>%
  left_join(genre_effect_reg, by = "genres") %>%
  group_by(yr_rate)%>%
  summarize(b_dr=sum(rating-mu_hat-b_i-b_u-b_y-b_g)/(n()+l))
##calculate predictions
predicted_ratings_model_10 <- test %>% 
  left_join(movie_effect_reg, by = "movieId") %>%
  left_join(user_effect_reg, by = "userId") %>%
  left_join(year_effect_reg, by = "year") %>%
  left_join(genre_effect_reg, by = "genres") %>%
  left_join(daterating_effect_reg, by="yr_rate") %>%
  mutate(pred = mu_hat + b_i + b_u + b_y + b_g +b_dr) %>%
  pull(pred)
##Calculate RMSE and print values
RMSE_model10<-RMSE(predicted_ratings_model_10, test$rating)
RMSE_10<-data.frame(Method= "mean + movie effect + user_effect + aging_effect + genre_effect+date rating  REGULARIZED", RMSE= RMSE_model10)
RMSE_results<-bind_rows(RMSE_1,RMSE_2,RMSE_6,RMSE_3,RMSE_7,RMSE_4,RMSE_8,RMSE_5,RMSE_9,RMSE_10)
knitr::kable(RMSE_results)
#-------------------------------------------------------------------
## MODEL 11 Matrix factorization
#-------------------------------------------------------------------

##Set seed
set.seed(1974, sample.kind = "Rounding")
##Adjust formats of training and test sets for the model
train_data <-  with(train, data_memory(user_index = userId, 
                                       item_index = movieId, 
                                       rating     = rating))
test_data  <-  with(test,  data_memory(user_index = userId, 
                                       item_index = movieId, 
                                       rating     = rating))
# Create the model object
r <-  Reco()
# Train the algorithm  (with default parameters)
r$train(train_data)
##Calculate predicted data
predicted_ratings_model_11 <-  r$predict(test_data, out_memory())
##calculate RMSES for final value
RMSE_model11<-RMSE(test$rating, predicted_ratings_model_11)
RMSE_11<-data.frame(Method= "Matrix Factorization", RMSE= RMSE_model11)
RMSE_results<-bind_rows(RMSE_1,RMSE_2,RMSE_6,RMSE_3,RMSE_7,RMSE_4,RMSE_8,RMSE_5,RMSE_9,RMSE_10,RMSE_11)
##Print all results to select best model
knitr::kable(RMSE_results)

### So finally te model with lowest RMSE is:
knitr::kable(RMSE_results[which.min(RMSE_results$RMSE),])

#---------------------------------------------------------------
###Evaluating validation set in final model: MATRIX FACTORIZATION
#---------------------------------------------------------------

set.seed(1974, sample.kind = "Rounding")
r$train(train_data)
#Convert validation dataset in matrix format
valid  <-  with(validation,  data_memory(user_index = userId, 
                                         item_index = movieId, 
                                         rating     = rating))
##Predict values
predicted_ratings_model_11v <-  r$predict(valid, out_memory())
##Calculate RMSES for validated models
RMSE_model11v<-RMSE(validation$rating, predicted_ratings_model_11v)
RMSE_11v<-data.frame(Method= "Matrix Factorization", RMSE= RMSE_model11v)

#####################################################################
####################   SELECTED MODEL   ###########################
#####################################################################
#Print result of MATRIX FACTORIZATION in validation set to check if RMSES<0.86490
RMSE_11v%>%kable()
#####################################################################



###################################################################
## **4-Conclusion**
###################################################################

###Evaluating all models in Validation set just to see effect

#------------------------------------------------------------
### MODEL 1: assume same rating for all: Y_u,i= mu+ err_u,i
#------------------------------------------------------------

##Calculate mean
mu_hat<-mean(train$rating)
##Calculate RMSES assuming rating is mean for all movies
RMSE_model1v<-RMSE(validation$rating,mu_hat)
RMSE_1v<-data.frame(Method= " validation set in mean", RMSE= RMSE_model1v)
#------------------------------------------------------------
###MODEL 2: considering movie effect Y u,i= mu+b_i+err_u,i
#------------------------------------------------------------

##Calculate movie effect
movie_effect<-train%>%group_by(movieId)%>%summarize(b_i=mean(rating-mu_hat))
##predict values for validation set
predicted_rating_model2v <-validation %>% 
  left_join(movie_effect, by='movieId') %>%
  mutate(pred = mu_hat + b_i)%>%
  pull(pred)
##Calculate RMSE for validation set
RMSE_model2v<-RMSE(validation$rating,predicted_rating_model2v)
RMSE_2v<-data.frame(Method= " validation set in mean + movie effect", RMSE= RMSE_model2v)

#------------------------------------------------------------
###MODEL 3: Consider user effect Y u,i= mu+b_i + b_u + err_u,i
#------------------------------------------------------------

##Calculate user effect
user_effect<-train%>%group_by(userId)%>%
  left_join(movie_effect, by='movieId') %>%
  summarize(b_u=mean(rating-mu_hat-b_i))

##Calculate prediction for validation set
predicted_rating_model3v <-validation %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  mutate(pred = mu_hat + b_i +b_u)%>%
  pull(pred)
##Calculate RMSES 
RMSE_model3v<-RMSE(validation$rating,predicted_rating_model3v)
RMSE_3v<-data.frame(Method= " validation set in mean + movie effect + user_effect ", RMSE= RMSE_model3v)

#------------------------------------------------------------
###MODEL 4 Consider aging effect Y u,i= mu+b_i + b_u + b_y + err_u,i
#------------------------------------------------------------

##Calculate year effect
year_effect<-train%>%
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  group_by(year)%>%
  summarize(b_y=mean(rating-mu_hat-b_i-b_u))
###Calculate prediction for validation set
predicted_rating_model4v <- validation %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(year_effect, by='year') %>%
  mutate(pred = mu_hat + b_i +b_u+b_y)%>%
  pull(pred)
##Calculate RMSES
RMSE_model4v<-RMSE(validation$rating,predicted_rating_model4v)
RMSE_4v<-data.frame(Method= " validation set in mean + movie effect + user_effect + aging_effect", RMSE= RMSE_model4v)

#------------------------------------------------------------
###MODEL 5: Consider genre Y u,i= mu+b_i + b_u +b_y + b_g + err_u,i
#------------------------------------------------------------

##Calculate genre effect
genre_effect<-train%>%
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(year_effect, by='year') %>%
  group_by(genres)%>%
  summarize(b_g=mean(rating-mu_hat-b_i-b_u-b_y))
##Calculate prediction for validation set
predicted_rating_model5v <- validation %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(year_effect, by='year') %>%
  left_join(genre_effect, by='genres') %>%
  mutate(pred = mu_hat + b_i +b_u+b_y+b_g)%>%
  pull(pred)
##Calculate RMSES
RMSE_model5v<-RMSE(validation$rating,predicted_rating_model5v)
##Save values
RMSE_5v<-data.frame(Method= " validation set in mean + movie effect + user_effect + aging_effect + genre_effect", RMSE= RMSE_model5v)

#------------------------------------------------------------
##MODEL 6 REGULARIZED Consider movie effect
#------------------------------------------------------------

lambda<-1.6
#Calculate movie effect regularized
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda))
##Predict values with validation set
predicted_rating_model6v <- validation  %>% 
  left_join(movie_effect_reg, by = "movieId")%>%
  mutate(pred = mu_hat + b_i) %>%
  pull(pred)
##Calculate RMSES
RMSE_model6v<-RMSE(validation$rating,predicted_rating_model6v)
RMSE_6v<-data.frame(Method= " validation set in mean + movie effect  REGULARIZED", RMSE= RMSE_model6v)
#------------------------------------------------------------
##### MODEL 7: Consider user effect  REGULARIZED
#Y u,i= mu+b_i + b_u + err_u,i
#------------------------------------------------------------
lambda<-5

##Calculate movie effect regularized
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda))
##Calculate user effect regularized
user_effect_reg <- train %>%
  left_join(movie_effect_reg, by="movieId") %>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - b_i- mu_hat)/(n()+lambda))
#Predict values
predicted_rating_model7v <- validation %>% 
  left_join(movie_effect_reg, by = "movieId") %>%
  left_join(user_effect_reg, by = "userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
##Calculate RMSES
RMSE_model7v<- RMSE(validation$rating,predicted_rating_model7v)
RMSE_7v<-data.frame(Method= " validation set in mean + movie effect + user_effect  REGULARIZED", RMSE= RMSE_model7v)

#------------------------------------------------------------
### MODEL 8: REGULARIZED Consider user aging Y u,i= mu+b_i + b_u + 
#+ b_y + err_u,i
#------------------------------------------------------------
lambda<-4.6

##Calculate movie effect
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda))
##calculate user effect
user_effect_reg <- train %>%
  left_join(movie_effect_reg, by="movieId") %>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - b_i- mu_hat)/(n()+lambda))
##Calculate year effect
year_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  group_by(year)%>%
  summarize(b_y=sum(rating-mu_hat-b_i-b_u)/(n()+lambda))
##predict values
predicted_rating_model_8v <- validation %>% 
  left_join(movie_effect_reg, by = "movieId") %>%
  left_join(user_effect_reg, by = "userId") %>%
  left_join(year_effect_reg, by = "year") %>%
  mutate(pred = mu_hat + b_i + b_u +b_y) %>%
  pull(pred)
## CALCULATE RMSES
RMSE_model8v<- RMSE(predicted_rating_model_8v, validation$rating)
RMSE_8v<-data.frame(Method= " validation set in mean + movie effect + user_effect + aging_effect  REGULARIZED", RMSE= RMSE_model8v)

#------------------------------------------------------------
###MODEL 9: REGULARIZED Consider genre effect 
##Y u,i= mu+b_i + b_u + b_y+b_g+err_u,i
#------------------------------------------------------------
l=4.6
##Calculate movie effect
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+l))
##Calculate user effect
user_effect_reg <- train %>%
  left_join(movie_effect_reg, by="movieId") %>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - b_i- mu_hat)/(n()+l))
##Calculate year effect
year_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  group_by(year)%>%
  summarize(b_y=sum(rating-mu_hat-b_i-b_u)/(n()+l))
##Calculate genre effect
genre_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  left_join(year_effect_reg, by='year') %>%
  group_by(genres)%>%
  summarize(b_g=sum(rating-mu_hat-b_i-b_u-b_y)/(n()+l))
##Calculate prediction
predicted_ratings_model_9v <- validation %>% 
  left_join(movie_effect_reg, by = "movieId") %>%
  left_join(user_effect_reg, by = "userId") %>%
  left_join(year_effect_reg, by = "year") %>%
  left_join(genre_effect_reg, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_y + b_g) %>%
  pull(pred)
##Calculate rmses
RMSE_model9v<-RMSE(validation$rating,predicted_ratings_model_9v)
RMSE_9v<-data.frame(Method= " validation set in mean + movie effect + user_effect + aging_effect + genre_effect  REGULARIZED", RMSE= RMSE_model9v)

#------------------------------------------------------------
###MODEL 10 REGULARIZED consider date of rating
##Y u,i= mu+b_i + b_u + b_y+b_g +b_dr + err_u,i
#------------------------------------------------------------
l=4.8
##Calculate effect
movie_effect_reg <- train %>%
  group_by(movieId)%>%
  summarize(b_i = sum(rating - mu_hat)/(n()+l))
##calculate user effect
user_effect_reg <- train %>%
  left_join(movie_effect_reg, by="movieId") %>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - b_i- mu_hat)/(n()+l))
##calculate aging effect
year_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  group_by(year)%>%
  summarize(b_y=sum(rating-mu_hat-b_i-b_u)/(n()+l))
##calculate genre effect
genre_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  left_join(year_effect_reg, by='year') %>%
  group_by(genres)%>%
  summarize(b_g=sum(rating-mu_hat-b_i-b_u-b_y)/(n()+l))
##calculate date of rating effect
daterating_effect_reg<-train%>%
  left_join(movie_effect_reg, by='movieId') %>%
  left_join(user_effect_reg, by='userId') %>%
  left_join(year_effect_reg, by='year') %>%
  left_join(genre_effect_reg, by = "genres") %>%
  group_by(yr_rate)%>%
  summarize(b_dr=sum(rating-mu_hat-b_i-b_u-b_y-b_g)/(n()+l))
##calculate prediction
predicted_ratings_model_10v <- validation %>% 
  left_join(movie_effect_reg, by = "movieId") %>%
  left_join(user_effect_reg, by = "userId") %>%
  left_join(year_effect_reg, by = "year") %>%
  left_join(genre_effect_reg, by = "genres") %>%
  left_join(daterating_effect_reg, by='yr_rate') %>%
  mutate(pred = mu_hat + b_i + b_u + b_y + b_g +b_dr) %>%
  pull(pred)
##Calculate RMSE for model
RMSE_model10v<-RMSE(predicted_ratings_model_10v, validation$rating)
RMSE_10v<-data.frame(Method= " validation set in mean + movie effect + user_effect + aging_effect + genre_effect + date rating  REGULARIZED", RMSE= RMSE_model10v)

RMSE_resultsv<-bind_rows(RMSE_1v,RMSE_2v,RMSE_6v,RMSE_3v,RMSE_7v,
                  RMSE_4v,RMSE_8v,RMSE_5v,RMSE_9v,RMSE_10v,RMSE_11v)
##print all values
knitr::kable(RMSE_resultsv)

###Even we selected just matrix factorization as answer, 
#MODELS 11 and 10 also were able to achieve RMSE<0.8649. 
