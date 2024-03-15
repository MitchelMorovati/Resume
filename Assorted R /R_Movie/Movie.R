rm(list=ls())  

movie <- read.table('data_movie.csv', header=TRUE, sep=",")

movie
#Construct a frequency distribution table of movie genres by mpaa rating in your 
#R script. Each row is a movie genre and each column is a mpaa rating.
freq_table <-table(movie$genre,movie$mpaa)

freq_table 
#1pt) Calculate the mean, median, and standard deviation of total box office revenue in 
#the US (in 1,000,000). Round your answer to two decimal places. 


box_mean <- mean(movie$us_total_gross)

box_median <- median(movie$us_total_gross)

box_sd <- sd(movie$us_total_gross)

#Calculate the z-score for total box office revenue in the US (in 1,000,000). Provide 
#the following summary statistics of the z-score. Round your answer to three decimal 
#places.

zscore_total_box <- (movie$us_total_gross - box_mean)/box_sd

summary(zscore_total_box)
#) Create a subset of data such that the total box office revenue in the US is within 
#2 standard deviation from the mean of total box office revenue. The number of 
#observations in this subset

min_box2 <- box_mean - 2*box_sd
max_box2 <- box_mean + 2*box_sd
movie_sub2 <- movie[movie$us_total_gross>=min_box2 & movie$us_total_gross<=max_box2,]

min_box3 <- box_mean - 3*box_sd
max_box3 <- box_mean + 3*box_sd
movie_sub3 <- movie[movie$us_total_gross>=min_box3 & movie$us_total_gross<=max_box3,]
221/226

#create 3 boxplots and attach them here: 
#  (a) the total box office revenue in the US (in 1,000,000); 
#(b) the total box office revenue in the US (in 1,000,000) by genre; 
#(c) the average user rating of the movie on IMDB
boxplot(movie$us_total_gross, data = movie) 

boxplot(movie$us_total_gross ~ movie$genre, data = movie)

boxplot(movie$user_rating, data = movie) 

hist(movie$us_total_gross)

cor(movie$us_total_gross,movie$open_theater)

plot(movie$us_total_gross,movie$open_theater)

