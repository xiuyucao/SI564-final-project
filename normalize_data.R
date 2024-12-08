## Normalize the data into tables for importing into the database
library(tidyverse)


## ----------------------------- Tool Functions ----------------------------- ##
## function for converting box office string into numeric value
get_numeric_boxoffice <- function(boxoffice.string) {
  length <- nchar(boxoffice.string)
  last_char <- substr(boxoffice.string, length, length)
  
  # Determine the numeric part of the string
  number <- ifelse(last_char %in% c('M', 'K'),
                   as.numeric(substr(boxoffice.string, 2, length - 1)),
                   as.numeric(substr(boxoffice.string, 2, length)))
  
  # Determine the magnitude based on the last character
  magnitude <- case_when(last_char == "M" ~ 1e6,
                         last_char == "K" ~ 1e3,
                         TRUE ~ 1)
  
  return(number * magnitude)
}


## ----------------------------- Data Read-in ----------------------------- ##
movies.raw <- read_csv('data/rotten_tomatoes_movies.csv')
reviews.raw <- read_csv('data/rotten_tomatoes_movie_reviews.csv')


## -------------------- Clean and Normalize Movies Data -------------------- ##
## clean the movies
movies <- movies.raw %>%
  select(-writer, -soundMix) %>%  # remove the writer and sound mix columns
  filter(!is.na(genre),  # filter out rows without information in these columns
         !is.na(releaseDateTheaters),
         !is.na(boxOffice),
         !is.na(distributor)) %>%
  distinct() %>%  # some rows have duplicates, remove the duplicates
  mutate(boxOffice=get_numeric_boxoffice(boxOffice))  # convert box office string into numeric value


## get director table
director <- movies %>%
  select(director) %>%
  distinct() %>%
  na.omit() %>%
  mutate(director_id = row_number()) %>%
  select(director_id, director) %>% 
  rename(name = director)


## get distributer table
distributer <- movies %>%
  select(distributor) %>%
  distinct() %>%
  na.omit() %>%
  mutate(distributer_id = row_number()) %>%
  select(distributer_id, distributor) %>%
  rename(name = distributor)


## get movie table
movie <- movies %>%
  select(id, title, rating, ratingContents, 
         releaseDateTheaters, releaseDateStreaming, 
         runtimeMinutes, originalLanguage, director, distributor, 
         boxOffice, audienceScore, tomatoMeter) %>%
  inner_join(director, by=c('director'='name')) %>%
  inner_join(distributer, by=c('distributor'='name')) %>%
  select(id, title, rating, ratingContents,
         releaseDateTheaters, releaseDateStreaming,
         runtimeMinutes, originalLanguage, director_id, distributer_id,
         boxOffice, audienceScore, tomatoMeter)


## get genre table
movie2keep <- select(movie, id)

movie_genrename <- movies %>%
  inner_join(movie2keep, by='id') %>%
  select(id, genre) %>%
  separate_rows(genre, sep=', ') %>%  # separate by comma and blank space
  rename(movie_id=id) %>%
  mutate(id=row_number())

genre <- movie_genrename %>%
  select(genre) %>%
  distinct() %>%
  mutate(id = row_number()) %>%
  select(id, genre) %>%
  rename(name = genre)

movie_genre <- movie_genrename %>%
  select(-id) %>%
  inner_join(genre, by=c('genre'='name')) %>%
  rename(genre_id = id) %>%
  mutate(id = row_number()) %>%
  select(id, movie_id, genre_id)
  
  
## -------------------- Clean and Normalize Reviews Data -------------------- ##
movies2review <- select(movie, id)

reviews <- reviews.raw %>%
  inner_join(movies2review, by='id') %>%
  select(-originalScore) %>%
  group_by(id) %>%
  slice(1:5) %>%  # each movie leave up to 5 records
  ungroup()

## get critic table
is_top_critic <- reviews %>%
  select(criticName, isTopCritic) %>%
  group_by(criticName) %>%
  summarize(most_frequent = ifelse(sum(isTopCritic, na.rm = TRUE) >= n()/2, TRUE, FALSE)) %>%
  distinct() %>%
  na.omit()

critic <- reviews %>%
  select(criticName) %>%
  distinct() %>%
  na.omit() %>%
  mutate(critic_id = row_number()) %>%
  select(critic_id, criticName) %>%
  left_join(is_top_critic, by='criticName') %>%
  rename(name = criticName, is_top_critic=most_frequent)

## get review table
review <- reviews %>%
  inner_join(critic, by=c('criticName'='name')) %>%
  select(reviewId, id, creationDate, critic_id, reviewState, reviewText, reviewUrl) %>%
  rename(movie_id = id) %>%
  distinct()


## ------------------------------ Write tables ------------------------------ ##
write_csv(director, 'data/table_director.csv')
write_csv(distributer, 'data/table_distributer.csv')
write_csv(movie, 'data/table_movie.csv')
write_csv(genre, 'data/table_genre.csv')
write_csv(movie_genre, 'data/table_movie_genre.csv')

write_csv(critic, 'data/table_critic.csv')
write_csv(review, 'data/table_review.csv')


## check the output tables
director <- read_csv('data/table_director.csv')
distributer <- read_csv('data/table_distributer.csv')
movie <- read_csv('data/table_movie.csv')
genre <- read_csv('data/table_genre.csv')
movie_genre <- read_csv('data/table_movie_genre.csv')
critic <- read_csv('data/table_critic.csv')
review <- read_csv('data/table_review.csv')
