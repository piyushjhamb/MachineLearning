library(sentimentr)
library(ggplot2)
library(dplyr)
library(magrittr)

mytext <- c(
  'do you like it?  But I hate really bad dogs',
  'I am the really best friend.',
  'Do you really like it?  I\'m not a fan'
)

mytext <- get_sentences(mytext)

s <- sentiment(mytext)


data(presidential_debates_2012)

presidential_debates_2012 %>%
  dplyr::mutate(dialogue_split = get_sentences(dialogue)) %$%
  sentiment_by(dialogue_split, list(person, time))


cannon_reviews %>%
  mutate(review_split = get_sentences(text)) %$%
  sentiment(review_split)
 
# Extract sentiment words:
'My life has become terrible since I met you and lost money' %>% extract_sentiment_terms()



# Canon review Text highlights, GREEN + and Pink -
cannon_reviews %>%
  filter(number %in% sample(unique(number), 3)) %>%
  mutate(review = get_sentences(text)) %$%
  sentiment_by(review, number) %>%
  highlight()



## Function to get sentiment score of text 
df<- data.frame(cannon_reviews)

GetSentiments <- function(df){
  
  df %>%
    mutate(reviewSentence = get_sentences(df$text)) %>%
    sentiment_by(reviewSentence, df$number) %>%
    highlights()
    
}

GetSentiments(df)
