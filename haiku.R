library(newsanchor)
library(tidytext)
library(gutenbergr)
library(dplyr)
library(quanteda)
library(textdata)
library(rapportools)

# In order to add your key to your environment file, you can use the function edit_r_environ from the usethis package
# NEWS_API_KEY="yourkeygoeshere"

#get sources. run for information only
#get_sources()

##create data based on search term(s)##
# search_term is any string
# category_term is from terms_category
# sources_term is from terms_sources
# country_term is from terms_country
# language_term is from terms_language
search_term <- "trump"
category_term <- "entertainment"
sources_term <- "bbc-news"
country_term <- "gb"
language_term <- "en"

##get some articles and content
#h_lines <- get_everything(query = search_term)
#h_lines <- get_everything(query = search_term, sources = sources_term)
#h_lines <- get_headlines(category = category_term)
#h_lines <- get_headlines(country = country_term)
h_lines <- get_headlines(query = search_term)
#is.empty(h_lines)

#parse content
headlines1 <- as.data.frame(h_lines[2])

#put content into a data table as "text"
hgwells <- select(headlines1, results_df.id, "text"=results_df.content)

#get a tidy data frame of the words, excluding stop words
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#get a list of the word count
hgwells_count1 <- tidy_hgwells %>%
  count(word, sort = TRUE)

#get sentiment analysis, not currently being used below
hgwells_count <- hgwells_count1 %>% 
  left_join(get_sentiments("afinn"))


names(hgwells_count)

#get the syllables for each word
hgwells_count$syllable <- nsyllable(hgwells_count$word)

#filter out words that are less than 2 characters
hgwells_sub <- subset(hgwells_count, nchar(word) > 2)

#create haiku based on syllables 5-7-5
#this should be randomized to create interesting things
#possible sentiment analysis to get contrasting terms
#need to fix to get exact syllable match
#use of cumsum function works but may not be ideal approach

#get random sample for words where syllable is not na and use is >5
words1 <- subset(hgwells_sub, !is.na(syllable) & n > 1)

#get random sample of 7 words for verse1
i = 0
while(i != 5){
  temp <- words1[sample(nrow(words1), 7), ]
  temp$syllable_sum <- cumsum(temp$syllable)
  if(5 %in% temp$syllable_sum){
    i = 5
    verse1 <- subset(temp, syllable_sum <=5)
  }
}

#get random sample of 7 words for verse2
i = 0
while(i != 7){
  words2 <- subset(words1, !(word %in% verse1$word))
  temp <- words2[sample(nrow(words2), 7), ]
  temp$syllable_sum <- cumsum(temp$syllable)
  if(7 %in% temp$syllable_sum){
    i = 7
    verse2 <- subset(temp, syllable_sum <=7)
  }
}

#get random sample of 7 words for verse3
i = 0
while(i != 5){
  words3 <- subset(words2, !(word %in% verse2$word))
  temp <- words3[sample(nrow(words3), 7), ]
  temp$syllable_sum <- cumsum(temp$syllable)
  if(5 %in% temp$syllable_sum){
    i = 5
    verse3 <- subset(temp, syllable_sum <=5)
  }
}

#get haiku output
print(verse1$word)
print(verse2$word)
print(verse3$word)
