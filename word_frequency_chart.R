# Word Frequency Chart

library(dplyr)
library(tidytext)
library(ggplot2)
library(readxl)

data(stop_words)

# Read data

# install.packages("googlesheets4")
# once installed
library(googlesheets4) 

# running read_sheet() will ask you to authenticate with Google first
dataset <- read_sheet("https://docs.google.com/spreadsheets/d/1RqNe4ETrhK_t9cSQbXqCdcdd82ZLtc0ZFm4Wz0wqPvQ/edit#gid=1390658809")

names(dataset)

dataset_education <- dataset %>% 
    filter(`Ed or No Ed`== "Yes"|`Ed or No Ed`=="Yes?") %>% 
    select(`NGO Name and General Website`,`Ed Interventions`)

# Method 1: Unigrams ----
# Remove stopwords from comments: 
comments_df <- dataset_education %>%
    select(`Ed Interventions`) %>%
    unnest_tokens(word, `Ed Interventions`) %>%
    anti_join(stop_words)

word_freq <- comments_df %>%
    
    # singularize(comments_df$word) %>% 
    count(word, sort = TRUE)

# Word Frequency Plot


library(dplyr)
library(ggplot2)

word_freq_filtered <- word_freq %>% 
    # filter(!word %in% c("the", "and", "of", "loans", "loan", "vfk", "vfl")) %>%
    filter( n > 2)

word_freq_filtered %>% 
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_col(fill = "darkorange1") +
    coord_flip() +
    labs(title = "Most Frequent Words", x = "Word", y = "Frequency") +
    theme_minimal()


#### Method 2: Trying bi-grams ----

library(tidytext)
library(tidyverse)

dataset_education %>%
    select(`Ed Interventions`) %>%
    unnest_tokens(word, `Ed Interventions`, token = "ngrams", n = 2) %>% 
    separate(word, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    unite(word,word1, word2, sep = " ") %>% 
    count(word, sort = TRUE) %>% 
    slice(1:10) %>% 
    ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
    theme_minimal() +
    coord_flip() +
    labs(title = "Top Bigrams of Education Interventions",
         subtitle = "Compiled by Doris Suzuki Esmerio and Kelsey Daniels",
         caption = "Data Source: Accord Network Members's websites")
