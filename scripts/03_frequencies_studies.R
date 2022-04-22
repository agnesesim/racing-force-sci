library(tidyverse)
library(tidytext)
library(wordcloud)
library(SnowballC)
library(fs)

patents <- read_csv("data/main_dataset.csv")

patents_text <- patents %>%
  mutate(patent_id = substr(filename, 0, unlist(gregexpr(pattern ='.txt',filename)) -1 )) %>%
  #mutate(text = paste(title, abstract, claims, sep=" ")) %>%
  mutate(text = title) %>%
  select(patent_id, ipc_classes, text)

tidy_patents <- patents_text %>%
  unnest_tokens(word, text)

tidy_patents <- tidy_patents %>%
  filter(str_detect(word, "(?!^\\d+$)^.+$"))

extra_words = c("claims", "claim", "method", "methods", "system")
extra_words <- as_tibble(extra_words) %>%
  rename( word = value)

tidy_patents <- tidy_patents %>%
  anti_join(stop_words) %>%
  anti_join(extra_words)

tidy_patents <- tidy_patents %>%
  mutate(word = wordStem(word)) # stemming

tidy_patents %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

