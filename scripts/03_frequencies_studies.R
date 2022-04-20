library(tidyverse)
library(tidytext)
library(fs)

patents <- read_csv("data/main_dataset.csv")

get_patents_text <- patents %>%
  mutate(patent_id = substr(filename, 0, unlist(gregexpr(pattern ='.txt',filename)) -1 )) %>%
  mutate(text = paste(title, abstract, claims, sep=" ")) %>%
  select(patent_id, text)

tidy_patents <- get_patents_text %>%
  unnest_tokens(word, text)

