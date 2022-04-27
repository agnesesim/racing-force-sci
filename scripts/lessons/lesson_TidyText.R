library(tidyverse)
library(tidytext)
library(fs)

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text_df <- data_frame(line = 1:4, text = text)
unnest_tokens(tbl = text_df, output = word, input = text)

library(janeaustenr)
library(stringr)
austen_books()

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(
           str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  # reorder levels of factor word wrt n
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()

library(wordcloud)
tidy_books %>%
  count(word) %>%
  # evaluate an R expression in an environment constructed from data
  with(wordcloud(word, n, max.words = 100))

library(SnowballC)
tidy_books <- tidy_books %>%
  mutate(word = wordStem(word)) # stemming

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()


austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text,
                token = "regex",
                pattern = "(Chapter|CHAPTER) [\\dIVXLC]{1,8}") %>%
  ungroup()

austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n()) %>%
  arrange(-chapters)
