library(tidyverse)
library(tidytext)
library(fs)

patents <- read_csv("data/main_dataset.csv")

patents_text <- patents %>%
  mutate(patent_id = substr(filename, 0, unlist(gregexpr(pattern ='.txt',filename)) -1 )) %>%
  mutate(text = paste(title, abstract, claims, sep=" ")) %>%
  select(patent_id, ipc_classes, text)

patents_H04N <- patents_text %>% filter(str_detect(ipc_classes, "H04N"))
patents_G06T <- patents_text %>% filter(str_detect(ipc_classes, "G06T"))
patents_G06F <- patents_text %>% filter(str_detect(ipc_classes, "G06F"))
patents_G06V <- patents_text %>% filter(str_detect(ipc_classes, "G06V"))
patents_G06K <- patents_text %>% filter(str_detect(ipc_classes, "G06K"))
patents_A63B <- patents_text %>% filter(str_detect(ipc_classes, "A63B"))
patents_A42B <- patents_text %>% filter(str_detect(ipc_classes, "A42B"))

##########################################

t_patents_H04N <- patents_H04N %>%
  unnest_tokens(word, text)

t_patents_H04N <- t_patents_H04N %>%
  anti_join(stop_words)

t_patents_H04N %>%
  count(word, sort = TRUE)

t_patents_H04N %>%
  count(word, sort = TRUE) %>%
  filter(n > 60000) %>%
  # reorder levels of factor word wrt n
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()

 ##########################################

t_patents_G06T <- patents_G06T %>%
  unnest_tokens(word, text)

t_patents_G06T <- t_patents_G06T %>%
  anti_join(stop_words)

t_patents_G06T %>%
  count(word, sort = TRUE)

t_patents_G06T %>%
  count(word, sort = TRUE) %>%
  filter(n > 100000) %>%
  # reorder levels of factor word wrt n
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()

##########################################

t_patents_G06F <- patents_G06F %>%
  unnest_tokens(word, text)

t_patents_G06F <- t_patents_G06F %>%
  anti_join(stop_words)

t_patents_G06F %>%
  count(word, sort = TRUE)

t_patents_G06F %>%
  count(word, sort = TRUE) %>%
  filter(n > 300000) %>%
  # reorder levels of factor word wrt n
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()

##########################################

t_patents_G06V <- patents_G06V %>%
  unnest_tokens(word, text)

t_patents_G06V <- t_patents_G06V %>%
  anti_join(stop_words)

t_patents_G06V %>%
  count(word, sort = TRUE)

t_patents_G06V %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  # reorder levels of factor word wrt n
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()

##########################################

t_patents_G06K <- patents_G06K %>%
  unnest_tokens(word, text)

t_patents_G06K <- t_patents_G06K %>%
  anti_join(stop_words)

t_patents_G06K %>%
  count(word, sort = TRUE)

t_patents_G06K %>%
  count(word, sort = TRUE) %>%
  filter(n > 150000) %>%
  # reorder levels of factor word wrt n
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()

##########################################

t_patents_A63B <- patents_A63B %>%
  unnest_tokens(word, text)

t_patents_A63B <- t_patents_A63B %>%
  anti_join(stop_words)

t_patents_A63B %>%
  count(word, sort = TRUE)

t_patents_A63B %>%
  count(word, sort = TRUE) %>%
  filter(n > 3000) %>%
  # reorder levels of factor word wrt n
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()

##########################################

patents_A42B <- patents_A42B %>%
  unnest_tokens(word, text)

patents_A42B <- patents_A42B %>%
  filter(str_detect(word, "(?!^\\d+$)^.+$"))

patents_A42B <- patents_A42B %>%
  anti_join(stop_words) %>%
  anti_join(extra_words)

patents_A42B <- patents_A42B %>%
  mutate(word = wordStem(word)) # stemming

patents_A42B %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  # reorder levels of factor word wrt n
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()



