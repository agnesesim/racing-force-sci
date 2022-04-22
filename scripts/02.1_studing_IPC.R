library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(fs)


##########################################################
get_text_clean <- function(data){
  data_text <- data %>%
    mutate(patent_id = substr(filename, 0, unlist(gregexpr(pattern ='.txt',filename)) -1 )) %>%
    mutate(text = paste(title, abstract, claims, sep=" ")) %>%
    mutate(text = tolower(text)) %>% 
    mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
    select(patent_id, ipc_classes, text)
  
  return(data_text)
}

extra_words = c("claims", "claim", "method", "system")
extra_words <- as_tibble(extra_words) %>%
  rename( word = value)

get_words <- function(data) {
  tidy_data <- data %>%
    unnest_tokens(word, text)
  
  tidy_data <- tidy_data %>%
    filter(str_detect(word, "(?!^\\d+$)^.+$"))
  
  tidy_data <- tidy_data %>%
    anti_join(stop_words, by="word") %>%
    anti_join(extra_words, by="word")
  
  tidy_data <- tidy_data %>%
    mutate(word = wordStem(word)) # stemming

  return (tidy_data)
}

get_word_frquency <- function(data) {
  tidy_data <- get_words(data) 
  
  word_freq <- tidy_data %>%
    count(word) %>% 
    mutate(freq = n / sum(n)) %>% 
    arrange(desc(n))
  
  return (word_freq)
}

get_word_cloud <- function(data) {
  tidy_data <- get_words(data) 
  
  tidy_data %>%
    count(word) %>% 
    with(wordcloud(word, n, max.words = 100))
}

get_list_frequencies <- function(data, list_words){
  data %>% 
    mutate(count = sum(str_count(text, list_words))) %>%
    mutate(freq = (count/str_count(text, "\\s"))*100) %>% 
    arrange(desc(freq))
}

get_list_frequencies_imp <- function(data, list_words, list_imp){
  data %>% 
    mutate(count = sum(str_count(text, list_words)*list_imp)) %>%
    mutate(freq = (count/str_count(text, "\\s"))*100) %>% 
    arrange(desc(freq))
}

##########################################################

patents <- read_csv("data/main_dataset.csv")
patents_text <-get_text_clean(patents)

get_word_cloud(patents_text %>% filter(str_detect(ipc_classes, "^A") | str_detect(ipc_classes, ',A')))

frequency <- get_word_frquency(patents_text %>% filter(str_detect(ipc_classes, "^H")))

frequency %>% filter(str_detect(word, "imag."))

patents_text %>% filter(str_detect(ipc_classes, "^D")) %>%
  mutate(count = str_count(text, "imag.*")) %>%
  mutate(freq = (count/str_count(text, "\\s"))*100) %>% 
  arrange(desc(freq))

patents_text %>% filter(str_detect(ipc_classes, "^D") | str_detect(ipc_classes, ",D"))  %>%
  mutate(n_image = str_count(text,"imag.")) %>%
  mutate(n_cv = str_count(text,"computer vision")) %>%
  mutate(count = n_image + n_cv) %>%
  mutate(freq = (count/str_count(text, "\\s"))*100) %>% 
  arrange(desc(n_cv))

patents_text %>% filter(str_detect(ipc_classes, "^H"))  %>%
  mutate(n_elab = str_count(text,"imag. elaborat.")) %>%
  mutate(n_proc = str_count(text,"imag. process.")) %>%
  mutate(n_analy = str_count(text,"imag. analys.")) %>%
  mutate(n_cv = str_count(text,"computer vision")) %>%
  mutate(count = n_elab + n_proc + n_analy + n_cv) %>%
  mutate(freq = (count/str_count(text, "\\s"))*100) %>% 
  arrange(desc(freq))

patents_text %>% filter(str_detect(ipc_classes, "^D") | str_detect(ipc_classes, ",D"))  %>%
  mutate(n_image = str_count(text,"imag.")) %>%
  mutate(n_cv = str_count(text,"computer vision")) %>%
  mutate(count = n_image + n_cv) %>%
  mutate(freq = (count/str_count(text, "\\s"))*100) %>% 
  arrange(desc(n_cv))


  
get_list_frequencies(patents_text)

