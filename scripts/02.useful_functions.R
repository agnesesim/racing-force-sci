library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(fs)

# funzione per la fusione e la pulizia del testo 
get_text_clean <- function(data){
  data_text <- data %>%
    # riunisco tutto il testo del patent sotto un solo attributo 'text'
    mutate(text = paste(title, abstract, claims, sep=" ")) %>%
    # trasformo tutto il testo in minuscolo
    mutate(text = tolower(text)) %>% 
    # rimuovo la punteggiatura
    #mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
    # estraggo solo le 3 colonne interessanti per la ricerca
    select(patent_id, ipc_classes, text)
  
  return(data_text)
}

# parole molto frequenti nei patent indipendentemente dall'invenzione
words_to_delete = c("claims", "claim", "method", "system", "comprise", "data")
words_to_delete <- as_tibble(words_to_delete) %>%
  rename( word = value)

# restituisce la lista delle parole presenti nei patents
get_words <- function(data) {
  tidy_data <- data %>%
    unnest_tokens(word, text)
  
  # rimuove i numeri 
  tidy_data <- tidy_data %>%
    filter(str_detect(word, "(?!^\\d+$)^.+$"))
  
  # rimuove le stop words e le parole comuni ai patent
  tidy_data <- tidy_data %>%
    anti_join(stop_words, by="word") %>%
    anti_join(words_to_delete, by="word")
  
  # stemming
  tidy_data <- tidy_data %>%
    mutate(word = wordStem(word))

  return (tidy_data)
}

# restituisce la lista delle parole presenti in data in ordine
# dalla più frequente alla meno frequente
get_word_frquency <- function(data) {
  tidy_data <- get_words(data) 
  
  word_freq <- tidy_data %>%
    count(word) %>% 
    mutate(freq = n / sum(n)) %>% 
    arrange(desc(n))
  
  return (word_freq)
}

# produce un wordcloud con le 50 parole più frequenti in data
get_word_cloud <- function(data) {
  tidy_data <- get_words(data) 
  
  tidy_data %>%
    count(word) %>% 
    with(wordcloud(word, n, max.words = 50))
}

# produce un plot con le 20 parole più frequenti
get_top_words <- function(data){
  tidy_data <- get_words(data) 
  
  tidy_data %>%
    count(word, sort = TRUE) %>%
    top_n(20) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme_bw()
}



#########################

filter_founds <- function(data, frequence){
  found <- data %>%
    filter(freq>frequence) 
  return(found)
}

