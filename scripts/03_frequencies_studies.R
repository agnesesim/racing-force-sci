library(tidyverse)
library(tidytext)
library(wordcloud)
library(SnowballC)
library(fs)

patents <- read_csv("data/main_dataset.csv")
patents_text <- get_text_clean(patents)

get_word_cloud(patents_text %>% 
                 filter(str_detect(ipc_classes, "^H") | str_detect(ipc_classes, ',H')))

get_word_frquency(patents_text %>% 
                    filter(str_detect(ipc_classes, "^H") | str_detect(ipc_classes, ',H')))

frequency <- get_word_frquency(patents_text)
frequency %>% filter(str_detect(word, "imag."))

get_top_words(patents_text)

############################# prove ############################# 

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



