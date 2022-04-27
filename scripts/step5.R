get_list_frequencies <- function(data, list_words){
  data %>% 
    mutate(count = sum(str_count(text, list_words))) %>%
    mutate(totwords = str_count(text, "\\s")+1) %>%
    mutate(freq = (count/totwords)*100) %>% 
    arrange(desc(freq))
}

get_list_frequencies_imp <- function(data, list_words, list_imp){
  data %>% 
    mutate(count = sum(str_count(text, list_words)*list_imp)) %>%
    mutate(freq = (count/str_count(text, "\\s"))*100) %>% 
    arrange(desc(freq))
}


result_img <- get_list_frequencies(patents_text, image_words)
result_tech <- get_list_frequencies(patents_text, tech_words)
result_motor <- get_list_frequencies(patents_text, motor_words)

