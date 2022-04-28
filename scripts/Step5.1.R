
patents <- read_csv("data/main_dataset.csv")
patents_text <- get_text_clean(patents)
patents_words_vectors <- patents_text 

patents_words_vectors <- patents_words_vectors %>%
  filter((str_detect(ipc_classes, '^A[64]2B')
          | str_detect(ipc_classes, ',A[64]2B')
          | str_detect(ipc_classes, '^A63K')
          | str_detect(ipc_classes, ',A63K')
          | str_detect(ipc_classes, '^B60')
          | str_detect(ipc_classes, ',B60')
          | str_detect(ipc_classes, '^B62D')
          | str_detect(ipc_classes, ',B62D')
          | str_detect(ipc_classes, '^G03')
          | str_detect(ipc_classes, ',G03')
          | str_detect(ipc_classes, '^G06[^CDMQ]')
          | str_detect(ipc_classes, ',G06[^CDMQ]')
          | str_detect(ipc_classes, '^H04')
          | str_detect(ipc_classes, ',H04')) == TRUE) %>%
  mutate(totwords = str_count(text, "\\s")+1) 
 
patents_words_vectors <- patents_words_vectors %>% 
  mutate(image_count = str_count(text, image_words))%>%
  mutate(image_freq = (image_count/totwords)*100) 

patents_words_vectors <- patents_words_vectors %>% 
  mutate(tech_count = str_count(text, tech_words))%>%
  mutate(tech_freq = (tech_count/totwords)*100) 

patents_words_vectors <- patents_words_vectors %>% 
  mutate(motor_count = str_count(text, motor_words))%>%
  mutate(motor_freq = (motor_count/totwords)*100) 

filter_founds <- function(data, freq){
  found <- data %>%
    filter(image_freq>freq | tech_freq>freq | motor_freq>freq) %>% 
    select(patent_id)
  return(found)
}

patents_foundInOr <- filter_founds(patents_words_vectors, 0)
write_csv(patents_foundInOr,"patents_foundInOr.csv")

patents_foundInAnd <- patents_words_vectors %>%
  filter(image_count>0 & tech_count>0 & motor_count>0) %>%
  select(patent_id)
write_csv(patents_foundInOr,"patents_foundInAnd.csv")
