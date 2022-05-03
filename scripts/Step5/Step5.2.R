library(rebus)

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


image_words_regex <- BOUNDARY %R% image_words %R% BOUNDARY
patents_words_vectors <- patents_words_vectors %>% 
  mutate(image_matches = str_match_all(text, image_words_regex)) %>%
  mutate(image_count = lengths(image_matches)) %>%
  mutate(image_freq = (image_count/totwords)*100) 

tech_words_regex <- BOUNDARY %R% tech_words %R% BOUNDARY
patents_words_vectors <- patents_words_vectors %>% 
  mutate(tech_matches = str_match_all(text, tech_words_regex)) %>%
  mutate(tech_count  = lengths(tech_matches)) %>%
  mutate(tech_freq = (tech_count/totwords)*100)
  #mutate(tech_count = str_count(text, tech_words))

motor_words_regex <- BOUNDARY %R% motor_words %R% BOUNDARY
patents_words_vectors <- patents_words_vectors %>% 
  mutate(motor_matches = str_match_all(text, motor_words_regex)) %>%
  mutate(motor_count  = lengths(motor_matches)) %>%
  mutate(motor_freq = (motor_count/totwords)*100)
  #mutate(motor_count = str_count(text, motor_words))



patents_found <- patents_words_vectors %>%
  filter(image_count>0) #& tech_count>0 & motor_count>0) 
  #select(patent_id)

filter_founds <- function(data, freq){
  found <- data %>%
    filter(image_freq>freq | tech_freq>freq | motor_freq>freq) 
  return(found)
}

patents_found <- filter_founds(patents_words_vectors, 0)

patents_found <- patents_words_vectors %>%
  filter(image_freq>0.5 | tech_freq>0.5 | motor_freq>0.5) 
#select(patent_id)

write_csv(patents_found,"patentInAnd.csv")


# plot_df <-patents_words_vectors %>%
#   filter(image_freq>0)
# d <- density(plot_df$image_freq) # returns the density data
# plot(d) # plots the results
