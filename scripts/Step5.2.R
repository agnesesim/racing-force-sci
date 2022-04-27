library(rebus)

patents <- read_csv("data/main_dataset.csv")
patents_text <- get_text_clean(patents)

patents_words_vectors <- patents_text

image_words_regex <- BOUNDARY %R% image_words %R% BOUNDARY
patents_words_vectors <- patents_words_vectors %>% 
  mutate(image_matches = str_match_all(text, image_words_regex)) %>%
  mutate(count = str_count(text, image_words))


tech_words_regex <- BOUNDARY %R% tech_words %R% BOUNDARY
patents_words_vectors <- patents_words_vectors %>% 
  mutate(tech_matches = str_match_all(text, tech_words_regex))

motor_words_regex <- BOUNDARY %R% motor_words %R% BOUNDARY
patents_words_vectors <- patents_words_vectors %>% 
  mutate(motor_matches = str_match_all(text, motor_words_regex))



