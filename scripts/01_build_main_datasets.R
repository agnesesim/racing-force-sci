library(tidyverse)
library(fs)

patents <- read_csv("data/data_prepared_python.csv")

patents <- patents %>%
  mutate(has_claims = !is.na(claims)) 

write.csv(patents %>% 
            filter(has_claims == TRUE),
          "data/main_dataset.csv")
