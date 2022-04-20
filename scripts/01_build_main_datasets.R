library(tidyverse)
library(fs)

patents <- read_csv("data/patent_complete.csv")

patents <- patents %>%
  mutate(has_claims = !is.na(claims)) 

write.csv(patents %>% 
            filter(has_claims == TRUE),
          "data/main_dataset.csv")

#commento per provare git