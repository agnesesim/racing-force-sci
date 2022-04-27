library(tidyverse)
library(fs)

patents <- read_csv("data/data_prepared_python.csv")

write.csv(patents %>% 
            filter(!is.na(claims)),
          "data/main_dataset.csv")
