library(tidyverse) 
library(fs)

patents <- read_csv("data/data_prepared_python.csv")

patents <- patents %>% 
  mutate( is_a = str_detect(ipc_classes, '^A') | str_detect(ipc_classes, ',A')) %>% 
  mutate( is_b = str_detect(ipc_classes, '^B') | str_detect(ipc_classes, ',B')) %>% 
  mutate( is_c = str_detect(ipc_classes, '^C') | str_detect(ipc_classes, ',C')) %>% 
  mutate( is_d = str_detect(ipc_classes, '^D') | str_detect(ipc_classes, ',D')) %>% 
  mutate( is_e = str_detect(ipc_classes, '^E') | str_detect(ipc_classes, ',E')) %>% 
  mutate( is_f = str_detect(ipc_classes, '^F') | str_detect(ipc_classes, ',F')) %>% 
  mutate( is_g = str_detect(ipc_classes, '^G') | str_detect(ipc_classes, ',G')) %>% 
  mutate( is_h = str_detect(ipc_classes, '^H') | str_detect(ipc_classes, ',H')) %>% 
  mutate(patent_id = substr(filename, 0, unlist(gregexpr(pattern ='.txt',filename)) -1 ))

write.csv(patents %>% 
            filter(!is.na(claims)),
          "data/main_dataset.csv")

