library(dplyr) 
library(tidyverse)
library(tidytext)
library(fs)

patents <- read_csv("data/main_dataset.csv")
patents_RFGA <- read_csv("patent_RFGA.csv", show_col_types = FALSE)

patents <-patents %>%
  filter( patent_id %in%  pull(patents_RFGA,patent_id))

patents_unique <- distinct(patents, docdb_family_id, claims, .keep_all = TRUE)


patents_ids <-  patents$patent_id   

patents_id_toKeep <- character(0)
patents_id_toDelete <- character(0)

for(p in patents_ids){
  
  p_all <- patents %>% filter(patent_id == p)
  duplicate <- patents %>% filter(docdb_family_id == p_all$docdb_family_id & patent_id != p)
  
  if (nrow(duplicate)>0){  
    
    duplicate_claims <- duplicate %>% filter(claims == p_all$claims)  

    if (nrow(duplicate_claims)>0){
      
      # se tra gli altri ho un patent con più IPC
      # elimino questo patent
      l=0
      for (ipcs in duplicate_claims$ipc_classes){
        l = max(l, nchar(ipcs))
      }
      if (nchar(p_all$ipc_classes)<l){
        patents_id_toDelete <- append(patents_id_toDelete, p)
        next
      }
      
      # se tra gli altri ho un patent più recent 
      # elimino quello in questione
      maxDate = max(as.Date(duplicate_claims$publication_date))
      myDate <- as.Date(p_all$publication_date)
     
      if (myDate < maxDate){
        patents_id_toDelete <- append(patents_id_toDelete, p)
        next
      }
    }
  }
  patents_id_toKeep <- append(patents_id_toKeep, p)
}

patents_final <-patents %>%
  filter( patent_id %in% patents_id_toKeep )

patents_final %>% count(docdb_family_id) %>% filter(n>1)
