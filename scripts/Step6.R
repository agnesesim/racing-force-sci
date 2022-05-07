library(dplyr) 
library(tidyverse)
library(tidytext)
library(fs)

patents <- read_csv("data/main_dataset.csv")
patents_RFGA <- read_csv("patent_RFGA.csv", show_col_types = FALSE)

patents <-patents %>%
  filter( patent_id %in%  pull(patents_RFGA,patent_id))

patents %>% group_by(docdb_family_id)
patents_ids <-  patents$patent_id   

patents_id_toKeep <- character(0)
patents_id_toDelete <- character(0)

for(p in patents_ids){
  
  p_all <- patents %>% filter(patent_id == p)
  duplicate <- patents %>% filter(docdb_family_id == p_all$docdb_family_id & patent_id != p)
  
  if (nrow(duplicate)>0){  
    
    duplicate_claims <- duplicate %>% filter(claims == p_all$claims  & patent_id != p)  

    if (nrow(duplicate_claims)>0){
      
      # se tra gli altri ho un patent con più IPC
      # elimino questo patent
      l=0
      for (ipcs in duplicate_claims$ipc_classes){
        l = max(l, nchar(ipcs))
      }
      if (nchar(p_all$ipc_classes)<l){
        print(p)
        patents_id_toDelete <- append(patents_id_toDelete, p)
        next
      }
      
      # se tra gli altri ho un patent più recent 
      # elimino quello in questione
      d = min(as.Date(duplicate_claims$filing_date))
      myDate <- as.Date(p_all$filing_date)
      if (myDate < d){
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
