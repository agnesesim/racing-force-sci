library(dplyr) 
library(tidyverse)
library(tidytext)
library(fs)

patents <- read_csv("data/main_dataset.csv")
patents_RFGA <- read_csv("patent_RFGA.csv", show_col_types = FALSE)

patents <-patents %>%
  filter( patent_id %in%  pull(patents_RFGA,patent_id))

patents_unique <- distinct(patents, docdb_family_id, claims, .keep_all = TRUE)


patents_unique %>% 
  select(patent_id)%>% 
  write_csv("patent_RFGA.csv")

families <- patents_unique %>% count(docdb_family_id) %>% filter(n>1) %>% select(docdb_family_id)

families <- unlist(families)

patents_id_toKeep <- character(0)
for (fam in families){
  
  pat_of_fam <- patents_unique %>% filter(docdb_family_id == fam) %>% arrange(desc(filing_date))
  
  patent_chose = pat_of_fam[["patent_id"]][1]
  ipc = pat_of_fam[["ipc_classes"]][1]
  
  for (p in pat_of_fam$patent_id){
    patent = pat_of_fam %>% filter(patent_id == p)
    if (nchar(patent$ipc_classes)>nchar(ipc)){
      patent_chose = patent$patent_id
      ipc = patent$ipc_classes
    }
  }
  
  patents_id_toKeep <- append(pat_of_fam, p)
}
