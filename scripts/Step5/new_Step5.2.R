library(dplyr) 
library(tidyverse)
library(tidytext)
library(fs)
library(readxl)

patents <- read_csv("data/data_prepared_python.csv")
patents<- patents  %>% 
  mutate(patent_id = substr(filename, 0, unlist(gregexpr(pattern ='.txt',filename)) -1 ))

# controllo se l'id nel title, nell'abstract e nei claims è presente
id_title = "US20210166350" 
id_abs = "US2021166350"

found <- patents %>% 
  filter( str_detect(patent_id, id_title)
        | str_detect(patent_id, id_abs)) 

# se ho trovato una sola riga prendo l'id
if (nrow(found)<=1){
  found %>% select(patent_id)
}

# se ho trovato più righe prendo quello con più claims
if (str_length(found["claims"][[1]][1])>str_length(found["claims"][[1]][2])){
  print(found["patent_id"][[1]][1])
}
if (str_length(found["claims"][[1]][1])<str_length(found["claims"][[1]][2])){
  print(found["patent_id"][[1]][2])
}

# se ho trovato più righe prendo quello con più abstract
if (str_length(found["abstract"][[1]][1])>str_length(found["abstract"][[1]][2])){
  print(found["patent_id"][[1]][1])
}
if (str_length(found["abstract"][[1]][1])<str_length(found["abstract"][[1]][2])){
  print(found["patent_id"][[1]][2])
}

# se ho trovato più righe prendo quello con più ipc_classes
if (str_length(found["ipc_classes"][[1]][1])>str_length(found["ipc_classes"][[1]][2])){
  print(found["patent_id"][[1]][1])
}
if (str_length(found["ipc_classes"][[1]][1])<str_length(found["ipc_classes"][[1]][2])){
  print(found["patent_id"][[1]][2])
}


# scelgo a occhio il più vecchio
found %>%
  select(patent_id, publication_date)


##################### se non ho trovato nessun match id #####################
this_title = "Fusion network-based method for image super-resolution and non-uniform motion deblurring" 
this_abstract = "fusion network-based method for image super-resolution and non-uniform motion deblurring"



found <- patents %>% 
  filter( str_detect(tolower(abstract), tolower(this_abstract)) )

found

found <- patents %>% 
  filter( str_detect(tolower(title), tolower(this_title)))

found

found <- patents %>% 
  filter( str_detect(tolower(abstract), tolower(this_abstract))
          & str_detect(tolower(title), tolower(this_title)) )
