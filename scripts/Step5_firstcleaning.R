library(dplyr) 
library(tidyverse)
library(tidytext)
library(fs)

patents <- read_csv("data/main_dataset.csv")


# funzione per la fusione e la pulizia del testo 
get_text_clean <- function(data){
  data_text <- data %>%
    # riunisco tutto il testo del patent sotto un solo attributo 'text'
    mutate(text = paste(title, abstract, claims, sep=" ")) %>%
    # trasformo tutto il testo in minuscolo
    mutate(text = tolower(text)) %>% 
    # rimuovo la punteggiatura
    #mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
    # estraggo solo le 3 colonne interessanti per la ricerca
    select(patent_id, ipc_classes, text) %>%
    mutate(totwords = str_count(text, "\\s")+1) 
  
  return(data_text)
}

patents_text <- get_text_clean(patents)

patents_with_ipc_ok <- patents_text %>%
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
          | str_detect(ipc_classes, ',H04')) == TRUE) 

patents_with_offtopic <- patents_text %>% 
  filter(( str_detect(text, "wearable sensors") 
           | str_detect(text, "uav")
           | str_detect(text, "autonomous driving")
           | str_detect(text, "unmanned aerial vehicles")
           | str_detect(text, "drone")
           | str_detect(text, "autonomous robot")
           | str_detect(text, "driver.{1,10}assistance")
           | str_detect(text, "parking.{1,10}assistance")
           | str_detect(text, "authentication systems")
           | str_detect(text, "face recognition")
           | str_detect(text, "facial recognition")
           | str_detect(text, "facial imag")
           | str_detect(text, "image projection systems")
           | str_detect(text, "working helmet")
           | str_detect(text, "pilotless")
           | str_detect(text, "baseball")
           | str_detect(text, "soccer")
           | str_detect(text, "curling")
           | str_detect(text, "sport bet")
           | str_detect(text, "live bet")
           | str_detect(text, "real estate")
           | str_detect(text, "sport hors")
           | str_detect(text, "quadrup")
           | str_detect(text, "(?<![\\w])skin")
           | str_detect(text, "(?<![\\w])medical")
           | str_detect(text, "patient")
  ) == TRUE)


