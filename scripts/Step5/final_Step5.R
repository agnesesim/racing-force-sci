library(dplyr) 
library(tidyverse)
library(tidytext)
library(fs)
library(readxl)

patents <- read_csv("data/data_prepared_python.csv")

patents<- patents  %>% 
  mutate(patent_id = substr(filename, 0, unlist(gregexpr(pattern ='.txt',filename)) -1 ))

racing_force<-read_excel("509_racing.xlsx")
racing_force<-racing_force %>% select(filename_original, `english claims`)
patents_id_509 = pull(racing_force,filename_original)


new_patents <- patents %>%
  filter(patent_id %in% patents_id_509)

join <- left_join(new_patents, racing_force, by = c("patent_id" = "filename_original"))

join$claims <- ifelse(is.na(join$claims), join$`english claims`, join$claims)
join %>% 
  select(patent_id, filing_date, publication_date, priority_date, assignee, inventors, docdb_family_id, title, abstract, claims, filename)%>% 
  write_csv("patent_RFGA.csv")
  