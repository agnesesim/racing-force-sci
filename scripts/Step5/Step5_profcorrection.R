library(dplyr) 
library(tidyverse)
library(tidytext)

patents <- read_csv("data/main_dataset.csv")
patents_text <- get_text_clean(patents)

tmp_patents <- patents_text %>%
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
          | str_detect(ipc_classes, ',H04')) == TRUE) %>%
  mutate(totwords = str_count(text, "\\s")+1) 

# 2. Search for real-time + sport in title OR abstract OR claims:
#   (real-time OR realtime OR (real NEAR time)) AND sport** AND (imag+ OR video+)

tmp_patents_2 <- tmp_patents %>%
  filter(((str_detect(text, "real-time")
          | str_detect(text, "real time")
          | str_detect(text, 'real.{1,10}time'))
          & str_detect(text, "sport")
          & (str_detect(text, "imag")
                     | str_detect(text, "video"))) == TRUE) 

# remove patents founded at point 2
id_2 <- pull(tmp_patents_2,patent_id)  
tmp_patents <- tmp_patents %>%
  filter( !patent_id %in% id_2)

# 3. Search for something about this in title OR abstract OR claims:
#   ((imag+ OR video+) 3NEAR interpret+) AND sport**

tmp_patents_3 <- tmp_patents %>%
  filter(((str_detect(text, "\b(?:imag\W+(?:\w+\W+){0,4}?elaborat|elaborat\W+(?:\w+\W+){0,4}?imag)\b")
          | str_detect(text,"\b(?:video\W+(?:\w+\W+){0,4}?elaborat|elaborat\W+(?:\w+\W+){0,4}?video)\b"))
          & str_detect(text, "sport")) == TRUE) 
