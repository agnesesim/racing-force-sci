
tmp_patents_0 <- patents_text  %>%
  filter(patent_id %in%  pull(patents_with_ipc_ok, patent_id)) 

tmp_patents_0 <- tmp_patents_0 %>%
  filter(!(patent_id %in%  pull(patents_with_offtopic, patent_id)))

tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(patents_found,patent_id) )

tmp_patents_n <- tmp_patents_0 %>%
  mutate(match = str_match_all(text, "(?<![\\w])sport")) %>%
  mutate(match_count = lengths(match)) %>%
  mutate(match_freq = (match_count/totwords)*100) %>%
  filter(match_freq>0.5) %>%
  select(patent_id, ipc_classes, text, totwords)

patents_found_3 <- tmp_patents_n

tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id))

tmp_patents_n <- tmp_patents_0 %>%
  filter(str_detect(text, "auto.{0,5}rac")
        | str_detect(text, "moto.{0,5}rac") 
        | str_detect(text, "rally.{0,5}rac") 
        | str_detect(text, "kart.{0,5}rac") == TRUE)  

patents_found_3 <- rbind(patents_found_3, tmp_patents_n)

tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id))


tmp_patents_n <- tmp_patents_0 %>%
  mutate(match = str_match_all(text, "comput.{0,5}vision")) %>%
  mutate(match_count = lengths(match)) %>%
  mutate(match_freq = (match_count/totwords)*100) %>%
  filter(match_freq>0.5) %>%
  select(patent_id, ipc_classes, text, totwords)

patents_found_3 <- rbind(patents_found_3, tmp_patents_n)

tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id))

tmp_patents_n <- tmp_patents_0 %>%
  mutate(match = str_match_all(text, "pilot(?![\\w])")) %>%
  mutate(match_count = lengths(match)) %>%
  mutate(match_freq = (match_count/totwords)*100) %>%
  filter(match_freq>0.5) %>%
  select(patent_id, ipc_classes, text, totwords)

patents_found_3 <- rbind(patents_found_3, tmp_patents_n)

tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id))

tmp_patents_n <- tmp_patents_0 %>%
  mutate(match = str_match_all(text, "car.{0,50}safe")) %>%
  mutate(match_count = lengths(match)) %>%
  mutate(match_freq = (match_count/totwords)*100) %>%
  filter(match_freq>0.5)  %>%
  select(patent_id, ipc_classes, text, totwords)

patents_found_3 <- rbind(patents_found_3, tmp_patents_n)

tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id))

tmp_patents_n <- tmp_patents_0 %>%
  mutate(match = str_match_all(text,"imag.{1,10}classificat")) %>%
  mutate(match_count = lengths(match)) %>%
  mutate(match_freq = (match_count/totwords)*100) %>%
  filter(match_freq>0.5)  %>%
  select(patent_id, ipc_classes, text, totwords)

patents_found_3 <- rbind(patents_found_3, tmp_patents_n)

tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id))

tmp_patents_n <- tmp_patents_0 %>%
  mutate(match = str_match_all(text,"(?<![\\w])obj.{1,10}recogn")) %>%
  mutate(match_count = lengths(match)) %>%
  mutate(match_freq = (match_count/totwords)*100) %>%
  filter(match_freq>0.5)  %>%
  select(patent_id, ipc_classes, text, totwords)


patents_found_3 <- rbind(patents_found_3, tmp_patents_n)

tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id))


patents_found_3 <- rbind(patents_found_3, tmp_patents_n)

tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id))

patent_rfg <- rbind (patents_found, patents_found_2, patents_found_3)

patent_rfg %>% 
  select(patent_id)%>% 
  write_csv("patent_RFGA.csv")

