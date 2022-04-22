library(tidyverse)
library(tidytext)
library(fs)

patents <- read_csv("data/main_dataset.csv")

patents %>% filter(is.na(ipc_classes)) %>% mutate (ipc_classes = '')

patents <- patents %>% 
  mutate( is_a = str_detect(ipc_classes, '^A') | str_detect(ipc_classes, ',A')) %>% 
  mutate( is_b = str_detect(ipc_classes, '^B') | str_detect(ipc_classes, ',B')) %>% 
  mutate( is_c = str_detect(ipc_classes, '^C') | str_detect(ipc_classes, ',C')) %>% 
  mutate( is_d = str_detect(ipc_classes, '^D') | str_detect(ipc_classes, ',D')) %>% 
  mutate( is_e = str_detect(ipc_classes, '^E') | str_detect(ipc_classes, ',E')) %>% 
  mutate( is_f = str_detect(ipc_classes, '^F') | str_detect(ipc_classes, ',F')) %>% 
  mutate( is_g = str_detect(ipc_classes, '^G') | str_detect(ipc_classes, ',G')) %>% 
  mutate( is_h = str_detect(ipc_classes, '^H') | str_detect(ipc_classes, ',H'))

a <- tibble(section = 'A', n = nrow(patents %>% filter(is_a == TRUE)))
b <- tibble(section = 'B', n = nrow(patents %>% filter(is_b == TRUE)))
c <- tibble(section = 'C', n = nrow(patents %>% filter(is_c == TRUE)))
d <- tibble(section = 'D', n = nrow(patents %>% filter(is_d == TRUE)))
e <- tibble(section = 'E', n = nrow(patents %>% filter(is_e == TRUE)))
f <- tibble(section = 'F', n = nrow(patents %>% filter(is_f == TRUE)))
g <- tibble(section = 'G', n = nrow(patents %>% filter(is_g == TRUE)))
h <- tibble(section = 'H', n = nrow(patents %>% filter(is_h == TRUE)))

patents_count = bind_rows(a,b)
patents_count = bind_rows(patents_count, c)
patents_count = bind_rows(patents_count, d)
patents_count = bind_rows(patents_count, e)
patents_count = bind_rows(patents_count, f)
patents_count = bind_rows(patents_count, g)
patents_count = bind_rows(patents_count, h)
patents_count

patents_count %>%
ggplot(aes(section, n, fill=section)) +
  geom_col() +
  xlab(NULL) +
  theme_bw()

patents %>% filter(is_h==TRUE) %>% 
  mutate(class = substr(ipc_classes, str_locate(ipc_classes,'H\\d\\d[^/]'), str_locate(ipc_classes,'H\\d\\d[^/]')+2)) %>%
count(class, sort = TRUE) %>%
  ggplot(aes(class, n)) +
  geom_col( fill="dark violet") +
  xlab(NULL) +
  coord_flip() +
  theme_bw()

patents <- patents %>% 
  mutate(section = substr(ipc_classes, 1,1))

ggplot(patents, aes(x = publication_date)) +
  geom_histogram(binwidth = 25, position = "dodge") 

