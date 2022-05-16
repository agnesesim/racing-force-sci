library(dplyr) 
library(tidyverse)
library(tidytext)
library(fs)
library(readxl)

patents <- read_csv("data/data_prepared_python.csv")

patents<- patents  %>% 
  mutate(patent_id = substr(filename, 0, unlist(gregexpr(pattern ='.txt',filename)) -1 ))

racing_force<-read_excel("data_prof/racing.xlsx")

# funzione per la lettura di ciascun file in un tibble
extract_patent_id <- function(title, abstract, claims){
  out <- tryCatch(
    {
      
      y <- tibble( patent_id_1 = str_sub(title,str_locate(title, "\\(")[1]+1,str_locate(title, "\\)")[1]-1),
                   patent_id_2 = str_sub(title,str_locate(abstract, "\\(")[1]+1,str_locate(title, "\\)")[1]-1),
                   patent_id_3 = str_sub(title,str_locate(claims, "\\(")[1]+1,str_locate(title, "\\)")[1]-1),
                   title = str_sub(title,str_locate(title, "\\)")[1]+2),
                   abstract = str_sub(abstract,str_locate(abstract, "\\)")[1]+2),
                   claims = str_sub(claims,str_locate(claims, "\\)")[1]+2),
                   )
      return(y)
    },
    error=function(cond) {
      message(paste("Error:",cond))
      return(NA)
    },
    warning=function(cond) {
      message(paste("Warning:",cond))
      return(NULL)
    }
  )    
  return(out)
}

racing_force_id <- t(mapply(extract_patent_id,
                            racing_force$Title, 
                            racing_force$Abstract, 
                            racing_force$`English claims`)
                     ) %>% as_tibble()

###############################################################################
####################################ID#########################################
###############################################################################

patents_id_1 = pull(racing_force_id,patent_id_1)
patents_id_2 = pull(racing_force_id,patent_id_2)
patents_id_3 = pull(racing_force_id,patent_id_3)
find_patent_by_id <- function(id){
  out <- tryCatch(
    {
      id_seach = str_sub(id, 0, str_locate(id, "\\_")[1]-1)
      
      res = grepl(id_seach, patents_id_1, fixed = TRUE)
      res_num = which(res)
      len = length(res_num) 

      #print(paste(res_num, len, sep='.'))
      if (len>0){
        y <- tibble( patent_id = id,
                     erica_id = patents_id_1[res_num])
        return(y)
      }
      else
      {
        res = grepl(id_seach, patents_id_2, fixed = TRUE)
        res_num = which(res)
        len = length(res_num) 
        
        #print(paste(res_num, len, sep='.'))
        if (len>0){
          y <- tibble( patent_id = id,
                       erica_id = patents_id_2[res_num])
          return(y)
        }
        else
        {
          res = grepl(id_seach, patents_id_3, fixed = TRUE)
          res_num = which(res)
          len = length(res_num) 
          
          #print(paste(res_num, len, sep='.'))
          if (len>0){
            y <- tibble( patent_id = id,
                         erica_id = patents_id_3[res_num])
            return(y)
          }
        }
      }
    },
    error=function(cond) {
      message(paste("Error:",cond))
      return(NA)
    },
    warning=function(cond) {
      message(paste("Warning:",cond))
      return(NULL)
    }
  )
  return(out)
}

data <-lapply(patents$patent_id,
              find_patent_by_id)
patents_by_id <- do.call(rbind, data)
nrow(distinct(patents_by_id, erica_id))

###############################################################################
##################################TITLE########################################
###############################################################################

find_patent_by_title <- function(title, id){
  out <- tryCatch(
    {
      res = grepl(title, pull(racing_force_id,title), fixed = TRUE)
      res_num = which(res)
      len = length(res_num) 
      
      if (len>0){
        
        y <- tibble( id = id,
                     erica_id = pull(racing_force_id,patent_id_1)[res_num],
                     title = title,
                     erica_title = pull(racing_force_id,title)[res_num])
      }
    },
    error=function(cond) {
      message(paste("Error:",cond))
      return(NA)
    },
    warning=function(cond) {
      message(paste("Warning:",cond))
      return(NULL)
    }
  )
  return(out)
}


data <- mapply(find_patent_by_title,
               patents$title,
               patents$patent_id,
               SIMPLIFY = FALSE)
patents_by_title <- do.call(rbind, data)     

nrow(distinct(patents_by_title, erica_id))


patents_by_title %>% 
  write_csv("patents_by_title")
###############################################################################
################################ABSTRACT#######################################
###############################################################################

find_patent_by_abstract <- function(abstract, id){
  out <- tryCatch(
    {
      res = grepl(abstract, pull(racing_force_id,abstract), fixed = TRUE)
      res_num = which(res)
      len = length(res_num) 
      
      if (len>0){
        
        y <- tibble( id = id,
                     erica_id = pull(racing_force_id,patent_id_1)[res_num],
                     abstract = abstract,
                     erica_abstract = pull(racing_force_id,abstract)[res_num])
      }
    },
    error=function(cond) {
      message(paste("Error:",cond))
      return(NA)
    },
    warning=function(cond) {
      message(paste("Warning:",cond))
      return(NULL)
    }
  )
  return(out)
}


data <- mapply(find_patent_by_abstract,
               patents$abstract,
               patents$patent_id,
               SIMPLIFY = FALSE)
patents_by_abstract <- do.call(rbind, data)     


patents_by_abstract %>% 
  write_csv("patents_by_abstract.csv")
nrow(distinct(patents_by_abstract, erica_id))



