
tmp_patents_0 <- patents_text  %>%
  filter(!patent_id %in%  pull(patents_with_ipc_ok, patent_id)) 

tmp_patents_0 <- tmp_patents_0 %>%
  filter(!(patent_id %in%  pull(patents_with_offtopic, patent_id)))

# 2. Search for real-time + sport in title OR abstract OR claims:
#   (real-time OR realtime OR (real NEAR time)) AND sport** AND (imag+ OR video+)
tmp_patents_n <- tmp_patents_0 %>%
  filter(((str_detect(text, "real-time")
           | str_detect(text, "real time")
           | str_detect(text, 'real.{0,10}time'))
          & (str_detect(text, "(?<![\\w])sport")
             | str_detect(text, "(?<![\\w])race"))
          & (str_detect(text, "imag")
             | str_detect(text, "video"))) == TRUE) 

patents_found_2 <- tmp_patents_n

# remove patents founded at point 2
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id) )


# 3. Search for something about this in title OR abstract OR claims:
#   ((imag+ OR video+) 3NEAR interpret+) AND sport**
tmp_patents_n <- tmp_patents_0 %>%
  filter(((str_detect(text, "imag.{1,50}(interpret|elaborat|analys|process)")
           | str_detect(text,"(interpret|elaborat|analys|process).{1,50}imag"))
          & str_detect(text, "(?<![\\w])sport")) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 3
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id) )


# 4 Search for camera + sport in title OR abstract OR claims: 
#   (cameraORpixel+) AND sport**

tmp_patents_n <- tmp_patents_0 %>%
  filter(((str_detect(text, "(?<![\\w])camera")
           | str_detect(text,"(?<![\\w])cam(?![\\w])")
           | str_detect(text,"pixel"))
          & str_detect(text, "(?<![\\w])sport")) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 4
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id)  )

# 5 Searchfor broadcast+ (in anysport!) + sport in title OR abstract OR claims: 
#   broadcast+ AND sport**

tmp_patents_n <- tmp_patents_0 %>%
  filter((str_detect(text, "broadcast")
          & str_detect(text, "(?<![\\w])sport")) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 5
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id)  )


# 6 Searchfor race + sport in title OR abstract OR claims: 
#   ((race+ OR racing OR racetrack+ OR speeway+ OR ride+ OR trajector+) OR pilot) AND sport** AND (imag+ OR video+) -> verifythe results

tmp_patents_n <- tmp_patents_0 %>%
  filter(((str_detect(text, "(?<![\\w])race(?![\\w])")
           | str_detect(text, "racing")
           | str_detect(text, "racetrack")
           | str_detect(text, "race.{1,10}track")
           | str_detect(text, "speedway")
           | str_detect(text, "speed.{1,10}way")
           | str_detect(text, "(?<![\\w])ride")
           | str_detect(text, "trajector"))
          & str_detect(text, "(?<![\\w])sport")) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 6
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id)  )


# 7 Search for helmet + sport in title OR abstract OR claims:
#   helmet+ AND sport** AND (imag+ OR video+ OR camera OR pixel+ OR broadcast+)
# -> verify the results to understand if needto skip some keywords

tmp_patents_n <- tmp_patents_0 %>%
  filter((str_detect(text, "helmet")
          & str_detect(text, "(?<![\\w])sport")
  ) 
  | ((str_detect(text, "imag")
      | str_detect(text, "video")
      | str_detect(text, "camera")
      | str_detect(text, "pixel")
      | str_detect(text, "broadcast"))
     &  str_detect(text, "helmet")
     & !(str_detect(text, "helmet.{1,50}work")
         | str_detect(text,"work.{1,50}helmet")))
  == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 7
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id)  )


# 8  Search for quantum + sport in title OR abstract OR claims:
#   (quantum+) AND sport** or in other way like asG06N10/00+ "vehicle“ as you made
# -> keep all of them(wecan make a special)

tmp_patents_n <- tmp_patents_0 %>%
  filter((
    (str_detect(text, "quantum") | str_detect(text, "quantic"))
    & (str_detect(text, "(?<![\\w])vehicl") | str_detect(text, "(?<![\\w])sport"))
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 8
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id)  )

# 9 Search for damage analysis + sport in title OR abstract OR claims: 
#   ((camera OR pixel+) OR imag+ OR video+) AND damag+ AND sport** -> verifythe results

tmp_patents_n <- tmp_patents_0 %>%
  filter(((str_detect(text, "(?<![\\w])camera")
           | str_detect(text,"(?<![\\w])cam(?![\\w])")
           | str_detect(text,"pixel")
           | str_detect(text,"imag")
           | str_detect(text,"video"))
          & str_detect(text, "(?<![\\w])damag")
          & (str_detect(text, "(?<![\\w])vehicl") | str_detect(text, "(?<![\\w])sport"))
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 9 
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id)  )

# 10 Search for simulation and training + sport + pilot in title OR abstract OR claims:
#   sport**ANDpilotAND (simulat+ OR train+ OR gam+)AND (imag+ OR video+)-> verifythe results

tmp_patents_n <- tmp_patents_0 %>%
  filter((
    (str_detect(text, "simulat")
     | str_detect(text,"(?<![\\w])train")
     | str_detect(text,"(?<![\\w])gam"))
    & (str_detect(text,"imag")
       | str_detect(text,"video"))
    & str_detect(text, "pilot")
    & (str_detect(text, "(?<![\\w])vehicl") | str_detect(text, "(?<![\\w])sport"))
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 10
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id)  )

# 11  Search for image + sport in title OR abstract OR claims:
#   (imag+ OR video+) AND sport** 
#   -> verifyand refinethe resultsto understandifyouneedto skip some keywords

tmp_patents_n <- tmp_patents_0 %>%
  filter((
    (str_detect(text,"imag")
     | str_detect(text,"video"))
    & str_detect(text, "(?<![\\w])sport")
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 11
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id)  )

# 12  Search For something about this in title OR abstract OR claims: 
#   •anomaly detection in the sport field
# •image classification or object recognition in the sport field
# •smart mirror in the sport field
# •safety issues in the sport field
# •image segmentation in the sport field
# •motion detection in the sport field (for car and vehicles)

tmp_patents_n <- tmp_patents_0 %>%
  filter((
    str_detect(text,"anomal.{1,50}detect")
    & str_detect(text, "(?<![\\w])vehicl")
    & str_detect(text, "(?<![\\w])gam")
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 12_1
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id))

################################################
# •image classification or object recognition in the sport field
tmp_patents_n <- tmp_patents_0 %>%
  filter((
    (str_detect(text,"imag.{1,50}classificat")
     | str_detect(text,"(?<![\\w])obj.{1,50}recogn"))
    & str_detect(text, "(?<![\\w])sport")
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 12_2
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id) )

################################################
# •smart mirror in the sport field
tmp_patents_n <- tmp_patents_0 %>%
  filter((
    str_detect(text,"smart.{1,50}mirror")
    #& str_detect(text, "(?<![\\w])sport")
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 12_3
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in% pull(tmp_patents_n,patent_id))

################################################
# •safety issues in the sport field
tmp_patents_n <- tmp_patents_0 %>%
  filter((
    str_detect(text,"(?<![\\w])saf")
    & ((str_detect(text, "(?<![\\w])vehicl")
        & str_detect(text, "(?<![\\w])gam"))
       | str_detect(text, "(?<![\\w])sport"))
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 12_4
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id) )

################################################
# •image segmentation in the sport field
tmp_patents_n <- tmp_patents_0 %>%
  filter((
    str_detect(text,"imag.{1,50}segmentat")
    & ((str_detect(text, "(?<![\\w])vehicl")
        & str_detect(text, "(?<![\\w])gam"))
       | str_detect(text, "(?<![\\w])sport"))
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 12_5 
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id) )

################################################
# •motion detection in the sport field (for car and vehicles)
tmp_patents_n <- tmp_patents_0 %>%
  filter((
    str_detect(text,"motion.{1,50}detect")
    & ((str_detect(text, "(?<![\\w])vehicl")
        & str_detect(text, "(?<![\\w])gam"))
       | str_detect(text, "(?<![\\w])sport"))
  ) == TRUE) 

patents_found_2 <- rbind(patents_found_2,tmp_patents_n)

# remove patents founded at point 12_6
tmp_patents_0 <- tmp_patents_0 %>%
  filter( !patent_id %in%  pull(tmp_patents_n,patent_id)  )

