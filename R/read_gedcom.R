library(tidyverse)
library(gtools)
library(flipTime)
library(tidytext)
library(igraph)
library(ggraph)
library(magrittr)
library(leaflet)
library(lubridate)

read_gedcom <- function(file_path) {
  file <- read_delim(file_path, '\n', "\n\n", col_names = FALSE)
  start_recording <- FALSE
  is_first <- 0
  birthdate <-  ""
  birthplace <-  ""
  birthlat <-  ""
  birthlong <-  ""
  firstname <- ""
  lastname <- ""
  famc <- ""
  fams <- ""
  id <- ""
  deathdate <-  ""
  deathplace <-  ""
  deathlat <-  ""
  deathlong <-  ""
  sex <-  ""
  
  dfTemp <- data.frame(matrix(nrow=1, ncol=14)) 
  names(dfTemp) <- c("id", "firstname","lastname","birthdate", "birthplace",
                     "birthlat", "birthlong", "deathlat", "deathlong", "deathdate", 
                     "deathplace", "sex", "FAMC", "FAMS")
  
  for (i in 1:length(file[1][[1]])) {
    tmpv <- file[1][[1]][[i]]
    if(str_detect(tmpv, " INDI") && id == "") {
      start_recording <-TRUE
    }
    # print(tmpv)
    if(start_recording) {
      if(str_detect(tmpv, " INDI")) {
        # print("Yes")
        if (fams != "" || (is_first == 1 && famc != "")) {
          # print(cont)
          line.to.write <- data_frame(id, firstname, lastname, birthdate, birthplace, 
                                      birthlat, birthlong, deathlat, deathlong, 
                                      deathdate, deathplace, sex, famc, fams)
          names(line.to.write) <- c("id", "firstname","lastname","birthdate", "birthplace",
                                    "birthlat", "birthlong", "deathlat", "deathlong", 
                                    "deathdate", "deathplace","sex","FAMC", "FAMS")
          dfTemp <- rbind(dfTemp, line.to.write)
          birthdate <-  ""
          birthplace <-  ""
          birthlat <-  ""
          birthlong <-  ""
          deathdate <-  ""
          deathplace <-  ""
          deathlat <-  ""
          deathlong <-  ""
          firstname <- ""
          lastname <- ""
          sex <- ""
          famc <- ""
          fams <- ""
        }
        id <- str_extract(tmpv,"@.+@")
        is_first <- is_first + 1
        next
      }
      
      if(str_detect(tmpv, " NAME")) {
        firstname <- str_extract(tmpv,"(?<=NAME ).+(?= /+.)")
        lastname <- str_extract(tmpv,"(?<=/).+/")
        next
      }
      
      if(str_detect(tmpv, " BIRT")) {
        birthdate <- str_extract(file[1][[1]][[i+1]],"(?<=DATE ).+")
        birthplace <- str_extract(file[1][[1]][[i+2]],"(?<=PLAC ).+")
        birthlat <- str_extract(file[1][[1]][[i+4]],"(?<=LATI ).+")
        birthlong <- str_extract(file[1][[1]][[i+5]],"(?<=LONG ).+")
        next
      }
      
      if(str_detect(tmpv, " DEAT")) {
        deathdate <- str_extract(file[1][[1]][[i+1]],"(?<=DATE ).+")
        deathplace <- str_extract(file[1][[1]][[i+2]],"(?<=PLAC ).+")
        deathlat <- str_extract(file[1][[1]][[i+4]],"(?<=LATI ).+")
        deathlong <- str_extract(file[1][[1]][[i+5]],"(?<=LONG ).+")
        next
      }
      
      if(str_detect(tmpv, "SEX")) {
        sex <- str_extract(tmpv,"(?<=SEX ).+")
        next
      }
      
      if(str_detect(tmpv, " FAMC")) {
        famc <- str_extract(tmpv,"@.+@")
        next
      }
      
      if(str_detect(tmpv, " FAMS")) {
        fams <- str_extract(tmpv,"@.+@")
      }
    }
  }
  return(dfTemp)
}





