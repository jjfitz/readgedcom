library(tidyverse)

read_gedcom <- function(file_path) {
  file <- read_delim(file_path, '\n', "\n\n", col_names = FALSE)
  start_recording <- FALSE
  is_first <- 0
  birthdate <-  NA
  birthplace <-  NA
  birthlat <-  NA
  birthlong <-  NA
  firstname <- NA
  lastname <- NA
  famc <- NA
  fams <- NA
  id <- NA
  deathdate <-  NA
  deathplace <-  NA
  deathlat <-  NA
  deathlong <-  NA
  sex <-  NA

  dfTemp <- data.frame(matrix(nrow=1, ncol=14))
  names(dfTemp) <- c("id", "firstname","lastname","birthdate", "birthplace",
                     "birthlat", "birthlong", "deathlat", "deathlong", "deathdate",
                     "deathplace", "sex", "FAMC", "FAMS")

  for (i in 1:length(file[1][[1]])) {
    tmpv <- file[1][[1]][[i]]
    # print(tmpv)
    # if(str_detect(tmpv, "@ INDI") && id == NA) {
    #   start_recording <-TRUE
    # }
    # print(tmpv)
    # if(start_recording) {
    if(str_detect(tmpv, "@ INDI")) {
      # if (fams != NA || (is_first == 1 && famc != NA)) {
      # print(cont)
      line.to.write <- data_frame(id, firstname, lastname, birthdate, birthplace,
                                  birthlat, birthlong, deathlat, deathlong,
                                  deathdate, deathplace, sex, famc, fams)
      names(line.to.write) <- c("id", "firstname","lastname","birthdate", "birthplace",
                                "birthlat", "birthlong", "deathlat", "deathlong",
                                "deathdate", "deathplace","sex","FAMC", "FAMS")
      dfTemp <- rbind(dfTemp, line.to.write)
      birthdate <-  NA
      birthplace <-  NA
      birthlat <-  NA
      birthlong <-  NA
      deathdate <-  NA
      deathplace <-  NA
      deathlat <-  NA
      deathlong <-  NA
      firstname <- NA
      lastname <- NA
      sex <- NA
      famc <- NA
      fams <- NA
      # }
      id <- str_extract(tmpv,"@.+@")
      # is_first <- is_first + 1
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

    if(str_detect(tmpv, " SEX")) {
      sex <- str_extract(tmpv,"(?<=SEX ).+")
      next
    }

    if(str_detect(tmpv, " FAMC")) {
      famc <- str_extract(tmpv,"@.+@")
      next
    }

    if(str_detect(tmpv, " FAMS")) {
      fams <- paste0(fams, " ", str_extract(tmpv,"@.+@"))
    }

    # if (!IsDateTime(file[1][[1]][[i]])) {
    #   ctemp <- file[1][[1]][[i]]
    #   cont <- paste0(cont, " ", ctemp)
    # }
    # }
  }
  return(as_tibble(dfTemp))
}




