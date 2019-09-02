source("read_gedcom.R")

gd <- read_gedcom("./data/George_Washington_Fam.txt")

gd %>% as_tibble() %>%
  filter(!is.na(id), !is.na(birthlat)) %>%
  mutate(birthlat = as.numeric(birthlat),
         birthlong = as.numeric(birthlong),
         # deathlat = as.numeric(deathlat),
         # deathlong = as.numeric(deathlong),
         birthdate = dmy(birthdate),
         deathdate = dmy(deathdate),
         age = year(deathdate) - year(birthdate))

gd %<>%
  filter(!is.na(birthlat)) %>%
  mutate(FAMS = if_else(FAMS == "", FAMC, FAMS))

gd$childlat <- ""
gd$childlong <- ""

for(i in 1:nrow(gd)){
  for (j in 1:nrow(gd)) {
    if(gd$FAMC[i] == gd$FAMS[j]) {
      gd$childlat[j] <- as.numeric(gd$birthlat[i])
      gd$childlong[j] <- as.numeric(gd$birthlong[i])
    }
    if(gd$FAMC[i] == "") {
      break;
    }
  }
}

gd %<>%
  mutate(childlat = as.numeric(childlat),
         childlong = as.numeric(childlong),
         generation = if_else(as.numeric(str_extract(FAMS,"[:digit:]+")) == 1, 1, 
                              if_else(as.numeric(str_extract(FAMS,"[:digit:]+")) < 4, 2,
                                      if_else(as.numeric(str_extract(FAMS,"[:digit:]+")) < 8, 3,
                                              if_else(as.numeric(str_extract(FAMS,"[:digit:]+")) < 16, 4,
                                                      if_else(as.numeric(str_extract(FAMS,"[:digit:]+")) < 32, 5, 6))))),
         gencol = if_else(generation == 1, "blue",
                          if_else(generation == 2, "red",
                                  if_else(generation == 3, "yellow",
                                          if_else(generation == 4, "green",
                                                  if_else(generation == 5, "pink", "brown"))))),
         birthlat = if_else(!is.na(birthlat), birthlat, 43.2344 + as.numeric(str_extract(FAMS,"[:digit:]+"))),
         birthlong = if_else(!is.na(birthlong), birthlong, as.numeric(str_extract(FAMS,"[:digit:]+"))-122.2344)
  )

greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

map4 <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = gd$birthlong, 
             lat = gd$birthlat,
             popup = paste0("Name: ", gd$firstname, " ", gd$lastname, "<br />",
                            "Birth Place: ", gd$birthplace, "<br />",
                            "Birth Day: ", gd$birthdate, "<br />",
                            gd$id),
             icon = greenLeafIcon) 

for(i in 1:nrow(gd)){
  map4 <- addPolylines(map4, lat = as.numeric(gd[i, c(6, 18)]), 
                       lng = as.numeric(gd[i, c(7, 19)]),
                       color = gd$gencol[i])
}

# for( group in levels(df$group)){
#   map <- addPolylines(map, lng=~lon,lat=~lat,data=df[df$group==group,], color=~col)
# }
map4 %>%
  addLegend(colors = levels(as_factor(gd$gencol)), labels = levels(as_factor(gd$generation)))
