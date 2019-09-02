source("read_gedcom.R")

gd <- read_gedcom("./data/George_Washington_Fam.txt")

gd %<>%
  slice(1:31)

fam_graph <- gd %>%
  mutate(FAMS = if_else(FAMS == "", FAMC, FAMS)) %>%
  filter(FAMC != "", firstname != "Nathaniel") %>%
  select(FAMS, FAMC, firstname, birthlat, birthlong, deathlat, deathlong) %>%
  graph_from_data_frame()

g_name <- gd %>%
  mutate(FAMS = if_else(FAMS == "", FAMC, FAMS)) %>%
  filter(FAMC != "", firstname != "Nathaniel") %>%
  select(FAMS, FAMC, firstname)

g_name$V_verts <- V(fam_graph)$name

correct_name <- g_name %>%
  left_join(gd, by = c("V_verts" = "FAMC"))

fam_graph <- correct_name %>%
  mutate(FAMS.x = if_else(FAMS.x == "", FAMC, FAMS.x)) %>%
  select(FAMS.x, FAMC, firstname.y) %>%
  filter(FAMC != "") %>%
  graph_from_data_frame()

V(fam_graph)$F_name <- E(fam_graph)$firstname.y

ggraph(fam_graph, layout = "dendrogram") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = F_name), vjust = 1, hjust = 1, angle = 35) +
  expand_limits(x = -.5, y = -.5)

