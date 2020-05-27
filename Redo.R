library("XML")
library("methods")
require(data.table)
require(ggplot2)
require(tidyr)
require(plyr)
require(knitr)
require(RMySQL)
require(ineq)
require(grid)
require(igraph)
require(rgexf)
library(igraph)
library(ggrepel)
require(rcrossref)
library(KDViz)
library(xml2)
require(patchwork)
require(leiden)
library(readxl)
library(gtools)
library(directlabels)
pswd = 'alex55Truc!1epistemo'
usr = 'alexandre'
ESH <- dbConnect(MySQL(), user=usr, password=pswd, dbname='OST_Expanded_SciHum',
                 host='127.0.0.1')





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART I : GETTING THE CORPUS FROM XML, GETTING THE CORP, GETTING THE EXTENDED CORP ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Setting things up####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


######################### Directory **********************
setwd("/projects/data/alexandre/BE_JEL/Redo")

######################### Functions **********************
#not in
`%notin%` <- Negate(`%in%`)

#the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#get references and their disciplines
get_disc_ref <- function(x ### x=df of corpus, y=minimum number of connections
){
  x <- merge(x, all_ref, by="ID_Art")
  x <- merge(x, all_art[,.(ItemID_Ref, ID_Art, Code_Revue)], by="ItemID_Ref")
  x <- merge(x, revues, by="Code_Revue")
  x <- merge(x, disciplines[,.(Code_Discipline, ESpecialite)], by = "Code_Discipline")
  return(x) # utilser cette ligne pour sortir un objet.
}

#reorder plots
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}



######################### Basic DF **********************
#DF of disciplines of interest for especialite_regrouped
list_discipline <- data.table(disciplines = c("Economics", "Management", "Psychology", "Neurosciences", "General Sciences", "Law"))

#DF of revues
revues  <-  dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% data.table

#DF of disciplines
disciplines <- dbGetQuery(ESH, "SELECT ESpecialite, Code_Discipline FROM OST_Expanded_SciHum.Disciplines;") %>% data.table
disciplines <- disciplines[Code_Discipline>=101 & Code_Discipline<=109, ESpecialite:="Psychology"]
disciplines <- disciplines[Code_Discipline==18, ESpecialite:="General Sciences"]
disciplines <- disciplines[Code_Discipline==51, ESpecialite:="Neurosciences"]

disciplines_big <- dbGetQuery(ESH, "SELECT EGrande_Discipline, Code_Discipline FROM OST_Expanded_SciHum.Disciplines;") %>% data.table
disciplines_big <- disciplines_big[EGrande_Discipline=="Natural Sciences and Engineering", ESpecialite_big:="Other NSE"]
disciplines_big <- disciplines_big[EGrande_Discipline=="Social Sciences and Humanities", ESpecialite_big:="Other SSH"]

#DF of authors
all_aut <- fread("BD_VM/EXTENDED_AUTHORS.csv", quote="")
colnames(all_aut)[colnames(all_aut) == "Nom_ISI"] <- "Nom"

#DF of authors
all_aut_ad <- fread("BD_VM/EXTENDED_AUTHORS_ADRESS.csv", quote="")
colnames(all_aut_ad)[colnames(all_aut) == "Nom_ISI"] <- "Nom"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Main Corpus and Nodes ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### Importing and Cleaning **********************
BE_extended <- fread("BD_VM/EXTENDED.csv", quote="")
#only after 45
BE_extended <- BE_extended[Annee_Bibliographique>1945]
BE_extended <- BE_extended[Annee_Bibliographique<2019]

#get the name of disciplines
BE_extended <- merge(BE_extended, disciplines, by="Code_Discipline")
BE_extended <- merge(BE_extended, disciplines_big, by="Code_Discipline")
#regroup disciplines 
BE_extended[,ESpecialite_grouped := ESpecialite_big]; BE_extended[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]
#not_econ
BE_extended[,econ_bin := "Not Econ"]; BE_extended[ESpecialite == "Economics", econ_bin := "Economics"]; BE_extended[ESpecialite == "Management", econ_bin := "Economics"]
#not_psy
BE_extended[,psy_bin := "Not Psychology"]; BE_extended[ESpecialite == "Psychology", psy_bin := "Psychology"]

#regroup Years
BE_extended[,annee_regrouped := "<1980"]
BE_extended[Annee_Bibliographique >= 1980 & Annee_Bibliographique < 1990,annee_regrouped := "80-89"]
BE_extended[Annee_Bibliographique >= 1990 & Annee_Bibliographique < 2000,annee_regrouped := "90-99"]
BE_extended[Annee_Bibliographique >= 2000 & Annee_Bibliographique < 2010,annee_regrouped := "00-09"]
BE_extended[Annee_Bibliographique >= 2010 & Annee_Bibliographique < 2020,annee_regrouped := "10-19"]
BE_extended[annee_regrouped=="10-19"]
######################### Count Stuff **********************
BE_extended[,articles_by_year:=.N, Annee_Bibliographique]
BE_extended[,disciplines_by_year:=.N, .(Annee_Bibliographique, ESpecialite_grouped)]
BE_extended[,share_of_discipline:=disciplines_by_year/articles_by_year]

#getting autor-year (88982)
BE_extended <- merge(BE_extended, all_aut[Ordre==1,.(ID_Art, Nom)], by = "ID_Art", all.x = TRUE, all.y = FALSE)
BE_extended <- BE_extended[, name_short:= paste0(unlist(strsplit(Nom,"-"))[1],"-", substr(unlist(strsplit(Nom,"-"))[2],1,1) 
), by=list(ID_Art,Nom)]
BE_extended$name_short <- toupper(BE_extended$name_short)
BE_extended <- BE_extended[,Label:=paste0(name_short,",",Annee_Bibliographique)]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Autocitations ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### Importing and Cleaning**********************
BE_core_extended_autocit <- read.csv(file = 'BD_VM/EXTENDED_AUTOCIT.csv', header = TRUE, sep = "|") %>% data.table
#changing some names
colnames(BE_core_extended_autocit)[colnames(BE_core_extended_autocit) == "ID_Art_Source"] <- "Source"
colnames(BE_core_extended_autocit)[colnames(BE_core_extended_autocit) == "ID_Art_Target"] <- "Target"
#getting only autocitations
BE_core_extended_autocit <- BE_core_extended_autocit[Source %in% BE_extended$ID_Art & Target %in% BE_extended$ID_Art]
#export edges
write.csv(BE_core_extended_autocit, file = "Networks/edges_BE_extended_autocit.csv", row.names=FALSE)
#export nodes (after changing)
colnames(BE_extended)[colnames(BE_extended) == "ID_Art"] <- "Id"
BE_extended_nodes <- BE_extended[,.(Id, ESpecialite, ESpecialite_grouped, Annee_Bibliographique, Titre, Label)]
write.csv(BE_extended_nodes, file = "Networks/nodes_BE_extended_autocit.csv", row.names=FALSE)
colnames(BE_extended)[colnames(BE_extended) == "Id"] <- "ID_Art"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Cocitations####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### Importing and Cleaning **********************
cocitation <- fread("BD_VM/EXTENDED_COCIT.csv", quote="", sep = "|")
#some weird years in there
cocitation <- cocitation[Annee_Bibliographique!=""]
cocitation <- cocitation[Annee_Bibliographique!="Annee_Bibliographique"]
cocitation[,.N,Annee_Bibliographique]
#get disciplines
cocitation <- merge(cocitation, disciplines, by="Code_Discipline")
cocitation[,ESpecialite_grouped := "Other"]; cocitation[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]

######################### All Nodes **********************
cocitation_nodes <- cocitation[,.(Annee_Target, Nom_Target, ESpecialite_grouped), .(ItemID_Ref_Target)]
setkey(cocitation_nodes, ItemID_Ref_Target, Annee_Target)
#get mode year to reduce the probability of a mistake
cocitation_nodes[,Annee_mode:=Mode(Annee_Target), ItemID_Ref_Target][,Annee_Target:=NULL]
#keep one of each
cocitation_nodes <- cocitation_nodes[, head(.SD, 1), .(ItemID_Ref_Target)]
#name for gephi
colnames(cocitation_nodes)[colnames(cocitation_nodes) == "ItemID_Ref_Target"] <- "Id"

######################### All Edges per Decades **********************
#cocitation edges
cocitation_edges <- function(x, y ### x=df of corpus, y=minimum number of connections
){
  cocitation_edgesf <- x[,.(ID_Art_Source, ItemID_Ref_Target)]
  setkey(cocitation_edgesf, ID_Art_Source, ItemID_Ref_Target)
  # dropping articles citing only one document
  cocitation_edgesf <-  cocitation_edgesf[,nb_ref2 :=.N,by=ID_Art_Source][nb_ref2>1][,nb_ref2:=NULL]
  # removing duplicated citations with exactly the same source and target
  cocitation_edgesf <- unique(cocitation_edgesf)
  # creating all links between cocited docs
  cocitation_edgesf <- cocitation_edgesf[,list(Target = rep(ItemID_Ref_Target[1:(length(ItemID_Ref_Target)-1)],(length(ItemID_Ref_Target)-1):1)
                                               , Source = rev(ItemID_Ref_Target)[sequence((length(ItemID_Ref_Target)-1):1)])
                                         ,by = ID_Art_Source]
  # remove loop
  cocitation_edgesf <- cocitation_edgesf[Source!=Target]
  # counting the number of identical links across citing articles
  cocitation_edgesf <- cocitation_edgesf[,.N, .(Source,Target)]
  cocitation_edgesf <- cocitation_edgesf[N>y]
  # fetching the number of citations per cited document (new_id2)
  new_id2_w_nb_cit <-  x[,.(nb_cit =.N),ItemID_Ref_Target]
  # getting the number of citations for all Target
  cocitation_edgesf <-  merge(cocitation_edgesf,new_id2_w_nb_cit,by.x = "Target",by.y="ItemID_Ref_Target")
  setnames(cocitation_edgesf,"nb_cit","nb_cit_Target")
  # getting the number of citations for all Source
  cocitation_edgesf <-  merge(cocitation_edgesf,new_id2_w_nb_cit,by.x = "Source",by.y="ItemID_Ref_Target")
  setnames(cocitation_edgesf,"nb_cit","nb_cit_Source")
  
  cocitation_edgesf[,weighted_edge := N/sqrt(nb_cit_Target*nb_cit_Source)]
  colnames(cocitation_edgesf)[colnames(cocitation_edgesf) == "weighted_edge"] <- "Weight"
  
  return(cocitation_edgesf) # utilser cette ligne pour sortir un objet.
}

#per year
cocitation[,annee_regrouped := "<1970"]
cocitation[Annee_Bibliographique >= 1970 & Annee_Bibliographique < 1980,annee_regrouped := "70-79"]
cocitation[Annee_Bibliographique >= 1980 & Annee_Bibliographique < 1990,annee_regrouped := "80-89"]
cocitation[Annee_Bibliographique >= 1990 & Annee_Bibliographique < 2000,annee_regrouped := "90-99"]
cocitation[Annee_Bibliographique >= 2000 & Annee_Bibliographique < 2010,annee_regrouped := "00-09"]
cocitation[Annee_Bibliographique >= 2010 & Annee_Bibliographique < 2020,annee_regrouped := "10-19"]
BE_extended_70 <- cocitation[annee_regrouped == "70-79"]
BE_extended_80 <- cocitation[annee_regrouped == "80-89"]
BE_extended_90 <- cocitation[annee_regrouped == "90-99"]
BE_extended_00 <- cocitation[annee_regrouped == "00-09"]
BE_extended_10 <- cocitation[annee_regrouped == "10-19"]
edges_BE_extented_cocit_70 <- cocitation_edges(BE_extended_70, 1) #217
edges_BE_extented_cocit_80 <- cocitation_edges(BE_extended_80, 1) #479
edges_BE_extented_cocit_90 <- cocitation_edges(BE_extended_90, 2) #1505
edges_BE_extented_cocit_00 <- cocitation_edges(BE_extended_00, 4) #6353
edges_BE_extented_cocit_10 <- cocitation_edges(BE_extended_10, 4) #18383
write.csv(edges_BE_extented_cocit_70, file = "Networks/edges_BE_extended_cocit_70.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_80, file = "Networks/edges_BE_extended_cocit_80.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_90, file = "Networks/edges_BE_extended_cocit_90.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_00, file = "Networks/edges_BE_extended_cocit_00.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_10, file = "Networks/edges_BE_extended_cocit_10.csv", row.names=FALSE)
BE_extended_70[ItemID_Ref_Target==1504209]
######################### All Nodes per Decades **********************
######################### All Nodes per Decades **********************
n_cit <- function(x, y ### x=df of corpus, y=minimum number of connections
){
  n_cit_source <- y[,.(Id = Source, nb_cit = nb_cit_Source)]
  n_cit_target <- y[,.(Id = Target, nb_cit = nb_cit_Target)]
  n_cit <- rbind(n_cit_source, n_cit_target)
  n_cit <- n_cit[, head(.SD, 1), Id]
  n_cit_final <-merge(x, n_cit, by = "Id")
  return(n_cit_final) # utilser cette ligne pour sortir un objet.
}


#cocitation_nodes <- cocitation_nodes[,Label:=paste0(Nom_mode,",",Annee_mode)]
nodes_BE_extented_cocit_70 <- cocitation_nodes[Id %in% edges_BE_extented_cocit_70$Target | Id %in% edges_BE_extented_cocit_70$Source]
nodes_BE_extented_cocit_70 <- n_cit(nodes_BE_extented_cocit_70, edges_BE_extented_cocit_70)

nodes_BE_extented_cocit_80 <- cocitation_nodes[Id %in% edges_BE_extented_cocit_80$Target | Id %in% edges_BE_extented_cocit_80$Source]
nodes_BE_extented_cocit_80 <- n_cit(nodes_BE_extented_cocit_80, edges_BE_extented_cocit_80)

nodes_BE_extented_cocit_90 <- cocitation_nodes[Id %in% edges_BE_extented_cocit_90$Target | Id %in% edges_BE_extented_cocit_90$Source]
nodes_BE_extented_cocit_90 <- n_cit(nodes_BE_extented_cocit_90, edges_BE_extented_cocit_90)

nodes_BE_extented_cocit_00 <- cocitation_nodes[Id %in% edges_BE_extented_cocit_00$Target | Id %in% edges_BE_extented_cocit_00$Source]
nodes_BE_extented_cocit_00 <- n_cit(nodes_BE_extented_cocit_00, edges_BE_extented_cocit_00)

nodes_BE_extented_cocit_10 <- cocitation_nodes[Id %in% edges_BE_extented_cocit_10$Target | Id %in% edges_BE_extented_cocit_10$Source]
nodes_BE_extented_cocit_10 <- n_cit(nodes_BE_extented_cocit_10, edges_BE_extented_cocit_10)
label <- function(x, y ### x=df of corpus, y=minimum number of connections
){
  label <- x[, Nom_Target:=as.character(Nom_Target)]
  label <- label[, name_short:= paste0(unlist(strsplit(Nom_Target,"-"))[1],"-", substr(unlist(strsplit(Nom_Target,"-"))[2],1,1) ), by=list(Id,Nom_Target)]
  label <- label[,Label:=paste0(name_short,",",Annee_mode)]
  return(label) # utilser cette ligne pour sortir un objet.
}
nodes_BE_extented_cocit_70 <- label(nodes_BE_extented_cocit_70)
nodes_BE_extented_cocit_80 <- label(nodes_BE_extented_cocit_80)
nodes_BE_extented_cocit_90 <- label(nodes_BE_extented_cocit_90)
nodes_BE_extented_cocit_00 <- label(nodes_BE_extented_cocit_00)
nodes_BE_extented_cocit_10 <- label(nodes_BE_extented_cocit_10)
write.csv(nodes_BE_extented_cocit_70, file = "Networks/nodes_BE_extended_cocit_70.csv", row.names=FALSE)
write.csv(nodes_BE_extented_cocit_80, file = "Networks/nodes_BE_extended_cocit_80.csv", row.names=FALSE)
write.csv(nodes_BE_extented_cocit_90, file = "Networks/nodes_BE_extended_cocit_90.csv", row.names=FALSE)
write.csv(nodes_BE_extented_cocit_00, file = "Networks/nodes_BE_extended_cocit_00.csv", row.names=FALSE)
write.csv(nodes_BE_extented_cocit_10, file = "Networks/nodes_BE_extended_cocit_10.csv", row.names=FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Coupling ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### Importing and Cleaning **********************
coupling <- fread("BD_VM/EXTENDED_COCIT.csv", quote="", sep = "|")
#some weird years in there
coupling <- coupling[Annee_Bibliographique!=""]
coupling <- coupling[Annee_Bibliographique!="Annee_Bibliographique"]
coupling[,.N,Annee_Bibliographique]
#get disciplines
coupling <- merge(coupling, disciplines, by="Code_Discipline")
coupling[,ESpecialite_grouped := "Other"]; coupling[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]
#doubles and such
coupling <-  coupling[,nb_ref2 :=.N,by=ID_Art_Source][nb_ref2>1][,nb_ref2:=NULL]
coupling <-  coupling[,nb_ref2 :=.N,by=ItemID_Ref_Target][nb_ref2>1][,nb_ref2:=NULL]
coupling[,.N,.(ID_Art_Source, ItemID_Ref_Target)][order(N)]
coupling <-coupling[, head(.SD, 1), .(ID_Art_Source,ItemID_Ref_Target)]

######################### All Nodes **********************
coupling_nodes <- fread("BD_VM/EXTENDED.csv", quote="")
#name for gephi
colnames(coupling_nodes)[colnames(coupling_nodes) == "ID_Art"] <- "Id"
#get disciplines
coupling_nodes <- merge(coupling_nodes, disciplines, by="Code_Discipline")
coupling_nodes[,ESpecialite_grouped := "Other"]; coupling_nodes[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]
######################### All Edges per Decades **********************
#couplin edges

coupling_edges <- function(x, y ### x=df of corpus, y=minimum number of connections
){
  cocitation_edgesf <- x[,.(ID_Art_Source, ItemID_Ref_Target)]
  setkey(cocitation_edgesf, ItemID_Ref_Target, ID_Art_Source)
  # dropping articles citing only one document
  cocitation_edgesf <-  cocitation_edgesf[,nb_ref2 :=.N,by=ID_Art_Source][nb_ref2>1][,nb_ref2:=NULL]
  cocitation_edgesf <-  cocitation_edgesf[,nb_ref2 :=.N,by=ItemID_Ref_Target][nb_ref2>1][,nb_ref2:=NULL]
  # removing duplicated citations with exactly the same source and target
  cocitation_edgesf <- unique(cocitation_edgesf)
  # creating all links between cocited docs
  cocitation_edgesf <- cocitation_edgesf[,list(Target = rep(ID_Art_Source[1:(length(ID_Art_Source)-1)],(length(ID_Art_Source)-1):1)
                                               , Source = rev(ID_Art_Source)[sequence((length(ID_Art_Source)-1):1)])
                                         ,by = ItemID_Ref_Target]
  # remove loop
  cocitation_edgesf <- cocitation_edgesf[Source!=Target]
  # counting the number of identical links across citing articles
  cocitation_edgesf <- cocitation_edgesf[,.N, .(Source,Target)]
  cocitation_edgesf <- cocitation_edgesf[N>y]
  # fetching the number of citations per cited document (new_id2)
  new_id2_w_nb_cit <-  x[,.(nb_cit =.N),ID_Art_Source]
  # getting the number of citations for all Target
  cocitation_edgesf <-  merge(cocitation_edgesf,new_id2_w_nb_cit,by.x = "Target",by.y="ID_Art_Source")
  setnames(cocitation_edgesf,"nb_cit","nb_cit_Target")
  # getting the number of citations for all Source
  cocitation_edgesf <-  merge(cocitation_edgesf,new_id2_w_nb_cit,by.x = "Source",by.y="ID_Art_Source")
  setnames(cocitation_edgesf,"nb_cit","nb_cit_Source")
  
  cocitation_edgesf[,weighted_edge := N/sqrt(nb_cit_Target*nb_cit_Source)]
  colnames(cocitation_edgesf)[colnames(cocitation_edgesf) == "weighted_edge"] <- "Weight"
  
  return(cocitation_edgesf) # utilser cette ligne pour sortir un objet.
}
edges_BE_extented_coup <- coupling_edges(coupling, 10) #217
write.csv(edges_BE_extented_coup, file = "Networks/edges_BE_extended_coup.csv", row.names=FALSE)

nodes_BE_extented_coup <- coupling_nodes[Id %in% edges_BE_extented_coup$Target | Id %in% edges_BE_extented_coup$Source]
nodes_BE_extented_coup <- n_cit(nodes_BE_extented_coup, edges_BE_extented_coup)
write.csv(nodes_BE_extented_coup, file = "Networks/nodes_BE_extended_coup.csv", row.names=FALSE)

#per year
coupling[,annee_regrouped := "<1980"]
coupling[Annee_Bibliographique >= 1980 & Annee_Bibliographique < 1990,annee_regrouped := "80-89"]
coupling[Annee_Bibliographique >= 1990 & Annee_Bibliographique < 2000,annee_regrouped := "90-99"]
coupling[Annee_Bibliographique >= 2000 & Annee_Bibliographique < 2010,annee_regrouped := "00-09"]
coupling[Annee_Bibliographique >= 2010 & Annee_Bibliographique < 2020,annee_regrouped := "10-19"]
BE_extended_70 <- coupling[annee_regrouped == "<1980"]
BE_extended_80 <- coupling[annee_regrouped == "80-89"]
BE_extended_90 <- coupling[annee_regrouped == "90-99"]
BE_extended_00 <- coupling[annee_regrouped == "00-09"]
BE_extended_10 <- coupling[annee_regrouped == "10-19"]
edges_BE_extented_coup_70 <- coupling_edges(BE_extended_70, 2) #217
edges_BE_extented_coup_80 <- coupling_edges(BE_extended_80, 2) #479
edges_BE_extented_coup_90 <- coupling_edges(BE_extended_90, 2) #1505
edges_BE_extented_coup_00 <- coupling_edges(BE_extended_00, 5) #6353
edges_BE_extented_coup_10 <- coupling_edges(BE_extended_10, 10) #18383
write.csv(edges_BE_extented_coup_70, file = "Networks/edges_BE_extended_coup_70.csv", row.names=FALSE)
write.csv(edges_BE_extented_coup_80, file = "Networks/edges_BE_extended_coup_80.csv", row.names=FALSE)
write.csv(edges_BE_extented_coup_90, file = "Networks/edges_BE_extended_coup_90.csv", row.names=FALSE)
write.csv(edges_BE_extented_coup_00, file = "Networks/edges_BE_extended_coup_00.csv", row.names=FALSE)
write.csv(edges_BE_extented_coup_10, file = "Networks/edges_BE_extended_coup_10.csv", row.names=FALSE)

######################### All Nodes per Decades **********************
n_cit <- function(x, y ### x=df of corpus, y=minimum number of connections
){
  n_cit_source <- y[,.(Id = Source, nb_cit = nb_cit_Source)]
  n_cit_target <- y[,.(Id = Target, nb_cit = nb_cit_Target)]
  n_cit <- rbind(n_cit_source, n_cit_target)
  n_cit <- n_cit[, head(.SD, 1), Id]
  n_cit_final <-merge(x, n_cit, by = "Id")
  return(n_cit_final) # utilser cette ligne pour sortir un objet.
}


#cocitation_nodes <- cocitation_nodes[,Label:=paste0(Nom_mode,",",Annee_mode)]
nodes_BE_extented_coup_70 <- coupling_nodes[Id %in% edges_BE_extented_coup_70$Target | Id %in% edges_BE_extented_coup_70$Source]
nodes_BE_extented_coup_70 <- n_cit(nodes_BE_extented_coup_70, edges_BE_extented_coup_70)

nodes_BE_extented_coup_80 <- coupling_nodes[Id %in% edges_BE_extented_coup_80$Target | Id %in% edges_BE_extented_coup_80$Source]
nodes_BE_extented_coup_80 <- n_cit(nodes_BE_extented_coup_80, edges_BE_extented_coup_80)

nodes_BE_extented_coup_90 <- coupling_nodes[Id %in% edges_BE_extented_coup_90$Target | Id %in% edges_BE_extented_coup_90$Source]
nodes_BE_extented_coup_90 <- n_cit(nodes_BE_extented_coup_90, edges_BE_extented_coup_90)

nodes_BE_extented_coup_00 <- coupling_nodes[Id %in% edges_BE_extented_coup_00$Target | Id %in% edges_BE_extented_coup_00$Source]
nodes_BE_extented_coup_00 <- n_cit(nodes_BE_extented_coup_00, edges_BE_extented_coup_00)

nodes_BE_extented_coup_10 <- coupling_nodes[Id %in% edges_BE_extented_coup_10$Target | Id %in% edges_BE_extented_coup_10$Source]
nodes_BE_extented_coup_10 <- n_cit(nodes_BE_extented_coup_10, edges_BE_extented_coup_10)

label <- function(x, y ### x=df of corpus, y=minimum number of connections
){
  label <- x[, Nom_Target:=as.character(Nom_Target)]
  label <- label[, name_short:= paste0(unlist(strsplit(Nom_Target,"-"))[1],"-", substr(unlist(strsplit(Nom_Target,"-"))[2],1,1) ), by=list(Id,Nom_Target)]
  label <- label[,Label:=paste0(name_short,",",Annee_mode)]
  return(label) # utilser cette ligne pour sortir un objet.
}
#nodes_BE_extented_cocit_70 <- label(nodes_BE_extented_cocit_70)
#nodes_BE_extented_cocit_80 <- label(nodes_BE_extented_cocit_80)
#nodes_BE_extented_cocit_90 <- label(nodes_BE_extented_cocit_90)
#nodes_BE_extented_cocit_00 <- label(nodes_BE_extented_cocit_00)
#nodes_BE_extented_cocit_10 <- label(nodes_BE_extented_cocit_10)
write.csv(nodes_BE_extented_coup_70, file = "Networks/nodes_BE_extended_coup_70.csv", row.names=FALSE)
write.csv(nodes_BE_extented_coup_80, file = "Networks/nodes_BE_extended_coup_80.csv", row.names=FALSE)
write.csv(nodes_BE_extented_coup_90, file = "Networks/nodes_BE_extended_coup_90.csv", row.names=FALSE)
write.csv(nodes_BE_extented_coup_00, file = "Networks/nodes_BE_extended_coup_00.csv", row.names=FALSE)
write.csv(nodes_BE_extented_coup_10, file = "Networks/nodes_BE_extended_coup_10.csv", row.names=FALSE)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART II : Plotting our corpus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#colors
Communities[,.N,Cluster]
MyPalette <- c(Economics = "#f68d87", Management = "#91ba2e", Psychology = "#00bfc4", "General Sciences" =  "#9892ff"
               , "Other NSE" = "#fb61d7", "Other SSH" = "#01c094", Law = "#ee37e2", Neurosciences = "#31b4f6")
MyPalette_com <- c("5-Intertemporal Choice" = "#9691ff", "1-Social Preferences" = "#f8766d", "4-Risk and Uncertainty" = "#02b0f6")


BE_extended[,.N,ESpecialite_grouped]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Corpus Information ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
setkey(BE_extended, Annee_Bibliographique)
######################### Histogram **********************
ggplot(BE_extended, aes(x=Annee_Bibliographique)) + 
  geom_histogram(color="black", fill="white")
######################### Graph **********************
ggplot(BE_extended[,.(.N), .(Annee_Bibliographique)][order(N)]
       , aes(x=Annee_Bibliographique, y=N)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1970, 2018)) +
  scale_y_continuous("Number of Articles")
ggsave("Graphs/Corpus2.png", width=286, height=215, units = "mm")
######################### Table **********************
BE_extended[,.(.N), .(Annee_Bibliographique)][order(N)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Interdisciplinarity ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
setkey(BE_extended, Annee_Bibliographique, ESpecialite_grouped)
######################### Tables **********************
#seeing distribution of disciplines of our core extended
BE_extended[,.(.N), .(ESpecialite)][order(N)]

######################### Histogram **********************
#seeing distribution by disciplines of our core extended
ggplot(BE_extended, aes(x=Annee_Bibliographique, color=ESpecialite_grouped, fill=ESpecialite_grouped)) +
  geom_histogram()

######################### Smooth **********************
#all disciplines
BE_extended_disc <- BE_extended[Annee_Bibliographique<2019][, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped)]
BE_extended_disc <- complete(BE_extended_disc, ESpecialite_grouped, Annee_Bibliographique) %>% as.data.table
BE_extended_disc <- BE_extended_disc[is.na(share_of_discipline),share_of_discipline:=0]
BE_extended_disc <- BE_extended_disc[,share_of_discipline2:=frollmean(share_of_discipline, 3), ESpecialite_grouped]

ggplot(BE_extended_disc, aes(x=Annee_Bibliographique, y=share_of_discipline2, group=ESpecialite_grouped, color=ESpecialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share of Discicpline") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = ESpecialite_grouped),
                   data = tail(BE_extended_disc[Annee_Bibliographique==2018,.N, .(ESpecialite_grouped, Annee_Bibliographique, share_of_discipline2)], 8),
                   nudge_x = 20,
                   na.rm = TRUE,
                   size = 4) +
  scale_colour_manual(values = MyPalette, guide = FALSE)
ggsave("Graphs/BE_disc.png", width=286, height=215, units = "mm")

#economics_bin
ggplot(BE_extended[,n_econ_bin:=.N, .(Annee_Bibliographique, econ_bin)]
       , aes(x=Annee_Bibliographique, y=n_econ_bin/articles_by_year, group=econ_bin, color=econ_bin)) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95, span = 0.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2018)) +
  scale_y_continuous("Share disicpline")
#psychology_bin
ggplot(BE_extended[,n_psy_bin_bin:=.N, .(Annee_Bibliographique, psy_bin)]
       , aes(x=Annee_Bibliographique, y=n_psy_bin_bin/articles_by_year, group=psy_bin, color=psy_bin)) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95, span = 0.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2018)) +
  scale_y_continuous("Share disicpline")

#number of disc
ggplot(BE_extended[, head(.SD, 1), .(Annee_Bibliographique, ESpecialite)][,.N, .(Annee_Bibliographique)]
       , aes(x=Annee_Bibliographique, y=N)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2018)) +
  scale_y_continuous("Number of discipline")
ggsave("Graphs/BE_diversity.png", width=286, height=215, units = "mm")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Authors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### Corpus **********************
#the corpus to compute
authors_of_extended_core <- merge(BE_extended, all_aut[,.(ID_Art, Nom)], by = "ID_Art", all.x = TRUE, all.y = FALSE)
#Remove second name
authors_of_extended_core <- authors_of_extended_core[, name_short:= paste0(unlist(strsplit(Nom,"-"))[1],"-", substr(unlist(strsplit(Nom,"-"))[2],1,1) 
), by=list(ID_Art,Nom)]
#Upper all
authors_of_extended_core$name_short <- toupper(authors_of_extended_core$name_short)
#and have more than 4 letters
authors_of_extended_core[,name_lenght:= nchar(name_short)]
authors_of_extended_core <- authors_of_extended_core[name_lenght>5]

######################### Smooth v
#top 10
ggplot(authors_of_extended_core[name_short != "NA-NA"][name_short !="LI-J"][name_short !="LI-S"][name_short !="WANG-Y"][name_short !="ZHANG-Y"]
       [,.(.N), name_short][order(N)][,tail(.SD,10)], aes(x=name_short, y=N)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="tomato2") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Institutions (IN TESTING) ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
institutions_of_extended_core <- merge(BE_core_extended_autocit, all_aut_ad[,.(ID_Art, Institution)], by.x = "Source", by.y = "ID_Art", allow.cartesian=TRUE)
setnames(institutions_of_extended_core,"Institution", "Institution_Source")
institutions_of_extended_core <- merge(institutions_of_extended_core, all_aut_ad[,.(ID_Art, Institution)], by.x = "Target", by.y = "ID_Art", allow.cartesian=TRUE)
setnames(institutions_of_extended_core,"Institution", "Institution_Target")
write.csv(institutions_of_extended_core, file = "Networks/institutions_edges.csv", row.names=FALSE)
institution_nodes <- BE_extended[Id %in% institutions_of_extended_core$Target | Id %in% institutions_of_extended_core$Source]
write.csv(institution_nodes, file = "Networks/institutions_nodes.csv", row.names=FALSE)

institutions <- merge(all_aut_ad, BE_extended[,.(ID_Art, Annee_Bibliographique)], by="ID_Art")
institutions <- institutions[,n_inst_tot:=.N, Pays][order(-n_inst_tot)]
institutions <- institutions[,country_n:=.N,.(Annee_Bibliographique,Pays)][order(country_n)]
institutions <- institutions[,tot:=.N, Annee_Bibliographique]
institutions <- institutions[,share_country:=country_n/tot, Annee_Bibliographique]
institutions <- institutions[, head(.SD, 1), .(Annee_Bibliographique, Pays)]


institutions <- complete(institutions, Pays, Annee_Bibliographique = 1980:2018) %>% as.data.table
institutions <- institutions[is.na(share_country),share_country:=0]

institutions <- institutions[,share_country2:=frollmean(share_country, 3), Pays]

ggplot(institutions
       , aes(x=Annee_Bibliographique, y=country_n, group=Pays, color=Pays)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.9)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(2000, 2018)) +
  scale_y_continuous("Number of discipline")

ggplot(institutions[n_inst_tot>420]
       , aes(x=Annee_Bibliographique, y=share_country2, group=Pays, color=Pays)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1990, 2020)) +
  scale_y_continuous("Share of authors affiliated to a given country") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = Pays),
                   data = tail(institutions[n_inst_tot>420][Annee_Bibliographique==2018,.N, .(Annee_Bibliographique, Pays, share_country2)], 5),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size = 4,
                   direction="both")
ggsave("Graphs/BE_share_country.png", width=286, height=215, units = "mm")


institutions <- merge(all_aut_ad, BE_extended[,.(ID_Art, Annee_Bibliographique)], by="ID_Art")
institutions <- institutions[,n_inst_tot:=.N, Institution][order(-n_inst_tot)][n_inst_tot>300]
institutions <- institutions[,country_n:=.N,.(Annee_Bibliographique,Institution)][order(country_n)]

ggplot(institutions
       , aes(x=Annee_Bibliographique, y=country_n, group=Institution, color=Institution)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.9)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(2000, 2018)) +
  scale_y_continuous("Number of discipline")





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART IV : ANALYSIS OF COMMUNITIES  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Communities[especialite_grouped=="Law",.N/Communities[especialite_grouped=="Law",.N], modularity_class]
Communities[modularity_class=="6-Neuroeconomics",.N/Communities[modularity_class=="6-Neuroeconomics",.N], especialite_grouped]
Communities[,.N/Communities[,.N], modularity_class][order(V1)]
Communities[N]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Finding the lineage of the corpus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#most important articles by communities
Communities <- read.csv(file = "Networks/redo_Communities.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
#paper from the community
Communities <- Communities[modularity_class==0, .(Id)]
#get the citations from the community
Communities <- BE_core_extended_autocit[Source %in% Communities$Id]
#info about articles
Communities <- merge(Communities, BE_extended_nodes[,.(Id, Annee_Bibliographique, Titre)], by.x="Target", by.y = "Id")
#degree
Communities <- Communities[,local_degree:=.N,Target][order(local_degree)]
Communities[order(local_degree)]
#by decades
Communities[,annee_regrouped := "<1980"]
Communities[Annee_Bibliographique >= 1980 & Annee_Bibliographique < 1990,annee_regrouped := "80-89"]
Communities[Annee_Bibliographique >= 1990 & Annee_Bibliographique < 2000,annee_regrouped := "90-99"]
Communities[Annee_Bibliographique >= 2000 & Annee_Bibliographique < 2010,annee_regrouped := "00-09"]
Communities[Annee_Bibliographique >= 2010 & Annee_Bibliographique < 2020,annee_regrouped := "10-19"]

setkey(Communities, annee_regrouped, local_degree)
Communities[,tail(.SD, 1), annee_regrouped][order(Annee_Bibliographique)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Taking a look at our communities ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### setting things **********************
Communities <- read.csv(file = "Networks/redo_Communities.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
Communities <- Communities[especialite_grouped=="General Biomedical Research", especialite_grouped:="General Sciences"]
Communities <- Communities[especialite_grouped=="Neurology & Neurosurgery", especialite_grouped:="Neurosciences"]

Communities <- Communities[,modularity_class:=modularity_class+1]
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
#name of communities
Communities[modularity_class==1, modularity_class:= "1-Social Preferences"]
Communities[modularity_class==2, modularity_class:= "2-Biases, Heuristics and Nudges"]
Communities[modularity_class==3, modularity_class:= "3-Behavioral Finance"]
Communities[modularity_class==4, modularity_class:= "4-Risk and Uncertainty"]
Communities[modularity_class==5, modularity_class:= "5-Intertemporal Choice"]
Communities[modularity_class==6, modularity_class:= "6-Neuroeconomics"]
Communities[modularity_class==7, modularity_class:= "7-Behavioral GT"]
Communities[annee_bibliographique<2019]

Communities[,mean(clustering), especialite_grouped][order(V1)]
Communities[,.N/Communities[,.N], modularity_class][order(V1)]
Communities[,mean(Hub), especialite_grouped][order(V1)]
Communities[,mean(eigencentrality), especialite_grouped][order(V1)]
Communities[,mean(clustering), especialite_grouped][order(V1)]

ggplot(Communities[modularity_class!="7-Behavioral GT" & annee_bibliographique>=1980], aes(x=annee_bibliographique)) + geom_histogram() + facet_wrap(~modularity_class)
ggsave("Graphs/histo_com.png", width=286, height=215, units = "mm")

Communities[modularity_class=="3-Behavioral Finance" & annee_bibliographique==1983]
Communities[annee_bibliographique>=1980 & annee_bibliographique<1990]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Disciplines ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#disciplines
Communities[,psy_bin := "Not Psychology"]; Communities[especialite_grouped == "Psychology", psy_bin := "Psychology"]
Communities[,cog_bin := "Not Cog Sci"]; Communities[especialite_grouped == "Psychology", cog_bin := "Cog Sci"]; Communities[especialite_grouped == "Neurosciences", cog_bin := "Cog Sci"]; Communities[especialite_grouped == "General Sciences", cog_bin := "Cog Sci"]
Communities[,eco_bin := "Not Economics"]; Communities[especialite_grouped == "Economics", eco_bin := "Economics"]; Communities[especialite_grouped == "Management", eco_bin := "Economics"]

#total of articles by communities
Communities[,tot_com:=.N, modularity_class]
Communities[,tot_com_year:=.N, .(annee_bibliographique, modularity_class)]

#share of eco in each community
Communities[eco_bin=="Economics", n_eco_bin:=.N, .(annee_bibliographique, modularity_class)] #number of artices in economics
Communities[eco_bin=="Not Economics", n_eco_bin:=(tot_com_year-.N), .(annee_bibliographique, modularity_class)] # copy the number for non eocnomics_articles
Communities[,share_eco_bin:=(n_eco_bin/tot_com_year),  .(annee_bibliographique, modularity_class)]

Communities_eco <- Communities[annee_bibliographique<2019][modularity_class!="7-Behavioral GT"][, head(.SD, 1), .(annee_bibliographique,modularity_class, share_eco_bin)]

Communities_eco <- complete(Communities_eco, modularity_class, annee_bibliographique) %>% as.data.table
Communities_eco <- Communities_eco[,share_eco_bin2:=frollmean(share_eco_bin, 3), modularity_class]

ggplot(Communities_eco
       [annee_bibliographique<2019][modularity_class!="7-Behavioral GT"]
       , aes(x=annee_bibliographique, y=share_eco_bin2, group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2022)) +
  scale_y_continuous("Share of Economics and Management", limits =c(0, 1)) +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = modularity_class),
                   data = tail(Communities_eco[annee_bibliographique==2018 & modularity_class!= "9-Behavioral GT" & modularity_class!= "10-Hapiness BE"
                                               ,.N, .(modularity_class, annee_bibliographique, n_eco_bin, tot_com_year, share_eco_bin2)], 8),
                   nudge_x = 2,
                   na.rm = TRUE,
                   size = 4,
                   direction="both")
ggsave("Graphs/BE_share_eco.png", width=286, height=215, units = "mm")




#share of cog in each community
Communities[cog_bin=="Cog Sci", n_cog_bin:=.N, .(annee_bibliographique, modularity_class)] #same
Communities[cog_bin=="Not Cog Sci", n_cog_bin:=(tot_com_year-.N), .(annee_bibliographique, modularity_class)]
Communities[,share_cog_bin:=(n_cog_bin/tot_com_year),  .(annee_bibliographique, modularity_class)]

Communities_cog <- Communities[annee_bibliographique<2019][modularity_class!="7-Behavioral GT"][, head(.SD, 1), .(annee_bibliographique,modularity_class)]

Communities_cog <- complete(Communities_cog, modularity_class, annee_bibliographique) %>% as.data.table
Communities_cog <- Communities_cog[,share_cog_bin2:=frollmean(share_cog_bin, 3), modularity_class]

ggplot(Communities_cog
       , aes(x=annee_bibliographique, y=share_cog_bin2, group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2022)) +
  scale_y_continuous("Share of Cognitive Sciences and General Sciences")  +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = modularity_class),
                   data = tail(Communities_cog[annee_bibliographique==2018 & modularity_class!= "9-Behavioral GT" & modularity_class!= "10-Hapiness BE"
                                               ,.N, .(modularity_class, annee_bibliographique, share_cog_bin2)], 8),
                   nudge_x = 2,
                   na.rm = TRUE,
                   size = 4,
                   direction="both")
ggsave("Graphs/BE_share_cog.png", width=286, height=215, units = "mm")
Communities_cog[modularity_class=="4-Risk and Uncertainty"]

#seeing distribution by disciplines of our core extended
gg_plot_communities <- Communities[,n_articles_by_year:=.N, .(annee_bibliographique, modularity_class)]
gg_plot_communities <- gg_plot_communities[,tot_n_articles_by_year:=.N, .(annee_bibliographique)]
gg_plot_communities[,share_articles_by_year:=.N/n_articles_by_year, .(annee_bibliographique, modularity_class, especialite_grouped)]

gg_plot_communities_disc <- gg_plot_communities[especialite_grouped!="Other",.N,.(modularity_class, especialite_grouped)][order(N)][,tail(.SD, 3), modularity_class]
gg_plot_communities[,tot_com:=.N, modularity_class]
gg_plot_communities_disc <- merge(gg_plot_communities_disc, gg_plot_communities[,.(modularity_class, tot_com)][,tail(.SD, 1), modularity_class], by = "modularity_class")
gg_plot_communities_disc[,share_disc_com:=N/tot_com]

######################### Explore **********************
#order for graph
gg_plot_communities_disc <- gg_plot_communities_disc[, share_disc_com := as.numeric(share_disc_com)]
gg_plot_communities_disc[, ord := sprintf("%02i", frank(gg_plot_communities_disc, modularity_class, share_disc_com, ties.method = "first"))]
gg_plot_communities_disc <- gg_plot_communities_disc[especialite_grouped=="General Biomedical Research", especialite_grouped:="General Sciences"]
gg_plot_communities_disc <- gg_plot_communities_disc[especialite_grouped=="Neurology & Neurosurgery", especialite_grouped:="Neurosciences"]
gg_plot_communities_disc <- gg_plot_communities_disc[mixedorder(modularity_class)]
gg_plot_communities_disc$modularity_class <- factor(gg_plot_communities_disc$modularity_class,levels=unique(gg_plot_communities_disc$modularity_class))


#Most important disicpline by community
ggplot(gg_plot_communities_disc, 
       aes(x=ord, y=share_disc_com, fill=modularity_class))+
  geom_bar(stat='identity') + 
  facet_wrap(~modularity_class, ncol = 5, scales = "free_y") + 
  scale_x_discrete(labels = gg_plot_communities_disc[, setNames(as.character(especialite_grouped), ord)]) + 
  guides(fill = FALSE) +
  coord_flip()

#Most important articles
setkey(gg_plot_communities, modularity_class, indegree)

gg_plot_communities[,tail(.SD, 3), modularity_class][,.(modularity_class, titre)]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Communities ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#useless
communities_BE_extended_plot1 <- ggplot(gg_plot_communities[annee_bibliographique>=1980 & annee_bibliographique<2015]
                                        , aes(x=annee_bibliographique, y=share_articles_by_year, group=especialite_grouped, color=especialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles") +
  facet_grid(rows = vars(modularity_class))

#number of articles by communities
gg_plot_communities_label_n <- gg_plot_communities[, head(.SD, 1), .(annee_bibliographique, modularity_class, n_articles_by_year)][annee_bibliographique==2018 ,Label_repel:=modularity_class]
communities_BE_extended_plot2 <- ggplot(gg_plot_communities_label_n[annee_bibliographique>=1980 & annee_bibliographique<2019]
                                        , aes(x=annee_bibliographique, y=n_articles_by_year, group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = Label_repel),
                   nudge_x = 20,
                   na.rm = TRUE,
                   size = 3)

#label + share of communities
setkey(gg_plot_communities, annee_bibliographique, modularity_class)
gg_plot_communities_label <- gg_plot_communities[, head(.SD, 1), .(annee_bibliographique, modularity_class, n_articles_by_year, tot_n_articles_by_year)][annee_bibliographique==2018 ,Label_repel:=modularity_class]
gg_plot_communities_label <- gg_plot_communities_label[annee_bibliographique>=1970]

gg_plot_communities_label <- complete(gg_plot_communities_label, modularity_class, annee_bibliographique) %>% as.data.table
setkey(gg_plot_communities_label, annee_bibliographique, modularity_class, n_articles_by_year)

gg_plot_communities_label <- gg_plot_communities_label[,prop:=n_articles_by_year/tot_n_articles_by_year]
gg_plot_communities_label <- gg_plot_communities_label[is.na(prop),prop:=0]

gg_plot_communities_label <- gg_plot_communities_label[,prop2:=frollmean(prop, 3), modularity_class]

ggplot(gg_plot_communities_label[annee_bibliographique>=1980 & annee_bibliographique<2019]
       , aes(x=annee_bibliographique, y=prop2, group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share of Communities") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = Label_repel),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 3) +
  coord_cartesian(ylim = c(0,0.5), expand = TRUE, default = FALSE, clip = "on" )
ggsave("Graphs/Communities_years.png", width=286, height=215, units = "mm")

#label + share of communities only in economics
gg_plot_communities <- gg_plot_communities[especialite_grouped=="Economics",n_articles_by_year_onlyeco:=.N, .(annee_bibliographique, modularity_class)]
gg_plot_communities <- gg_plot_communities[especialite_grouped=="Economics",n_tot_by_year_onlyeco:=.N, .(annee_bibliographique)]
gg_plot_communities_label <- gg_plot_communities[annee_bibliographique>=1970]
gg_plot_communities_label <- gg_plot_communities_label[, head(.SD, 1), .(annee_bibliographique, modularity_class, n_articles_by_year_onlyeco, n_tot_by_year_onlyeco)][annee_bibliographique==2018 ,Label_repel:=modularity_class]

gg_plot_communities_label <- complete(gg_plot_communities_label, modularity_class, annee_bibliographique) %>% as.data.table
setkey(gg_plot_communities_label, annee_bibliographique, modularity_class, n_articles_by_year_onlyeco)
gg_plot_communities_label <- gg_plot_communities_label[, tail(.SD, 1), .(annee_bibliographique, modularity_class)]


gg_plot_communities_label <- gg_plot_communities_label[,prop:=n_articles_by_year_onlyeco/n_tot_by_year_onlyeco]
gg_plot_communities_label <- gg_plot_communities_label[is.na(prop),prop:=0]

gg_plot_communities_label <- gg_plot_communities_label[,prop2:=frollmean(prop, 3), modularity_class]

ggplot(gg_plot_communities_label[annee_bibliographique>=1980 & annee_bibliographique<2019]
       , aes(x=annee_bibliographique, y=prop2, group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share of Communities") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = Label_repel),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 3) +
  coord_cartesian(ylim = c(0,0.5), expand = TRUE, default = FALSE, clip = "on" )
ggsave("Graphs/Communities_years_eco.png", width=286, height=215, units = "mm")

#label + share of disciplines
setkey(gg_plot_communities, annee_bibliographique, especialite_grouped)
gg_plot_communities[,n_disc_year:=.N, .(annee_bibliographique,especialite_grouped)]
gg_plot_communities[,tot_year:=.N, annee_bibliographique]
gg_plot_communities[,share_disc_year:=n_disc_year/tot_year]
#label
gg_plot_communities_label2 <- gg_plot_communities[, head(.SD, 1), .(annee_bibliographique, especialite_grouped, share_disc_year)][annee_bibliographique==2018 ,Label_repel2:=especialite_grouped]

communities_BE_extended_plot3 <- ggplot(gg_plot_communities_label2[annee_bibliographique>=1970 & annee_bibliographique<2019]
                                        , aes(x=annee_bibliographique, y=share_disc_year, group=especialite_grouped, color=especialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1970, 2019)) +
  scale_y_continuous("Share of disciplines in number of articles", limits = c(0,0.6)) +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = Label_repel2),
                   nudge_x = 20,
                   na.rm = TRUE,
                   size = 3)

plot9 <- communities_BE_extended_plot1
saveRDS(plot9, file = "Plots/plot9.RDS")

plot10 <- communities_BE_extended_plot2
saveRDS(plot10, file = "Plots/plot10.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Authors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
setkey(gg_plot_communities, modularity_class, indegree)
#topc aritlces
gg_plot_communities[, tail(.SD, 3), .(modularity_class)][,.(titre, modularity_class)]

#journals
gg_plot_communities_journals  <- gg_plot_communities[,.(Id,modularity_class,especialite,especialite_grouped)]
gg_plot_communities_journals <- merge(gg_plot_communities_journals, BE_extended[,.(Id=ID_Art, Code_Revue)], by.x = "Id", by.y = "Id", all.x = TRUE, all.y = FALSE)
gg_plot_communities_journals <- merge(gg_plot_communities_journals, revues[,.(Code_Revue, Revue)], by = "Code_Revue", all.x = TRUE, all.y = FALSE)
gg_plot_communities_journals[,.N , .(modularity_class,especialite_grouped, Revue)][order(N)][modularity_class %like% "3-"][,tail(.SD, 20)]

#mean year
gg_plot_communities[, mean(annee_bibliographique, na.rm=TRUE), modularity_class][order(V1)]

#top authors
gg_plot_communities_aut <- merge(gg_plot_communities, all_aut[,.(ID_Art, Nom)], by.x = "Id", by.y = "ID_Art", all.x = TRUE, all.y = FALSE)
Communities[modularity_class==1, modularity_class:= "1-Social Preferences"]
Communities[modularity_class==2, modularity_class:= "2-Biases, Heuristics and Nudges"]
Communities[modularity_class==3, modularity_class:= "3-Behavioral Finance"]
Communities[modularity_class==4, modularity_class:= "4-Risk and Uncertainty"]
Communities[modularity_class==5, modularity_class:= "5-Intertemporal Choice"]
Communities[modularity_class==6, modularity_class:= "6-Neuroeconomics"]
Communities[modularity_class==7, modularity_class:= "7-Behavioral GT"]
#Remove second name
gg_plot_communities_aut <- gg_plot_communities_aut[, name_short:= paste0(unlist(strsplit(Nom,"-"))[1],"-", substr(unlist(strsplit(Nom,"-"))[2],1,1) 
), by=list(Id,Nom)]
#Upper all
gg_plot_communities_aut$name_short <- toupper(gg_plot_communities_aut$name_short)
gg_plot_communities_aut <- gg_plot_communities_aut[,n_articles_by_aut:=.N, .(modularity_class, name_short)]
gg_plot_communities_aut <- gg_plot_communities_aut[,sum_citations_by_aut:=sum(indegree), .(modularity_class, name_short)]

#most present authors by communities
setkey(gg_plot_communities_aut, modularity_class, n_articles_by_aut)
gg_plot_communities_aut[name_short!= "NA-NA",.N,.(modularity_class, name_short)][,tail(.SD, 3), modularity_class]

ggplot(gg_plot_communities_aut[,.N,.(modularity_class, name_short)][,tail(.SD, 5), modularity_class], 
       aes(x=name_short, y=N))+
  geom_bar(stat='identity', fill="forest green") + 
  facet_wrap(~modularity_class, scales = "free_y") + 
  coord_flip()

#most cited authors by communities
setkey(gg_plot_communities_aut, modularity_class, sum_citations_by_aut)
gg_plot_communities_aut[name_short!= "NA-NA",.N,.(modularity_class, name_short, sum_citations_by_aut)][,tail(.SD, 3), modularity_class]

ggplot(gg_plot_communities_aut[name_short!= "NA-NA",.N,.(modularity_class, name_short, sum_citations_by_aut)][,tail(.SD, 5), modularity_class], 
       aes(x=name_short, y=N))+
  geom_bar(stat='identity', fill="forest green") + 
  facet_wrap(~modularity_class, scales = "free_y") + 
  coord_flip()


setkey(gg_plot_communities_aut, modularity_class, indegree)
gg_plot_communities_aut[name_short!= "NA-NA"][,sum(indegree),.(modularity_class, name_short)][order(V1)][,tail(.SD, 5), .(modularity_class)]
gg_plot_communities_aut_topindegree <- gg_plot_communities_aut[name_short!= "NA-NA"][,sum(indegree),.(modularity_class, name_short)][order(V1)][,tail(.SD, 5), .(modularity_class)]
gg_plot_communities_aut_topindegree <- gg_plot_communities_aut_topindegree[, V1 := as.numeric(V1)]
setkey(gg_plot_communities_aut_topindegree, modularity_class, V1)
gg_plot_communities_aut_topindegree[, ord := sprintf("%02i", frank(gg_plot_communities_aut_topindegree, modularity_class, V1, ties.method = "first"))]
gg_plot_communities_aut_topindegree <- gg_plot_communities_aut_topindegree[mixedorder(modularity_class)]
gg_plot_communities_aut_topindegree$modularity_class <- factor(gg_plot_communities_aut_topindegree$modularity_class,levels=unique(gg_plot_communities_aut_topindegree$modularity_class))

gg_plot_communities_aut[modularity_class=="5-Intertemporal Choice" & name_short %like% "RABIN"]
gg_plot_communities_aut[name_short %like% "SIMON"]

ggplot(gg_plot_communities_aut_topindegree, 
       aes(reorder_within (name_short, V1, modularity_class), V1, fill=modularity_class))+
  geom_bar(stat='identity') + 
  facet_wrap(~modularity_class, scales = "free_y", ncol = 5) + 
  scale_x_reordered() +
  guides(fill = FALSE) +
  coord_flip()
ggsave("Graphs/top_auth_indegree.png", width=286, height=215, units = "mm")


#order for graph
gg_plot_communities_disc <- gg_plot_communities_disc[, share_disc_com := as.numeric(share_disc_com)]
gg_plot_communities_disc[, ord := sprintf("%02i", frank(gg_plot_communities_disc, modularity_class, share_disc_com, ties.method = "first"))]
gg_plot_communities_disc <- gg_plot_communities_disc[especialite_grouped=="General Biomedical Research", especialite_grouped:="General Sciences"]
gg_plot_communities_disc <- gg_plot_communities_disc[especialite_grouped=="Neurology & Neurosurgery", especialite_grouped:="Neurosciences"]
gg_plot_communities_disc <- gg_plot_communities_disc[mixedorder(modularity_class)]
gg_plot_communities_disc$modularity_class <- factor(gg_plot_communities_disc$modularity_class,levels=unique(gg_plot_communities_disc$modularity_class))


#Most important disicpline by community
ggplot(gg_plot_communities_disc, 
       aes(x=ord, y=share_disc_com, fill=modularity_class))+
  geom_bar(stat='identity') + 
  facet_wrap(~modularity_class, ncol = 5, scales = "free_y") + 
  scale_x_discrete(labels = gg_plot_communities_disc[, setNames(as.character(especialite_grouped), ord)]) + 
  guides(fill = FALSE) +
  coord_flip()
ggsave("Graphs/disc_communities.png", width=286, height=215, units = "mm")





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### TF-IDF####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### tf **********************
Communities <- read.csv(file = "Networks/redo_Communities.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ID_Art"

#name of communities
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
require(tm)
require(bibliometrix)
require(RColorBrewer)
library(janeaustenr)
library(tidytext)
require(plyr)
require(dplyr)

Communities <- Communities %>% group_by(modularity_class) %>% summarise(titre = paste(titre, collapse=", "))
Communities <- VCorpus(VectorSource(Communities$titre))

Communities <- tm_map(Communities, stripWhitespace)
Communities <- tm_map(Communities, removePunctuation)
Communities <- tm_map(Communities, content_transformer(tolower))
Communities <- tm_map(Communities, removeWords, stopwords("english"))
Communities <- tm_map(Communities, stemDocument)

Communities <- DocumentTermMatrix(Communities)

#Communities <- DocumentTermMatrix(Communities, control = list(weighting = weightTfIdf))



# convert dtm into a df
Communities <- tidy(Communities)


# take the product of tf and idf and create new column labeled "tf_idf". Graph it.



bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:10)) %>%  
  group_by(document) %>% 
  top_n(30, wt = tf_idf) %>% 
  mutate(chapter=recode(chapter, 
                        `1`="1-Social Preferences",
                        `2`="2-Biases, Heuristics and Nudges",
                        `3`="3-Behavioral Finance",
                        `4`="4-Risk and Uncertainty",
                        `5`="5-Intertemporal Choice",
                        `6`="6-Neuroeconomics",
                        `7`="7-Behavioral GT"
  )) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 4, scales = "free") +
  coord_flip()
ggsave("Graphs/Words.png", width=286, height=215, units = "mm")

ggplot_build(plot11)$data


#Wods DT
plot11 <- setDT(plot11)
plot11[, .(chapter,term)]
plot11[, test := paste(term, collapse=", "), chapter]
plot11[, head(.SD, 1), .(chapter)]
word_list <-word_list[,.(chapter, test)][order(chapter)]
write.csv(word_list, file = "Networks/word_list.csv", row.names=FALSE)








# to do it with DT
Communities <-setDT(Communities)
setnames(Communities,"document", "modularity_class")

Communities <- bind_tf_idf(Communities, term, modularity_class, count)
setkey(Communities,modularity_class,tf_idf)
Communities <- Communities[,tail(.SD, 5), modularity_class]
Communities[modularity_class==1, modularity_class:= "1-Social Preferences"]
Communities[modularity_class==2, modularity_class:= "2-Biases, Heuristics Nudges"]
Communities[modularity_class==3, modularity_class:= "3-Behavioral Finance"]
Communities[modularity_class==4, modularity_class:= "4-Risk and Uncertainty"]
Communities[modularity_class==5, modularity_class:= "5-Intertemporal Choice"]
Communities[modularity_class==6, modularity_class:= "6-Neuroeconomics"]
Communities[modularity_class==7, modularity_class:= "7-Behavioral GT"]

ggplot(Communities, aes(term, tf_idf, fill = modularity_class)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf words",
       x = "Words", y = "tf-idf") +
  facet_wrap(~modularity_class, ncol = 6, scales = "free") +
  coord_flip()




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART III : ANALYSIS OF CITATIONS AND REFERENCES (focus on economics)  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### setting things **********************
Communities <- read.csv(file = "Networks/redo_Communities.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
Communities <- Communities[especialite_grouped=="General Biomedical Research", especialite_grouped:="General Sciences"]
Communities <- Communities[especialite_grouped=="Neurology & Neurosurgery", especialite_grouped:="Neurosciences"]

Communities <- Communities[,modularity_class:=modularity_class+1]
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
#name of communities
Communities[modularity_class==1, modularity_class:= "1-Social Preferences"]
Communities[modularity_class==2, modularity_class:= "2-Biases, Heuristics and Nudges"]
Communities[modularity_class==3, modularity_class:= "3-Behavioral Finance"]
Communities[modularity_class==4, modularity_class:= "4-Risk and Uncertainty"]
Communities[modularity_class==5, modularity_class:= "5-Intertemporal Choice"]
Communities[modularity_class==6, modularity_class:= "6-Neuroeconomics"]
Communities[modularity_class==7, modularity_class:= "7-Behavioral GT"]
Communities[annee_bibliographique<2019]

######################### Importing and Cleaning **********************
BE_ref <- fread("BD_VM/EXTENDED_REFERENCES_FULL.csv", quote="") #1 229 658
#disciplines of target
BE_ref <- merge(BE_ref, disciplines, by="Code_Discipline") #1 229 658
setnames(BE_ref,"ESpecialite", "ESpecialite_Target")
BE_ref <- merge(BE_ref, disciplines_big, by="Code_Discipline")
setnames(BE_ref,"ESpecialite_big", "ESpecialite_big_Target")
BE_ref[,EGrande_Discipline:=NULL]
BE_ref[,Code_Discipline:=NULL] #1 229 658
BE_ref[,ESpecialite_grouped_Target := ESpecialite_big_Target]; BE_ref[ESpecialite_Target %in% list_discipline$disciplines, ESpecialite_grouped_Target := ESpecialite_Target]
#get info about sources
BE_ref <- merge(BE_ref, BE_extended[,.(ID_Art, Annee_Bibliographique, ESpecialite)], by.x="ID_Art_Source", by.y="ID_Art")
BE_ref <- merge(BE_ref, Communities[,.(Id, modularity_class)], by.x="ID_Art_Source", by.y="Id")

######################### Getting what we want **********************
#only after 45
BE_ref <- BE_ref[Annee_Bibliographique>1945]
#only economics
BE_ref_eco <- BE_ref[ESpecialite=="Economics"]
#not behavioral finance
######################### Plotting **********************
BE_ref_eco <- BE_ref_eco[, n_references_years:=.N, Annee_Bibliographique]
BE_ref_eco <- BE_ref_eco[, n_specialite_years:=.N, .(Annee_Bibliographique, ESpecialite_grouped_Target)]
BE_ref_eco <- BE_ref_eco[, share_years:=n_specialite_years/n_references_years]
BE_ref_eco <- BE_ref_eco[, n_articles_citing:=.N, ID_Art_Source]

#diversity
setkey(BE_ref_eco, Annee_Bibliographique, ESpecialite_Target )
ggplot(BE_ref_eco[, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_Target)][,.N, .(Annee_Bibliographique)]
       , aes(x=Annee_Bibliographique, y=N)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2018)) +
  scale_y_continuous("Number of discipline") +
  scale_colour_manual(values = MyPalette, guide = FALSE)
ggsave("Graphs/ref_eco_diversity.png", width=286, height=215, units = "mm")
setkey(BE_ref_eco, Annee_Bibliographique, ESpecialite_Target, modularity_class )

ggplot(BE_ref_eco[, head(.SD, 1), .(Annee_Bibliographique, modularity_class, ESpecialite_Target)][,.N, .(Annee_Bibliographique, modularity_class)]
       [modularity_class!="7-Behavioral GT" & modularity_class!="6-Neuroeconomics" & modularity_class!="3-Behavioral Finance"]
       , aes(x=Annee_Bibliographique, y=N,  group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Number of Distinct Disciplines") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = modularity_class),
                   data = tail(BE_ref_eco
                               [, head(.SD, 1), .(Annee_Bibliographique, modularity_class, ESpecialite_Target)]
                               [,.N, .(Annee_Bibliographique, modularity_class)][modularity_class!="7-Behavioral GT" & modularity_class!="6-Neuroeconomics" & modularity_class!="3-Behavioral Finance"]
                               , 4),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size = 4) +

ggsave("Graphs/ref_eco_diversity_com.png", width=286, height=215, units = "mm")

#Interdisciplinarity
setkey(BE_ref_eco, Annee_Bibliographique, ESpecialite_grouped_Target) 
BE_ref_eco_roll <- BE_ref_eco[Annee_Bibliographique<2019][ESpecialite_grouped_Target!="Economics"][, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]

BE_ref_eco_roll <- complete(BE_ref_eco_roll, ESpecialite_grouped_Target, Annee_Bibliographique) %>% as.data.table
BE_ref_eco_roll <- BE_ref_eco_roll[is.na(share_years),share_years:=0]
BE_ref_eco_roll <- BE_ref_eco_roll[,share_years2:=frollmean(share_years, 3), ESpecialite_grouped_Target]
print(BE_ref_eco_roll[,.(Annee_Bibliographique, ESpecialite_grouped_Target,share_years, share_years2)], nrows = 350)
setkey(BE_ref_eco_roll, Annee_Bibliographique, ESpecialite_grouped_Target) 
ggplot(BE_ref_eco_roll
       , aes(x=Annee_Bibliographique, y=share_years2, group=ESpecialite_grouped_Target, color=ESpecialite_grouped_Target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share of Disicpline in References") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = ESpecialite_grouped_Target),
                   data = tail(BE_ref_eco_roll[Annee_Bibliographique==2018,.N, .(Annee_Bibliographique, ESpecialite_grouped_Target, share_years2)][ESpecialite_grouped_Target!="Economics"], 7),
                   nudge_x = 2,
                   na.rm = TRUE,
                   size = 4) +
  scale_colour_manual(values = MyPalette, guide = FALSE)
ggsave("Graphs/ref_eco.png", width=286, height=215, units = "mm")

setkey(BE_ref_eco, Annee_Bibliographique, ESpecialite_grouped_Target) 
BE_ref_eco_roll <- BE_ref_eco[Annee_Bibliographique<2019][ESpecialite_grouped_Target=="Economics"][, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]

BE_ref_eco_roll <- complete(BE_ref_eco_roll, ESpecialite_grouped_Target, Annee_Bibliographique) %>% as.data.table
BE_ref_eco_roll <- BE_ref_eco_roll[is.na(share_years),share_years:=0]
BE_ref_eco_roll <- BE_ref_eco_roll[,share_years2:=frollmean(share_years, 3), ESpecialite_grouped_Target]
setkey(BE_ref_eco_roll, Annee_Bibliographique, ESpecialite_grouped_Target) 
ggplot(BE_ref_eco_roll[Annee_Bibliographique<2019]
       [ESpecialite_grouped_Target=="Economics"]
       [, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]
       , aes(x=Annee_Bibliographique, y=share_years2, group=ESpecialite_grouped_Target, color=ESpecialite_grouped_Target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share of Economics in References") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = ESpecialite_grouped_Target),
                   data = tail(BE_ref_eco_roll[ESpecialite_grouped_Target=="Economics"][Annee_Bibliographique==2018,.N, .(Annee_Bibliographique, ESpecialite_grouped_Target, share_years2)], 7),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 4) +
  scale_colour_manual(values = MyPalette, guide = FALSE)
ggsave("Graphs/coc_ref_eco.png", width=286, height=215, units = "mm")

BE_ref_eco[ESpecialite_grouped_Target=="Economics" & Annee_Bibliographique>1980 ,.N,.(share_years, Annee_Bibliographique, ESpecialite_grouped_Target)]

######################### Social Preferences **********************

BE_ref_eco_1 <- BE_ref_eco[modularity_class=="1-Social Preferences"]

BE_ref_eco_1 <- BE_ref_eco_1[, n_references_years:=.N, Annee_Bibliographique]
BE_ref_eco_1 <- BE_ref_eco_1[, n_specialite_years:=.N, .(Annee_Bibliographique, ESpecialite_grouped_Target)]
BE_ref_eco_1 <- BE_ref_eco_1[, share_years:=n_specialite_years/n_references_years]
BE_ref_eco_1 <- BE_ref_eco_1[, n_articles_citing:=.N, ID_Art_Source]

BE_ref_eco_1 <- BE_ref_eco_1[Annee_Bibliographique<2019][ESpecialite_grouped_Target!="Economics"][, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]
# complete missing disciplines as 0
BE_ref_eco_1 <- complete(BE_ref_eco_1, ESpecialite_grouped_Target, Annee_Bibliographique) %>% as.data.table
BE_ref_eco_1 <- BE_ref_eco_1[is.na(share_years),share_years:=0]
# complete missing years as NA
BE_ref_eco_1 <- complete(BE_ref_eco_1, Annee_Bibliographique=1980:2018) %>% as.data.table
BE_ref_eco_1 <- complete(BE_ref_eco_1, ESpecialite_grouped_Target, Annee_Bibliographique) %>% as.data.table

BE_ref_eco_1 <- BE_ref_eco_1[,share_years2:=frollmean(share_years, 3), ESpecialite_grouped_Target]

setkey(BE_ref_eco_1, Annee_Bibliographique, ESpecialite_grouped_Target) 

ggplot(BE_ref_eco_1
       , aes(x=Annee_Bibliographique, y=share_years2, group=ESpecialite_grouped_Target, color=ESpecialite_grouped_Target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share of Disicpline in References", limits = c(0, NA)) +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = ESpecialite_grouped_Target),
                   data = tail(BE_ref_eco_1[Annee_Bibliographique==2018,.N, .(Annee_Bibliographique, ESpecialite_grouped_Target, share_years2)], 7),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 4) +
  scale_colour_manual(values = MyPalette, guide = FALSE)
ggsave("Graphs/ref_1SP.png", width=286, height=215, units = "mm")

######################### Intertemporal Choice **********************
BE_ref_eco_1 <- BE_ref_eco[modularity_class=="5-Intertemporal Choice"]
BE_ref_eco_1 <- BE_ref_eco_1[, n_references_years:=.N, Annee_Bibliographique]
BE_ref_eco_1 <- BE_ref_eco_1[, n_specialite_years:=.N, .(Annee_Bibliographique, ESpecialite_grouped_Target)]
BE_ref_eco_1 <- BE_ref_eco_1[, share_years:=n_specialite_years/n_references_years]
BE_ref_eco_1 <- BE_ref_eco_1[, n_articles_citing:=.N, ID_Art_Source]

BE_ref_eco_1 <- BE_ref_eco_1[Annee_Bibliographique<2019][ESpecialite_grouped_Target!="Economics"][, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]
# complete missing disciplines as 0
BE_ref_eco_1 <- complete(BE_ref_eco_1, ESpecialite_grouped_Target, Annee_Bibliographique) %>% as.data.table
BE_ref_eco_1 <- BE_ref_eco_1[is.na(share_years),share_years:=0]
# complete missing years as NA
BE_ref_eco_1 <- complete(BE_ref_eco_1, Annee_Bibliographique=1980:2018) %>% as.data.table
BE_ref_eco_1 <- complete(BE_ref_eco_1, ESpecialite_grouped_Target, Annee_Bibliographique) %>% as.data.table

BE_ref_eco_1 <- BE_ref_eco_1[,share_years2:=frollmean(share_years, 3), ESpecialite_grouped_Target]

setkey(BE_ref_eco_1, Annee_Bibliographique, ESpecialite_grouped_Target) 

ggplot(BE_ref_eco_1[Annee_Bibliographique<2019]
       [ESpecialite_grouped_Target!="Economics"]
       [, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]
       , aes(x=Annee_Bibliographique, y=share_years2, group=ESpecialite_grouped_Target, color=ESpecialite_grouped_Target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share of Disicpline in References", limits = c(0, NA)) +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = ESpecialite_grouped_Target),
                   data = tail(BE_ref_eco_1[Annee_Bibliographique==2018,.N, .(Annee_Bibliographique, ESpecialite_grouped_Target, share_years2)], 7),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 4) +
  scale_colour_manual(values = MyPalette, guide = FALSE)
ggsave("Graphs/ref_7IC.png", width=286, height=215, units = "mm")

######################### Risk **********************
BE_ref_eco_1 <- BE_ref_eco[modularity_class=="4-Risk and Uncertainty"]
BE_ref_eco_1 <- BE_ref_eco_1[, n_references_years:=.N, Annee_Bibliographique]
BE_ref_eco_1 <- BE_ref_eco_1[, n_specialite_years:=.N, .(Annee_Bibliographique, ESpecialite_grouped_Target)]
BE_ref_eco_1 <- BE_ref_eco_1[, share_years:=n_specialite_years/n_references_years]
BE_ref_eco_1 <- BE_ref_eco_1[, n_articles_citing:=.N, ID_Art_Source]

BE_ref_eco_1 <- BE_ref_eco_1[Annee_Bibliographique<2019][ESpecialite_grouped_Target!="Economics"][, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]
# complete missing disciplines as 0
BE_ref_eco_1 <- complete(BE_ref_eco_1, ESpecialite_grouped_Target, Annee_Bibliographique) %>% as.data.table
BE_ref_eco_1 <- BE_ref_eco_1[is.na(share_years),share_years:=0]
# complete missing years as NA
BE_ref_eco_1 <- complete(BE_ref_eco_1, Annee_Bibliographique=1980:2018) %>% as.data.table
BE_ref_eco_1 <- complete(BE_ref_eco_1, ESpecialite_grouped_Target, Annee_Bibliographique) %>% as.data.table

BE_ref_eco_1 <- BE_ref_eco_1[,share_years2:=frollmean(share_years, 3), ESpecialite_grouped_Target]

setkey(BE_ref_eco_1, Annee_Bibliographique, ESpecialite_grouped_Target) 
ggplot(BE_ref_eco_1[Annee_Bibliographique<2019]
       [ESpecialite_grouped_Target!="Economics"]
       [, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]
       , aes(x=Annee_Bibliographique, y=share_years, group=ESpecialite_grouped_Target, color=ESpecialite_grouped_Target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share of Disicpline in References") +
  scale_color_discrete(guide = FALSE, limits = c(0, NA)) +
  geom_label_repel(aes(label = ESpecialite_grouped_Target),
                   data = tail(BE_ref_eco_1[Annee_Bibliographique==2018,.N, .(Annee_Bibliographique, ESpecialite_grouped_Target, share_years)], 7),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 4) +
  scale_colour_manual(values = MyPalette, guide = FALSE)
ggsave("Graphs/ref_5RU.png", width=286, height=215, units = "mm")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Share of COC ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
BE_ref_eco_coc <- BE_ref_eco[, n_references_years:=.N, .(Annee_Bibliographique, modularity_class)]
BE_ref_eco_coc <- BE_ref_eco_coc[, n_coc_years:=.N, .(Annee_Bibliographique, modularity_class, ESpecialite_grouped_Target)]
BE_ref_eco_coc <- BE_ref_eco_coc[, share_years:=n_coc_years/n_references_years]
BE_ref_eco_coc <- BE_ref_eco_coc[, n_articles_citing:=.N, ID_Art_Source]
BE_ref_eco_coc <- BE_ref_eco_coc[, share_years:=1-share_years]
BE_ref_eco_coc[,.N, modularity_class]
setkey(BE_ref_eco_coc, Annee_Bibliographique, modularity_class, ESpecialite_grouped_Target) 
#removing all low N and looking only at economics
BE_ref_eco_coc <- BE_ref_eco_coc
BE_ref_eco_coc <- BE_ref_eco_coc[ESpecialite_grouped_Target=="Economics"]
BE_ref_eco_coc <- BE_ref_eco_coc[modularity_class=="4-Risk and Uncertainty"| modularity_class=="5-Intertemporal Choice" | modularity_class=="1-Social Preferences"]


ggplot(BE_ref_eco_coc[Annee_Bibliographique<2019]
       [, head(.SD, 1), .(Annee_Bibliographique, modularity_class, ESpecialite_grouped_Target)]
       , aes(x=Annee_Bibliographique, y=share_years, group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1990, 2020)) +
  scale_y_continuous("Share Citations Outside Discipline") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = modularity_class),
                   data = tail(BE_ref_eco_coc[Annee_Bibliographique<2019,.N, .(Annee_Bibliographique, modularity_class, ESpecialite_grouped_Target, share_years)], 3),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 4) 
  scale_colour_manual(values = MyPalette_com, guide = FALSE)
ggsave("Graphs/coc_eco.png", width=286, height=215, units = "mm")

BE_ref_eco_coc2 <- BE_ref_eco_coc[, head(.SD, 1), .(Annee_Bibliographique, modularity_class, ESpecialite_grouped_Target)]
BE_ref_eco_coc2 <- complete(BE_ref_eco_coc2, Annee_Bibliographique, modularity_class, ESpecialite_grouped_Target) %>% as.data.table
BE_ref_eco_coc2 <- BE_ref_eco_coc2[,share_years2:=frollmean(share_years, 3), .(modularity_class, ESpecialite_grouped_Target)]

ggplot(BE_ref_eco_coc2[Annee_Bibliographique<2019]
       [, head(.SD, 1), .(Annee_Bibliographique, modularity_class, ESpecialite_grouped_Target)]
       , aes(x=Annee_Bibliographique, y=share_years2, group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share Citations Outside Discipline") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = modularity_class),
                   data = tail(BE_ref_eco_coc2[Annee_Bibliographique<2019,.N, .(Annee_Bibliographique, modularity_class, ESpecialite_grouped_Target, share_years2)], 3),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 4) +
  scale_colour_manual(values = MyPalette_com, guide = FALSE)
ggsave("Graphs/coc_eco.png", width=286, height=215, units = "mm")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Psychology####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#only after 45
BE_ref <- BE_ref[Annee_Bibliographique>1945]
#only economics
BE_ref_psy <- BE_ref[ESpecialite=="Psychology"]
BE_ref_psy[Annee_Bibliographique==2000]
######################### Plotting **********************
BE_ref_psy <- BE_ref_psy[, n_references_years:=.N, Annee_Bibliographique]
BE_ref_psy <- BE_ref_psy[, n_specialite_years:=.N, .(Annee_Bibliographique, ESpecialite_grouped_Target)]
BE_ref_psy <- BE_ref_psy[, share_years:=n_specialite_years/n_references_years]
BE_ref_psy <- BE_ref_psy[, n_articles_citing:=.N, ID_Art_Source]

BE_ref_psy <- BE_ref_psy[, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]

BE_ref_psy <- complete(BE_ref_psy, ESpecialite_grouped_Target, Annee_Bibliographique) %>% as.data.table
BE_ref_psy <- BE_ref_psy[is.na(share_years),share_years:=0]
BE_ref_psy <- BE_ref_psy[,share_years2:=frollmean(share_years, 3), ESpecialite_grouped_Target]

setkey(BE_ref_psy, Annee_Bibliographique, ESpecialite_grouped_Target) 

ggplot(BE_ref_psy[Annee_Bibliographique<2019]
       [ESpecialite_grouped_Target!="Psychology"]
       [, head(.SD, 1), .(Annee_Bibliographique, ESpecialite_grouped_Target)]
       , aes(x=Annee_Bibliographique, y=share_years2, group=ESpecialite_grouped_Target, color=ESpecialite_grouped_Target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Share disicplines in references") +
scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = ESpecialite_grouped_Target),
                   data = tail(BE_ref_psy[ESpecialite_grouped_Target!="Psychology"][Annee_Bibliographique==2018,.N, .(Annee_Bibliographique, ESpecialite_grouped_Target, share_years2)], 7),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 4) +
  scale_colour_manual(values = MyPalette, guide = FALSE)

ggsave("Graphs/ref_psy.png", width=286, height=215, units = "mm")




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART V : ALL ECONOMICS  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
all_eco <- fread("BD_VM/ALL_ECO_REF.csv", quote="")
#only after 45
all_eco <- all_eco[Annee_Bibliographique_Source>1945]
#get the name of disciplines
all_eco <- merge(all_eco, disciplines, by.x="Code_Discipline_Target", by.y="Code_Discipline")
all_eco <- merge(all_eco, disciplines_big, by.x="Code_Discipline_Target", by.y="Code_Discipline")
#regroup disciplines 
all_eco[,ESpecialite_grouped := ESpecialite_big]; all_eco[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]
all_eco[,ESpecialite_grouped := ESpecialite_big]; all_eco[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]
all_eco <- all_eco[ESpecialite!="Unknown"]
all_eco[,.N,ESpecialite_grouped]
all_eco <- all_eco[ID_Art_Source %notin% BE_extended$ID_Art]

#counting
all_eco <- all_eco[,n_articles_years:=.N, Annee_Bibliographique_Source]
all_eco <- all_eco[,n_disc_years:=.N, .(Annee_Bibliographique_Source, ESpecialite_grouped)]
all_eco <- all_eco[,share_disc_years:=n_disc_years/n_articles_years]

ggplot(all_eco[Annee_Bibliographique_Source<2019]
       [ESpecialite_grouped=="Psychology" | ESpecialite_grouped=="General Sciences" | ESpecialite_grouped=="Neurosciences"]
       [, head(.SD, 1), .(Annee_Bibliographique_Source, ESpecialite_grouped)]
       , aes(x=Annee_Bibliographique_Source, y=share_disc_years, group=ESpecialite_grouped, color=ESpecialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1960, 2020)) +
  scale_y_continuous("Share of Discipline in References") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = ESpecialite_grouped),
                   data = all_eco[Annee_Bibliographique_Source==2018 & (ESpecialite_grouped=="Psychology" | ESpecialite_grouped=="General Sciences" | ESpecialite_grouped=="Neurosciences")]
                   [, head(.SD, 1), .(Annee_Bibliographique_Source, ESpecialite_grouped)],
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 4) +
  scale_colour_manual(values = MyPalette, guide = FALSE)
ggsave("Graphs/all_eco_ref.png", width=286, height=215, units = "mm")

ggplot(all_eco[Annee_Bibliographique_Source<2019]
       [ESpecialite_grouped=="Psychology" | ESpecialite_grouped=="General Sciences" | ESpecialite_grouped=="Neurosciences"]
       [Code_Revue_Source=="9662" | Code_Revue_Source=="758" | Code_Revue_Source=="4695" | Code_Revue_Source=="13992" | Code_Revue_Source=="758"]
       [, head(.SD, 1), .(Annee_Bibliographique_Source, ESpecialite_grouped)]
       , aes(x=Annee_Bibliographique_Source, y=share_disc_years, group=ESpecialite_grouped, color=ESpecialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1945, 2020)) +
  scale_y_continuous("Share disicplines in references") +
  scale_color_discrete(guide = FALSE) +
  geom_label_repel(aes(label = ESpecialite_grouped),
                   data = all_eco[Annee_Bibliographique_Source==2018 & (ESpecialite_grouped=="Psychology" | ESpecialite_grouped=="General Sciences" | ESpecialite_grouped=="Neurosciences")]
                   [, head(.SD, 1), .(Annee_Bibliographique_Source, ESpecialite_grouped)],
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 4)
ggsave("Graphs/top5_eco_ref.png", width=286, height=215, units = "mm")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART VI: Mobility  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### Corpus ######################### 
mobility <- authors_of_extended_core[,n_authors:=.N,name_short][order(n_authors)]
#authors that appear 20 times
mobility <- mobility[n_authors>20]
#and have more than 4 letters
mobility[,name_lenght:= nchar(name_short)]
mobility <- mobility[name_lenght>5]

#regroup disciplines even more
mobility[,ESpecialite_supergrouped := ESpecialite_grouped]
mobility[ESpecialite == "Management", ESpecialite_supergrouped := "Economics"]
mobility[ESpecialite == "Neurosciences", ESpecialite_supergrouped := "Psychology"]
mobility[ESpecialite == "General Sciences", ESpecialite_supergrouped := "Psychology"]

#find the first 10 articles of each authors
setkey(mobility, name_short, Annee_Bibliographique)
id_origin <-mobility[, head(.SD, 10), .(name_short)]
id_origin[,origin_aut:=Mode(ESpecialite_supergrouped),name_short]
id_origin <-id_origin[, head(.SD, 1), .(name_short)]
id_origin <-id_origin[,.(name_short,origin_aut)]

#merge to identify authors
mobility <- merge(mobility, id_origin, by="name_short")
#count when origin match pub
mobility <- mobility[,mobility_bin:=ifelse(origin_aut==ESpecialite_supergrouped, 0, 1)]
#is there mobiltiy within this article
mobility <- mobility[,mobility_bin_article:=sum(mobility_bin), ID_Art]
mobility <- mobility[mobility_bin_article>1,mobility_bin_article:=1, ID_Art]
mobility[annee_regrouped=="<1980"]

#is there mobiltiy within this author
mobility <- mobility[,sum_mobility_bin_aut:=sum(mobility_bin), name_short]
mobility <- mobility[sum_mobility_bin_aut>4,mobility_bin_aut:=1, name_short][sum_mobility_bin_aut<=4,mobility_bin_aut:=0, name_short]
mobility[annee_regrouped=="<1980"]

######################### Plot ######################### 
#order of plot
mobility$annee_regrouped <- factor(mobility$annee_regrouped,levels = c("<1980", "80-89", "90-99", "00-09", "10-19"))

#mobility overall, share of articles published in a given
setkey(mobility, annee_regrouped)
ggplot(mobility[, head(.SD, 1), .(ID_Art)]
       [,tot:=.N,annee_regrouped]
       [,sum_mob:=sum(mobility_bin_article), annee_regrouped]
       [, head(.SD, 1), .(annee_regrouped)], aes(x=annee_regrouped, y=sum_mob/tot)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="grey") + 
  geom_text(aes(label=tot), vjust=-1) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

#mobility of economists
setkey(mobility, annee_regrouped)
ggplot(mobility[, head(.SD, 1), .(ID_Art)]
       [origin_aut=="Economics"]
       [,tot:=.N,annee_regrouped]
       [,sum_mob:=sum(mobility_bin_article), annee_regrouped]
       [, head(.SD, 1), .(annee_regrouped)], aes(x=annee_regrouped, y=sum_mob/tot)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="grey") + 
  geom_text(aes(label=tot), vjust=-1) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

#mobility in economics articles
setkey(mobility, annee_regrouped)
ggplot(mobility[, head(.SD, 1), .(ID_Art)]
       [ESpecialite_supergrouped=="Economics"]
       [,tot:=.N,annee_regrouped]
       [,sum_mob:=sum(mobility_bin_article), annee_regrouped]
       [, head(.SD, 1), .(annee_regrouped)], aes(x=annee_regrouped, y=sum_mob/tot)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="grey") + 
  geom_text(aes(label=tot), vjust=-1) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))









#mobility by authors
setkey(mobility, annee_regrouped)
ggplot(mobility[, head(.SD, 1), .(name_short)]
       [,tot:=.N,annee_regrouped]
       [,sum_mob:=sum(mobility_bin_aut), annee_regrouped]
       [, head(.SD, 1), .(annee_regrouped)], aes(x=annee_regrouped, y=sum_mob/tot)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="grey") + 
  geom_text(aes(label=tot), vjust=-1) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

#mobility by authors in economics
setkey(mobility, annee_regrouped)
ggplot(mobility[, head(.SD, 1), .(name_short)]
       [origin_aut=="Economics"]
       [,tot:=.N,annee_regrouped]
       [,sum_mob:=sum(mobility_bin_aut), annee_regrouped]
       [, head(.SD, 1), .(annee_regrouped)], aes(x=annee_regrouped, y=sum_mob/tot)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="grey") + 
  geom_text(aes(label=tot), vjust=-1) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))
















Communities[modularity_class==1]




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
####   ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### setting things **********************
Communities <- read.csv(file = "Networks/Communities (10).csv") %>%  data.table

setnames(Communities,"Cluster", "modularity_class")
Communities <- Communities[especialite_grouped=="General Biomedical Research", especialite_grouped:="General Sciences"]
Communities <- Communities[especialite_grouped=="Neurology & Neurosurgery", especialite_grouped:="Neurosciences"]

Communities <- Communities[,modularity_class:=modularity_class+1]
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
#name of communities
Communities[modularity_class==1, modularity_class:= "1-Social Preferences"]
Communities[modularity_class==2, modularity_class:= "2-Behavioral Finance"]
Communities[modularity_class==3, modularity_class:= "3-Emotions"]
Communities[modularity_class==4, modularity_class:= "4-Neuroeconomics"]
Communities[modularity_class==5, modularity_class:= "5-Risk and Uncertainty"]
Communities[modularity_class==6, modularity_class:= "6-Biases and Choice"]
Communities[modularity_class==7, modularity_class:= "7-Intertemporal Choice"]
Communities[modularity_class==8, modularity_class:= "8-Heuristics"]
Communities[modularity_class==9, modularity_class:= "9-Behavioral GT"]
Communities[modularity_class==10, modularity_class:= "10-Hapiness BE"]
Communities <- Communities[,.(Id, modularity_class)]

Communities2 <- read.csv(file = "Networks/Communities (22)_TEST.csv") %>%  data.table
setnames(Communities2,"Cluster", "modularity_class2")
Communities2 <- Communities2[,modularity_class2:=modularity_class2+1]
Communities2 <- transform(Communities2, modularity_class2 = as.character(modularity_class2))
Communities2[modularity_class2 == "1", modularity_class2 := "01"]
Communities2[modularity_class2 == "2", modularity_class2 := "02"]
Communities2[modularity_class2 == "3", modularity_class2 := "03"]
Communities2[modularity_class2 == "4", modularity_class2 := "04"]
Communities2[modularity_class2 == "5", modularity_class2 := "05"]
Communities2[modularity_class2 == "6", modularity_class2 := "06"]
Communities2[modularity_class2 == "7", modularity_class2 := "07"]
Communities2[modularity_class2 == "8", modularity_class2 := "08"]
Communities2[modularity_class2 == "9", modularity_class2 := "09"]
Communities2 <- Communities2[,.(Id, modularity_class2)]

super_Communities <- merge(Communities, Communities2, by = "Id")
super_Communities <- super_Communities[,.(modularity_class, modularity_class2)]
# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)

# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:

mygraph <- graph_from_data_frame(super_Communities)

# Control the size of each circle: (use the size column of the vertices data frame)
ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle() +
  theme_void()






######################### tf **********************
Communities <- read.csv(file = "Networks/Communities (22)_TEST.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ID_Art"
#name of communities
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
Communities[modularity_class == "1", modularity_class := "01"]
Communities[modularity_class == "2", modularity_class := "02"]
Communities[modularity_class == "3", modularity_class := "03"]
Communities[modularity_class == "4", modularity_class := "04"]
Communities[modularity_class == "5", modularity_class := "05"]
Communities[modularity_class == "6", modularity_class := "06"]
Communities[modularity_class == "7", modularity_class := "07"]
Communities[modularity_class == "8", modularity_class := "08"]
Communities[modularity_class == "9", modularity_class := "09"]
require(tm)
require(bibliometrix)
require(RColorBrewer)
library(janeaustenr)
library(tidytext)
require(plyr)
require(dplyr)

Communities <- Communities %>% group_by(modularity_class) %>% summarise(titre = paste(titre, collapse=", "))
Communities <- VCorpus(VectorSource(Communities$titre))

Communities <- tm_map(Communities, stripWhitespace)
Communities <- tm_map(Communities, removePunctuation)
Communities <- tm_map(Communities, content_transformer(tolower))
Communities <- tm_map(Communities, removeWords, stopwords("english"))
Communities <- tm_map(Communities, stemDocument)

Communities <- DocumentTermMatrix(Communities)

#Communities <- DocumentTermMatrix(Communities, control = list(weighting = weightTfIdf))



# convert dtm into a df
Communities <- tidy(Communities)


# take the product of tf and idf and create new column labeled "tf_idf". Graph it.
bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:22)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 5, scales = "free") +
  coord_flip()




















#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### COCIT ANALYSIS 1970 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#1970 community up
Communities[Id %in% BE_extended[ItemID_Ref==209455 | ItemID_Ref==147389 | ItemID_Ref==72915 | ItemID_Ref==570620, ID_Art]]
#1970 community down
Communities[Id %in% BE_extended[ItemID_Ref==1257893 | ItemID_Ref==558794 | ItemID_Ref==803377 | ItemID_Ref==4538586, ID_Art]]
Communities <- merge(Communities, all_art, by = "ItemID_Ref")
Communities <- merge(Communities, revues, by = "Code_Revue", all = FALSE)
Communities[, .N, .(modularity_class, Revue)][order(modularity_class, N)]
######################### tf **********************
Communities <- read.csv(file = "Networks/com_70.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ItemID_Ref"
Communities <- merge(Communities, all_art[,.(ItemID_Ref, Titre)], by = "ItemID_Ref")
colnames(Communities)[colnames(Communities)=="Titre"] <- "titre"
Communities <- Communities[modularity_class<9]
#name of communities
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
require(tm)
require(bibliometrix)
require(RColorBrewer)
library(janeaustenr)
library(tidytext)
require(plyr)
require(dplyr)

Communities <- Communities %>% group_by(modularity_class) %>% summarise(titre = paste(titre, collapse=", "))
Communities <- VCorpus(VectorSource(Communities$titre))

Communities <- tm_map(Communities, stripWhitespace)
Communities <- tm_map(Communities, removePunctuation)
Communities <- tm_map(Communities, content_transformer(tolower))
Communities <- tm_map(Communities, removeWords, stopwords("english"))
Communities <- tm_map(Communities, stemDocument)

Communities <- DocumentTermMatrix(Communities)

#Communities <- DocumentTermMatrix(Communities, control = list(weighting = weightTfIdf))



# convert dtm into a df
Communities <- tidy(Communities)


# take the product of tf and idf and create new column labeled "tf_idf". Graph it.



bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:9)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 5, scales = "free") +
  coord_flip()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### COCIT ANALYSIS 1980 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Communities <- read.csv(file = "Networks/com_80.csv") %>%  data.table
Communities[Cluster==0,.N/Communities[Cluster==0,.N],especialite_grouped]
Communities[Cluster==1,.N/Communities[Cluster==1,.N],especialite_grouped]
Communities[Cluster==2,.N/Communities[Cluster==2,.N],especialite_grouped]
Communities <- merge(Communities, all_art, by = "ItemID_Ref")
Communities <- merge(Communities, revues, by = "Code_Revue", all = FALSE)
Communities[, .N, .(modularity_class, Revue)][order(modularity_class, N)][, tail(.SD, 3), .(modularity_class)]
#all art
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
######################### tf **********************
Communities <- read.csv(file = "Networks/com_80.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ItemID_Ref"
Communities <- merge(Communities, all_art[,.(ItemID_Ref, Titre)], by = "ItemID_Ref")
colnames(Communities)[colnames(Communities)=="Titre"] <- "titre"

#name of communities
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
require(tm)
require(bibliometrix)
require(RColorBrewer)
library(janeaustenr)
library(tidytext)
require(plyr)
require(dplyr)

Communities <- Communities %>% group_by(modularity_class) %>% summarise(titre = paste(titre, collapse=", "))
Communities <- VCorpus(VectorSource(Communities$titre))

Communities <- tm_map(Communities, stripWhitespace)
Communities <- tm_map(Communities, removePunctuation)
Communities <- tm_map(Communities, content_transformer(tolower))
Communities <- tm_map(Communities, removeWords, stopwords("english"))
Communities <- tm_map(Communities, stemDocument)

Communities <- DocumentTermMatrix(Communities)

#Communities <- DocumentTermMatrix(Communities, control = list(weighting = weightTfIdf))



# convert dtm into a df
Communities <- tidy(Communities)


# take the product of tf and idf and create new column labeled "tf_idf". Graph it.



bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:10)) %>%  
  group_by(document) %>% 
  top_n(15, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 5, scales = "free") +
  coord_flip()

#Wods DT
com_80 <- bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:9)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% as.data.table()

com_80[, print(list(term)), by= chapter]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### COCIT ANALYSIS 1990 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Communities <- read.csv(file = "Networks/com_90.csv") %>%  data.table
Communities[Cluster==0,.N/Communities[Cluster==0,.N],especialite_grouped]
Communities[Cluster==1,.N/Communities[Cluster==1,.N],especialite_grouped]
Communities[Cluster==2,.N/Communities[Cluster==2,.N],especialite_grouped]
Communities[Cluster==4,.N/Communities[Cluster==4,.N],especialite_grouped]

#all art
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
######################### tf **********************
Communities <- read.csv(file = "Networks/com_90.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ItemID_Ref"
Communities <- merge(Communities, all_art[,.(ItemID_Ref, Titre)], by = "ItemID_Ref")
colnames(Communities)[colnames(Communities)=="Titre"] <- "titre"
Communities <- Communities[modularity_class<11]
Communities <- Communities[,modularity_class:=sprintf("%02d", Communities$modularity_class)]

#name of communities
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
require(tm)
require(bibliometrix)
require(RColorBrewer)
library(janeaustenr)
library(tidytext)
require(plyr)
require(dplyr)

Communities <- Communities %>% group_by(modularity_class) %>% summarise(titre = paste(titre, collapse=", "))
Communities <- VCorpus(VectorSource(Communities$titre))

Communities <- tm_map(Communities, stripWhitespace)
Communities <- tm_map(Communities, removePunctuation)
Communities <- tm_map(Communities, content_transformer(tolower))
Communities <- tm_map(Communities, removeWords, stopwords("english"))
Communities <- tm_map(Communities, stemDocument)

Communities <- DocumentTermMatrix(Communities)

#Communities <- DocumentTermMatrix(Communities, control = list(weighting = weightTfIdf))



# convert dtm into a df
Communities <- tidy(Communities)


# take the product of tf and idf and create new column labeled "tf_idf". Graph it.



bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:11)) %>%  
  group_by(document) %>% 
  top_n(20, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 5, scales = "free") +
  coord_flip()

#Wods DT
com_80 <- bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:9)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% as.data.table()

com_80[, print(list(term)), by= chapter]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### COCIT ANALYSIS 2000 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Communities <- read.csv(file = "Networks/com_00.csv") %>%  data.table
Communities[Cluster==0,.N/Communities[Cluster==0,.N],especialite_grouped]
Communities[Cluster==1,.N/Communities[Cluster==1,.N],especialite_grouped]
Communities[Cluster==2,.N/Communities[Cluster==2,.N],especialite_grouped]
Communities[Cluster==3,.N/Communities[Cluster==3,.N],especialite_grouped]
Communities[Cluster==4,.N/Communities[Cluster==4,.N],especialite_grouped]
Communities[Cluster==6,.N/Communities[Cluster==6,.N],especialite_grouped]

#all art
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
######################### tf **********************
Communities <- read.csv(file = "Networks/com_00.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ItemID_Ref"
Communities <- merge(Communities, all_art[,.(ItemID_Ref, Titre)], by = "ItemID_Ref")
colnames(Communities)[colnames(Communities)=="Titre"] <- "titre"
Communities <- Communities[modularity_class<7]
Communities <- Communities[,modularity_class:=sprintf("%02d", Communities$modularity_class)]

#name of communities
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
require(tm)
require(bibliometrix)
require(RColorBrewer)
library(janeaustenr)
library(tidytext)
require(plyr)
require(dplyr)

Communities <- Communities %>% group_by(modularity_class) %>% summarise(titre = paste(titre, collapse=", "))
Communities <- VCorpus(VectorSource(Communities$titre))

Communities <- tm_map(Communities, stripWhitespace)
Communities <- tm_map(Communities, removePunctuation)
Communities <- tm_map(Communities, content_transformer(tolower))
Communities <- tm_map(Communities, removeWords, stopwords("english"))
Communities <- tm_map(Communities, stemDocument)

Communities <- DocumentTermMatrix(Communities)

#Communities <- DocumentTermMatrix(Communities, control = list(weighting = weightTfIdf))



# convert dtm into a df
Communities <- tidy(Communities)


# take the product of tf and idf and create new column labeled "tf_idf". Graph it.



bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:11)) %>%  
  group_by(document) %>% 
  top_n(20, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 5, scales = "free") +
  coord_flip()

#Wods DT
com_80 <- bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:9)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% as.data.table()

com_80[, print(list(term)), by= chapter]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### COCIT ANALYSIS 2010 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Communities <- read.csv(file = "Networks/com_10.csv") %>%  data.table
Communities[Cluster==0,.N/Communities[Cluster==0,.N],especialite_grouped]
Communities[Cluster==1,.N/Communities[Cluster==1,.N],especialite_grouped]
Communities[Cluster==2,.N/Communities[Cluster==2,.N],especialite_grouped]
Communities[Cluster==3,.N/Communities[Cluster==3,.N],especialite_grouped]

Communities[Cluster==7,.N/Communities[Cluster==7,.N],especialite_grouped]
Communities[Cluster==6,.N/Communities[Cluster==6,.N],especialite_grouped]
Communities[Cluster==8,.N/Communities[Cluster==8,.N],especialite_grouped]

#all art
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
######################### tf **********************
Communities <- read.csv(file = "Networks/com_10.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ItemID_Ref"
Communities <- merge(Communities, all_art[,.(ItemID_Ref, Titre)], by = "ItemID_Ref")
colnames(Communities)[colnames(Communities)=="Titre"] <- "titre"
Communities <- Communities[modularity_class<11]
Communities <- Communities[,modularity_class:=sprintf("%02d", Communities$modularity_class)]

#name of communities
Communities <- transform(Communities, modularity_class = as.character(modularity_class))
require(tm)
require(bibliometrix)
require(RColorBrewer)
library(janeaustenr)
library(tidytext)
require(plyr)
require(dplyr)

Communities <- Communities %>% group_by(modularity_class) %>% summarise(titre = paste(titre, collapse=", "))
Communities <- VCorpus(VectorSource(Communities$titre))

Communities <- tm_map(Communities, stripWhitespace)
Communities <- tm_map(Communities, removePunctuation)
Communities <- tm_map(Communities, content_transformer(tolower))
Communities <- tm_map(Communities, removeWords, stopwords("english"))
Communities <- tm_map(Communities, stemDocument)

Communities <- DocumentTermMatrix(Communities)

#Communities <- DocumentTermMatrix(Communities, control = list(weighting = weightTfIdf))



# convert dtm into a df
Communities <- tidy(Communities)


# take the product of tf and idf and create new column labeled "tf_idf". Graph it.



bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:12)) %>%  
  group_by(document) %>% 
  top_n(20, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 5, scales = "free") +
  coord_flip()

#Wods DT
com_80 <- bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:9)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% as.data.table()

com_80[, print(list(term)), by= chapter]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Inwardness 80 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Communities <- read.csv(file = "Networks/com_80.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ItemID_Ref"
Communities <- Communities[,modularity_class:=sprintf("%02d", Communities$modularity_class)]


Inwardness <- merge(edges_BE_extented_cocit_80, Communities[,.(ItemID_Ref, modularity_class)], by.x = "Source", by.y = "ItemID_Ref")
colnames(Inwardness)[colnames(Inwardness)=="modularity_class"] <- "modularity_class_source"
Inwardness <- merge(Inwardness, Communities[,.(ItemID_Ref, modularity_class)], by.x = "Target", by.y = "ItemID_Ref")
colnames(Inwardness)[colnames(Inwardness)=="modularity_class"] <- "modularity_class_target"

Inwardness[modularity_class_source > modularity_class_target]

#exchange modularity class if different number, therefore only modularity_clas scolumns are valid now
Inwardness[modularity_class_source > modularity_class_target, c("modularity_class_target", "modularity_class_source")] <- Inwardness[modularity_class_source > modularity_class_target, c("modularity_class_source", "modularity_class_target")] 

#inward links
inward <- Inwardness[modularity_class_source==modularity_class_target,.(inward=sum(Weight)), modularity_class_source][order(modularity_class_source)]
#outward links
outward1 <- Inwardness[modularity_class_source!=modularity_class_target,sum(Weight), .(modularity_class_source, modularity_class_target)][order(modularity_class_source)][,sum(V1),modularity_class_source]
outward2 <- Inwardness[modularity_class_source!=modularity_class_target,sum(Weight), .(modularity_class_source, modularity_class_target)][order(modularity_class_source)][,sum(V1),modularity_class_target]
outward <- merge(outward1, outward2, by.x = "modularity_class_source" , by.y = "modularity_class_target" , all.x = TRUE, all.y = TRUE)
outward[is.na(outward)] <- 0
outward <- outward[,outward:= V1.x+V1.y][,V1.x:=NULL][,V1.y:=NULL]
#final
Inwardness <- merge(inward, outward, by.x = "modularity_class_source" , by.y = "modularity_class_source" , all.x = TRUE, all.y = TRUE)
Inwardness <- Inwardness[,inwardness:=inward/(inward+outward)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Inwardness 90 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Communities <- read.csv(file = "Networks/com_90.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ItemID_Ref"
Communities <- Communities[,modularity_class:=sprintf("%02d", Communities$modularity_class)]


Inwardness <- merge(edges_BE_extented_cocit_90, Communities[,.(ItemID_Ref, modularity_class)], by.x = "Source", by.y = "ItemID_Ref")
colnames(Inwardness)[colnames(Inwardness)=="modularity_class"] <- "modularity_class_source"
Inwardness <- merge(Inwardness, Communities[,.(ItemID_Ref, modularity_class)], by.x = "Target", by.y = "ItemID_Ref")
colnames(Inwardness)[colnames(Inwardness)=="modularity_class"] <- "modularity_class_target"

Inwardness[modularity_class_source > modularity_class_target]

#exchange modularity class if different number, therefore only modularity_clas scolumns are valid now
Inwardness[modularity_class_source > modularity_class_target, c("modularity_class_target", "modularity_class_source")] <- Inwardness[modularity_class_source > modularity_class_target, c("modularity_class_source", "modularity_class_target")] 

#inward links
inward <- Inwardness[modularity_class_source==modularity_class_target,.(inward=sum(Weight)), modularity_class_source][order(modularity_class_source)]
#outward links
outward1 <- Inwardness[modularity_class_source!=modularity_class_target,sum(Weight), .(modularity_class_source, modularity_class_target)][order(modularity_class_source)][,sum(V1),modularity_class_source]
outward2 <- Inwardness[modularity_class_source!=modularity_class_target,sum(Weight), .(modularity_class_source, modularity_class_target)][order(modularity_class_source)][,sum(V1),modularity_class_target]
outward <- merge(outward1, outward2, by.x = "modularity_class_source" , by.y = "modularity_class_target" , all.x = TRUE, all.y = TRUE)
outward[is.na(outward)] <- 0
outward <- outward[,outward:= V1.x+V1.y][,V1.x:=NULL][,V1.y:=NULL]
#final
Inwardness <- merge(inward, outward, by.x = "modularity_class_source" , by.y = "modularity_class_source" , all.x = TRUE, all.y = TRUE)
  Inwardness <- Inwardness[,inwardness:=inward/(inward+outward)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Inwardness 00 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Communities <- read.csv(file = "Networks/com_00.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ItemID_Ref"
Communities <- Communities[,modularity_class:=sprintf("%02d", Communities$modularity_class)]


Inwardness <- merge(edges_BE_extented_cocit_00, Communities[,.(ItemID_Ref, modularity_class)], by.x = "Source", by.y = "ItemID_Ref")
colnames(Inwardness)[colnames(Inwardness)=="modularity_class"] <- "modularity_class_source"
Inwardness <- merge(Inwardness, Communities[,.(ItemID_Ref, modularity_class)], by.x = "Target", by.y = "ItemID_Ref")
colnames(Inwardness)[colnames(Inwardness)=="modularity_class"] <- "modularity_class_target"

Inwardness[modularity_class_source > modularity_class_target]

#exchange modularity class if different number, therefore only modularity_clas scolumns are valid now
Inwardness[modularity_class_source > modularity_class_target, c("modularity_class_target", "modularity_class_source")] <- Inwardness[modularity_class_source > modularity_class_target, c("modularity_class_source", "modularity_class_target")] 

#inward links
inward <- Inwardness[modularity_class_source==modularity_class_target,.(inward=sum(Weight)), modularity_class_source][order(modularity_class_source)]
#outward links
outward1 <- Inwardness[modularity_class_source!=modularity_class_target,sum(Weight), .(modularity_class_source, modularity_class_target)][order(modularity_class_source)][,sum(V1),modularity_class_source]
outward2 <- Inwardness[modularity_class_source!=modularity_class_target,sum(Weight), .(modularity_class_source, modularity_class_target)][order(modularity_class_source)][,sum(V1),modularity_class_target]
outward <- merge(outward1, outward2, by.x = "modularity_class_source" , by.y = "modularity_class_target" , all.x = TRUE, all.y = TRUE)
outward[is.na(outward)] <- 0
outward <- outward[,outward:= V1.x+V1.y][,V1.x:=NULL][,V1.y:=NULL]
#final
Inwardness <- merge(inward, outward, by.x = "modularity_class_source" , by.y = "modularity_class_source" , all.x = TRUE, all.y = TRUE)
Inwardness <- Inwardness[,inwardness:=inward/(inward+outward)]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Inwardness 00 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Communities <- read.csv(file = "Networks/com_10.csv") %>%  data.table
setnames(Communities,"Cluster", "modularity_class")
colnames(Communities)[colnames(Communities)=="Id"] <- "ItemID_Ref"
Communities <- Communities[,modularity_class:=sprintf("%02d", Communities$modularity_class)]


Inwardness <- merge(edges_BE_extented_cocit_10, Communities[,.(ItemID_Ref, modularity_class)], by.x = "Source", by.y = "ItemID_Ref")
colnames(Inwardness)[colnames(Inwardness)=="modularity_class"] <- "modularity_class_source"
Inwardness <- merge(Inwardness, Communities[,.(ItemID_Ref, modularity_class)], by.x = "Target", by.y = "ItemID_Ref")
colnames(Inwardness)[colnames(Inwardness)=="modularity_class"] <- "modularity_class_target"

Inwardness[modularity_class_source > modularity_class_target]

#exchange modularity class if different number, therefore only modularity_clas scolumns are valid now
Inwardness[modularity_class_source > modularity_class_target, c("modularity_class_target", "modularity_class_source")] <- Inwardness[modularity_class_source > modularity_class_target, c("modularity_class_source", "modularity_class_target")] 

#inward links
inward <- Inwardness[modularity_class_source==modularity_class_target,.(inward=sum(Weight)), modularity_class_source][order(modularity_class_source)]
#outward links
outward1 <- Inwardness[modularity_class_source!=modularity_class_target,sum(Weight), .(modularity_class_source, modularity_class_target)][order(modularity_class_source)][,sum(V1),modularity_class_source]
outward2 <- Inwardness[modularity_class_source!=modularity_class_target,sum(Weight), .(modularity_class_source, modularity_class_target)][order(modularity_class_source)][,sum(V1),modularity_class_target]
outward <- merge(outward1, outward2, by.x = "modularity_class_source" , by.y = "modularity_class_target" , all.x = TRUE, all.y = TRUE)
outward[is.na(outward)] <- 0
outward <- outward[,outward:= V1.x+V1.y][,V1.x:=NULL][,V1.y:=NULL]
#final
Inwardness <- merge(inward, outward, by.x = "modularity_class_source" , by.y = "modularity_class_source" , all.x = TRUE, all.y = TRUE)
Inwardness <- Inwardness[,inwardness:=inward/(inward+outward)]


