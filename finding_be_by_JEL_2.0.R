# Load the package required to read XML files.
library("XML")

# Also load the other required package.
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
######################### Directory #########################
setwd("/projects/digital_history/behavioral economics/behavioral economics by JEL")

######################### Make some choices #########################
list_discipline <- data.table(disciplines = c("Economics", "Management", "Behavioral Science & Complementary Psychology", "Law"))

######################### Functions #########################
`%notin%` <- Negate(`%in%`)

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

######################### From xml to DT #########################
# Give the input file name to the function.
#econlit <- read_xml("BE_JEL6K005.xml") 6,630 without XP and with neuro
econlit <- read_xml("BE NEURO wihout XP.xml")

#This line create a kind of list of all entries
econlit <- xml_find_all(econlit, "//rec")
#Extracting data
title <- xml_text(xml_find_first(econlit, ".//atl"))
year <- data.table(xml_text(xml_find_first(econlit, ".//dt")))
year[,V1 := substr(V1,1,4)]
journal <- xml_text(xml_find_first(econlit, ".//jtl"))
vol <- xml_text(xml_find_first(econlit, ".//vid"))
no <- xml_text(xml_find_first(econlit, ".//iid"))
pubType <- xml_text(xml_find_first(econlit, ".//pubtype"))
pages <- xml_text(xml_find_first(econlit, ".//pages"))
#Compiling 5831 before finance now 6005
dt_JEL_Articles <- data.table(Title = title, Year = year, Journal = journal, Vol = vol, No = no, Pages = pages, PubType = pubType)

#Keep only articles
dt_JEL_Articles <- dt_JEL_Articles[PubType=="Journal Article"]
# From 6,630 on econlit, to 4204 articles on econlit, 1054 artilces found within the BD

######################### Getting the BD #########################
#all ref
all_ref <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.References7.ItemID_Ref, OST_Expanded_SciHum.References7.Annee, OST_Expanded_SciHum.References7.ID_Art, OST_Expanded_SciHum.References7.Volume FROM OST_Expanded_SciHum.References7 WHERE ItemID_Ref != 0;")) %>%  data.table
#all art
all_art <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.Articles WHERE ItemID_Ref != 0;")) %>%  data.table
#Disciplines
revues  <-  dbGetQuery(ESH, "SELECT Code_Revue, Revue, Code_Discipline FROM OST_Expanded_SciHum.Revues;") %>% data.table
#Regrouping psychology
revues  <- revues %>% mutate(Code_Discipline=replace(Code_Discipline, Code_Discipline>=101 & Code_Discipline<=109, 101)) %>% data.table
#DF of disciplines
disciplines  <-  dbGetQuery(ESH, "SELECT ESpecialite, Code_Discipline FROM OST_Expanded_SciHum.Disciplines;") %>% data.table
disciplines[ESpecialite=="Behavioral Science & Complementary Psychology", ESpecialite:="Psychology"]

setkey(all_art, ItemID_Ref)
setkey(all_ref, ItemID_Ref)
#auteurs
all_aut <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.Articles.ID_Art, OST_Expanded_SciHum.Articles.Titre, OST_Expanded_SciHum.Articles.Annee_Bibliographique, OST_Expanded_SciHum.Articles.Code_Revue, OST_Expanded_SciHum.Articles.ItemID_Ref, OST_Expanded_SciHum.Auteurs.Nom
                                   FROM OST_Expanded_SciHum.Articles
                                   JOIN OST_Expanded_SciHum.Auteurs ON OST_Expanded_SciHum.Articles.ID_Art=OST_Expanded_SciHum.Auteurs.ID_Art")) %>%  data.table

issueID <- read.csv("/projects/digital_history/behavioral economics/data/revueID.csv",sep=";") %>% data.table()
all_art <- merge(all_art, issueID[,.(IssueID,Volume)], by= "IssueID")
             

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Finding the JEL articles in the BD####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Getting all_art running smoothly #########################
all_art2 <- all_art[,.(ID_Art, Annee_Bibliographique, Title = Titre, Code_Revue, Page_Debut, Page_Fin, Nb_Page, ItemID_Ref, Volume)]
all_art2 <- merge(all_art2, revues, by="Code_Revue")
all_art2[,Revue := sub("\r","", Revue)]

#formatting pages number
all_art2 <- all_art2[,Pages := paste0(all_art2$Page_Debut,"-",all_art2$Page_Fin)]

#getting the volume of publication from the ItemID_Ref table
#all_art <- merge(all_art, all_ref, by = "ItemID_Ref", all.x = TRUE, all.y = FALSE)
#keeping only unique observations
#all_art <-unique(all_art)
#collapse columns 
all_art2 <- all_art2[, ID_by_pub := paste(all_art2$Annee_Bibliographique, all_art2$Revue, all_art2$Pages, all_art2$Volume)]

######################### Do the same for dt of JEL #########################
dt_JEL_Articles[,Journal := toupper(Journal)]
dt_JEL_Articles <- transform(dt_JEL_Articles, Year.V1 = as.numeric(Year.V1))
#collapse columns
dt_JEL_Articles <- dt_JEL_Articles[, ID_by_pub := paste(dt_JEL_Articles$Year.V1, dt_JEL_Articles$Journal, dt_JEL_Articles$Pages, dt_JEL_Articles$Vol)]

######################### Merging everything #########################
#JEL with bd to find article by their volume, pages, year, and journal
merging <- merge(dt_JEL_Articles, all_art2, by="ID_by_pub", all = FALSE)

#JEL with bd to find article with same title
merging_title <- merge(dt_JEL_Articles[,.(Title)], all_art2[,.(Title, ID_Art)], by.x = "Title", by.y = "Title", all = FALSE)

#merging the two mergesby ID_Art
merging_merge <- merge(merging, merging_title, by.x = "ID_Art", by.y = "ID_Art", all = TRUE)

#checking for double, 686 articles with no double
merging_merge[,.(.N), by = "ID_Art"][order(N)] 
merging_merge <- unique(merging_merge)

#ready to move to find the core with BE; 686 articles well identified!
BE <- merge(merging_merge[,.(ID_Art)], all_art, by="ID_Art", all.x = TRUE)

######################### Cleaning everything and only keeping the ID_Art #########################
rm(all_art2)
gc()

write.csv(BE[,.(ID_Art)], file = "BD_VM/BE_JEL.csv", row.names=TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Now we got our list of articles JEL coded; find the core####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Find the bibliography of our JEL articles #########################
#Find references
BE_ref <- merge(BE[,.(ID_Art, ItemID_Ref_source = ItemID_Ref, Titre, Annee_Bibliographique, Code_Revue)], all_ref[,.(ID_Art, ItemID_Ref_target = ItemID_Ref)], by="ID_Art")

#Find articles that are at least referenced N times
BE_ref <- BE_ref[,.(.N), by = "ItemID_Ref_target"][order(N)][N>5]
setkey(BE_ref, ItemID_Ref_target)
setkey(all_art, ItemID_Ref)

######################### Get the information of all our new articles and calling it "core" #########################
BE_ref <- merge(BE_ref[,.(ItemID_Ref=ItemID_Ref_target, citations_by_JEL=N)], all_art[,.(ID_Art, ItemID_Ref, Titre, Annee_Bibliographique, Code_Revue)], by.x = "ItemID_Ref", by.y = "ItemID_Ref", all = FALSE)
BE_ref <- merge(BE_ref, revues, by = "Code_Revue", all = FALSE)
BE_core <- merge(BE_ref, disciplines[,.(Code_Discipline, ESpecialite)], by = "Code_Discipline", all = FALSE)

######################### LOOK AT THE CORE ! 403 articles #########################
BE_core[,.(.N), by = "ESpecialite"][order(N)] # the disciplines 77 in psychology ! and 224 in economics !
BE_core[Annee_Bibliographique==1999] # FS 1999 is here
BE_core[Annee_Bibliographique==1979] # KT 1979 too !

#grouping disciplines
BE_core[,ESpecialite_grouped := "Other"]; BE_core[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]
BE_core[,.(.N), by = "ESpecialite_grouped"][order(N)]
#counting articles by years
BE_core[,articles_by_year:=.N, Annee_Bibliographique]
BE_core[,disciplines_by_year:=.N, .(Annee_Bibliographique, ESpecialite_grouped)]
BE_core[,share_of_discipline:=disciplines_by_year/articles_by_year]



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Getting more from our core ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### citations and references to/of core #########################
citations_to_core <- all_ref[ItemID_Ref %in% BE_core$ItemID_Ref]
references_of_core <- all_ref[ID_Art %in% BE_core$ID_Art]

######################### info about citations #########################
BE_core_extended_citations <- citations_to_core[,.(number_of_articles_cited_in_core = .N), ID_Art][order(number_of_articles_cited_in_core)][number_of_articles_cited_in_core>5]

######################### info about references #########################
BE_core_extended_references <- references_of_core[,.(number_of_citations_by_core = .N), ItemID_Ref][order(number_of_citations_by_core)][number_of_citations_by_core>5]
#replacing ItemID_Ref by ID_Art
BE_core_extended_references <- merge(BE_core_extended_references, all_art[,.(ID_Art, ItemID_Ref)], by = "ItemID_Ref")
BE_core_extended_references <- BE_core_extended_references[,.(ID_Art, number_of_citations_by_core)]

######################### binding #########################
BE_extended <- rbind(BE_core_extended_citations[,.(ID_Art)], BE_core_extended_references[,.(ID_Art)], BE_core[,.(ID_Art)], fill=TRUE)
#checking for double
BE_extended[,.(.N), by = "ID_Art"][order(N)] 
BE_extended <- unique(BE_extended)

#Identifying the core
BE_extended <- BE_extended[,core := 0]; BE_extended[ID_Art %in% BE_core$ID_Art,core := 1]
BE_extended[,.(.N), by = "core"][order(N)] 

#getting info about our ID_Art
BE_extended <- merge(BE_extended, all_art[,.(ID_Art, ItemID_Ref, Titre, Annee_Bibliographique, Code_Revue)], by = "ID_Art")
BE_extended <- merge(BE_extended, revues, by = "Code_Revue", all = FALSE)
BE_extended <- merge(BE_extended, disciplines[,.(Code_Discipline, ESpecialite)], by = "Code_Discipline", all = FALSE)

#grouping disciplines
BE_extended[,ESpecialite_grouped := "Other"]; BE_extended[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]

#counting articles by years
BE_extended[,articles_by_year:=.N, Annee_Bibliographique]
BE_extended[,disciplines_by_year:=.N, .(Annee_Bibliographique, ESpecialite_grouped)]
BE_extended[,share_of_discipline:=disciplines_by_year/articles_by_year]
BE_extended[ID_Art==21640410]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART II : Plotting our corpus and extended corpus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ggplot CORE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### set 1 (corpus info) #########################
setkey(BE_core, Annee_Bibliographique)
#seeing distribution by years of our core
corpus_core1 <- ggplot(BE_core, aes(x=Annee_Bibliographique)) + 
  geom_histogram(color="black", fill="white")

#seeing distribution by disciplines of our core
corpus_core2 <- ggplot(BE_core, aes(x=Annee_Bibliographique, color=ESpecialite, fill=ESpecialite)) +
  geom_histogram()

#seeing distribution of citations by JEL codes of our core
corpus_core3 <- ggplot(BE_core, aes(x=Annee_Bibliographique, y=citations_by_JEL)) +
  geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#journals
corpus_core4 <- ggplot(BE_core[,.(.N), Revue][order(N)][,tail(.SD,10)], aes(x=Revue, y=N)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="tomato2") + 
  theme(axis.text.x = element_text(angle=60, vjust=0.6)) +
  scale_x_discrete(label=abbreviate)

#interdisciplinarity
corpus_core42 <- ggplot(BE_core[ESpecialite_grouped!="Economics"]
                                , aes(x=Annee_Bibliographique, y=share_of_discipline, group=ESpecialite_grouped, color=ESpecialite_grouped)) +
  geom_smooth(method="auto")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2016)) +
  scale_y_continuous("Share disicpline") 

######################### set 2 (auteurs info) #########################
authors_of_core <- merge(BE_core, all_aut[,.(ID_Art, Nom)], by = "ID_Art", all.x = TRUE, all.y = FALSE)
#Remove second name
authors_of_core <- authors_of_core[, name_short:= paste0(unlist(strsplit(Nom,"-"))[1],"-", substr(unlist(strsplit(Nom,"-"))[2],1,1) 
), by=list(ID_Art,Nom)]
#Upper all
authors_of_core$name_short <- toupper(authors_of_core$name_short)

corpus_core5 <- ggplot(authors_of_core[,.(.N), name_short][order(N)][,tail(.SD,10)], aes(x=name_short, y=N)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="tomato2") + 
  theme(axis.text.x = element_text(angle=60, vjust=0.6))

corpus_core6 <- ggplot(authors_of_core[,.(sum=sum(citations_by_JEL)), name_short][order(sum)][,tail(.SD,10)], aes(x=name_short, y=sum)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="tomato2") + 
  theme(axis.text.x = element_text(angle=60, vjust=0.6))

plot1 <- (corpus_core1 + corpus_core2) / corpus_core3
plot2 <- (corpus_core6 + corpus_core5) / corpus_core4
saveRDS(plot1, file = "Plots/plot1.RDS")
saveRDS(plot2, file = "Plots/plot2.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ggplot Extended_CORE####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### set 1 (corpus info) #########################
setkey(BE_extended, Annee_Bibliographique)
#seeing distribution by years of our core extended
corpus_extended_core1 <- ggplot(BE_extended, aes(x=Annee_Bibliographique)) + 
  geom_histogram(color="black", fill="white")

######################### set 2 (discipline info) #########################
#seeing distribution of disciplines of our core extended
BE_extended[,.(.N), .(ESpecialite)][order(N)]
BE_extended[,.(.N), .(Annee_Bibliographique, ESpecialite)]

#seeing distribution by disciplines of our core extended
corpus_extended_core2 <- ggplot(BE_extended, aes(x=Annee_Bibliographique, color=ESpecialite_grouped, fill=ESpecialite_grouped)) +
  geom_histogram()


corpus_extended_core3 <- ggplot(BE_extended[ESpecialite_grouped!="Economics"]
                                , aes(x=Annee_Bibliographique, y=share_of_discipline, group=ESpecialite_grouped, color=ESpecialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2016)) +
  scale_y_continuous("Share disicpline")

######################### set 2 (auteurs info) #########################
authors_of_extended_core <- merge(BE_extended, all_aut[,.(ID_Art, Nom)], by = "ID_Art", all.x = TRUE, all.y = FALSE)
#Remove second name
authors_of_extended_core <- authors_of_extended_core[, name_short:= paste0(unlist(strsplit(Nom,"-"))[1],"-", substr(unlist(strsplit(Nom,"-"))[2],1,1) 
), by=list(ID_Art,Nom)]
#Upper all
authors_of_extended_core$name_short <- toupper(authors_of_extended_core$name_short)

corpus_extended_core5 <- ggplot(authors_of_extended_core[,.(.N), name_short][order(N)][,tail(.SD,10)], aes(x=name_short, y=N)) +
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=.3, fill="tomato2") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6))


plot3 <- (corpus_extended_core1 + corpus_extended_core2) / corpus_extended_core3
plot4 <- (corpus_core6 + corpus_core5) / corpus_core4

saveRDS(plot3, file = "Plots/plot3.RDS")
saveRDS(plot4, file = "Plots/plot4.RDS")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART III : DRAWING MY CORPUS FOR NETWORK ANALYSIS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#







#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Let's draw the core####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### Mapping it #########################
BE_core_autocit <- all_ref[ItemID_Ref %in% BE_core$ItemID_Ref & ID_Art %in% BE_core$ID_Art]
BE_core_autocit <- merge(BE_core_autocit[,.(ID_Art_Source = ID_Art, ItemID_Ref_Target = ItemID_Ref, Annee_Target = Annee)], all_art[,.(Titre_Source = Titre, Annee_Bibliographique, Code_Revue, ID_Art)], by.x = "ID_Art_Source", by.y = "ID_Art")
BE_core_autocit <- merge(BE_core_autocit, all_art[,.(ID_Art_Target = ID_Art, ItemID_Ref)], by.x = "ItemID_Ref_Target", by.y = "ItemID_Ref")

setkey(BE_core_autocit, ID_Art_Source)

#### Edges ####
edges_BE_core_autocit <- BE_core_autocit[,.(ID_Art_Source,ID_Art_Target)]
#### Nodes ####
adjency_BE_core_autocit<- merge(BE_core[,.(ID_Art, Titre, Annee_Bibliographique, Code_Revue, citations_by_JEL)], revues[,.(Code_Revue, Code_Discipline)], by="Code_Revue")
adjency_BE_core_autocit <- merge(adjency_BE_core_autocit[,.(ID_Art, Titre, Annee_Bibliographique, Code_Revue, Code_Discipline)], disciplines[,.(ESpecialite, Code_Discipline)], by="Code_Discipline")
adjency_BE_core_autocit <- adjency_BE_core_autocit[,.(Id = ID_Art, Titre, Annee_Bibliographique, ESpecialite)]

write.csv(edges_BE_core_autocit, file = "Networks/edges_BE_core_autocit.csv", row.names=FALSE)
write.csv(adjency_BE_core_autocit, file = "Networks/adjency_BE_core_autocit.csv", row.names=FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Let's draw the extended core####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### Mapping it #########################
BE_core_extended_autocit <- all_ref[ItemID_Ref %in% BE_extended$ItemID_Ref & ID_Art %in% BE_extended$ID_Art]
BE_core_extended_autocit <- merge(BE_core_extended_autocit[,.(ID_Art_Source = ID_Art, ItemID_Ref_Target = ItemID_Ref, Annee_Target = Annee)], all_art[,.(Titre_Source = Titre, Annee_Bibliographique, Code_Revue, ID_Art)], by.x = "ID_Art_Source", by.y = "ID_Art")
BE_core_extended_autocit <- merge(BE_core_extended_autocit, all_art[,.(ID_Art_Target = ID_Art, ItemID_Ref)], by.x = "ItemID_Ref_Target", by.y = "ItemID_Ref")

setkey(BE_core_extended_autocit, ID_Art_Source)

#### Edges ####
edge_BE_core_extended_autocit <- BE_core_extended_autocit[,.(ID_Art_Source,ID_Art_Target)]
#### Nodes ####
node_BE_core_extended_autocit<- merge(BE_extended[,.(ID_Art, Titre, Annee_Bibliographique, Code_Revue, core)], revues[,.(Code_Revue, Code_Discipline)], by="Code_Revue")
node_BE_core_extended_autocit <- merge(node_BE_core_extended_autocit[,.(ID_Art, Titre, Annee_Bibliographique, Code_Revue, Code_Discipline)], disciplines[,.(ESpecialite, Code_Discipline)], by="Code_Discipline")
node_BE_core_extended_autocit <- node_BE_core_extended_autocit[,.(Id = ID_Art, Titre, Annee_Bibliographique, ESpecialite)]
node_BE_core_extended_autocit[,ESpecialite_grouped := "Other"]; node_BE_core_extended_autocit[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]

write.csv(edge_BE_core_extended_autocit, file = "Networks/edges_BE_extended_core_autocit.csv", row.names=FALSE)
write.csv(node_BE_core_extended_autocit, file = "Networks/nodes_BE_extended_core_autocit.csv", row.names=FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Coupling#### (in progress)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
all_ref_temp <-  dbGetQuery(ESH, paste0("SELECT Annee, Nom, ID_Art, New_id2, ItemID_Ref 
                                   FROM OST_Expanded_SciHum.References7 WHERE New_id2!=0;")) %>%  data.table

######################### Some ggploting #########################

#most cited book
all_ref_temp[ID_Art %in% BE_extended$ID_Art][ItemID_Ref==0][,.(.N, Nom, Annee), .(New_id2)][order(-N)][,head(.SD,1),(New_id2)][,head(.SD,30)]
#most cited coument
all_ref_temp[ID_Art %in% BE_extended$ID_Art][,.(ID_Art_source=ID_Art, .N, Nom, Annee), .(New_id2)][order(-N)][,head(.SD,1),(New_id2)][,head(.SD,10)]
#most cited document by years
top_ref_extended <- all_ref_temp[ID_Art %in% BE_extended$ID_Art]
top_ref_extended <- top_ref_extended[,Annee_mode:=Mode(Annee), New_id2][,Annee:=NULL] #mode y
top_ref_extended <- top_ref_extended[,Nom_mode:=Mode(Nom), New_id2][,Nom:=NULL] #mode name
top_ref_extended <- top_ref_extended[,Label:=paste0(Nom_mode,",",Annee_mode)]# getting label
top_ref_extended <- merge(top_ref_extended, all_art[,.(ID_Art, Annee_Bibliographique)], by = "ID_Art")
top_ref_extended <- top_ref_extended[,n_references:=.N, Annee_Bibliographique]
#getting the top by decades
top_ref_extended[,annee_regrouped := "<1990"]
top_ref_extended[Annee_Bibliographique >= 1990 & Annee_Bibliographique < 2000,annee_regrouped := "90-99"]
top_ref_extended[Annee_Bibliographique >= 2000 & Annee_Bibliographique < 2010,annee_regrouped := "00-09"]
top_ref_extended[Annee_Bibliographique >= 2010 & Annee_Bibliographique < 2020,annee_regrouped := "10-19"]
top_ref_extended_decade <- top_ref_extended[,.(citation_by_decade=.N, Label, New_id2, Annee_Bibliographique),.(annee_regrouped, New_id2)]
top_ref_extended_decade <- top_ref_extended_decade[order(-citation_by_decade)][,head(.SD,1),.(annee_regrouped, New_id2)]#keep unique id by deacde
top_ref_extended_decade <- top_ref_extended_decade[order(-citation_by_decade)][,head(.SD,5),.(annee_regrouped)]#keep unique top x by deacde
top_ref_extended_decade <- top_ref_extended_decade[,head(.SD,1),.(New_id2)][,Annee_Bibliographique:=NULL]#keep unique documents x by deacde
#getting the top by decades in my top_ref_extended
top_ref_extended <- top_ref_extended[New_id2 %in% top_ref_extended_decade$New_id2]
top_ref_extended <- top_ref_extended[,.(.N, Label, n_references),.(Annee_Bibliographique, New_id2)][order(-N)][,head(.SD,1),.(Annee_Bibliographique, New_id2)]
top_ref_extended[Annee_Bibliographique==2015,Label_repel:=Label]


plot_most_cited <- ggplot(top_ref_extended[Annee_Bibliographique!=2016,norm:=N/n_references]
                          , aes(x=Annee_Bibliographique, y=norm, group=Label, color=Label)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Number of references", limits = c(0,0.005)) +
  scale_color_manual(values=c("red", "blue", "orange", "darkred", "purple", "grey", "violet", "cyan3", "grey", "grey", "grey", "darkblue"),guide = FALSE) +
  geom_label_repel(aes(label = Label_repel),
                   nudge_x = 3,
                   na.rm = TRUE,
                   size = 3)# garder uniquement les références 

saveRDS(plot_most_cited, file = "Plots/plot_most_cited.RDS")


ggplot(temp.dat, aes(x = Year, y = Capex, group = State, colour = State)) + 
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +

######################### Coupling it (in progress) #########################
#Getting Edges (45k with 10)
cocitation_newiD2_edges <- function(x, y ### x=df of corpus, y=minimum number of connections
){
  #getting all ref from corpus
  cocitation <- all_ref_temp[ID_Art %in% x$ID_Art]
  ######keeping only relevnat columns
  cocitation <- cocitation[,.(ID_Art,New_id2)]
  setkey(cocitation, ID_Art, New_id2)
  # dropping articles citing only one document
  cocitation <-  cocitation[,nb_ref :=.N,by=ID_Art][nb_ref>1][,nb_ref:=NULL]
  # creating all links between cocited docs
  cocitation <- cocitation[,list(Target = rep(New_id2[1:(length(New_id2)-1)],(length(New_id2)-1):1)
                                        , Source = rev(New_id2)[sequence((length(New_id2)-1):1)])
                                  ,by= ID_Art]
  # counting the number of identical links across citing articles
  cocitation <- cocitation[,.N, by=.(Target,Source)]
  # cosin weighting
  #cocitation <- cocitation[,target_n:=.N^2, by= Target]
  #cocitation <- cocitation[,source_n:=.N^2, by= Source]
  #cocitation <- cocitation[,Weight:=N/(sqrt(target_n)*sqrt(source_n))]
  #keeeping only if weight is above x
  cocitation <- cocitation[N>y]
  return(cocitation) # utilser cette ligne pour sortir un objet.
}
edges_BE_extended_cocit <- cocitation_newiD2_edges(BE_extended, 10)
write.csv(edges_BE_extended_cocit, file = "Networks/edges_BE_extended_cocit.csv", row.names=FALSE)

#Getting Nodes
cocitation_newiD2_nodes <- function(x ### x=df of edges
){
  bib_coup_nodes <- all_ref_temp[New_id2 %in% x$Target | New_id2 %in% x$Source]
  bib_coup_nodes <- bib_coup_nodes[,.(Annee, Nom), .(New_id2)]
  setkey(bib_coup_nodes, New_id2, Annee)
  bib_coup_nodes[,Annee_mode:=Mode(Annee), New_id2][,Annee:=NULL]
  bib_coup_nodes[,Nom_mode:=Mode(Nom), New_id2][,Nom:=NULL]
  bib_coup_nodes <- bib_coup_nodes[, head(.SD, 1), .(New_id2)]
  colnames(bib_coup_nodes)[colnames(bib_coup_nodes) == "New_id2"] <- "Id"
  bib_coup_nodes <- bib_coup_nodes[,Label:=paste0(Nom_mode,",",Annee_mode)]
  return(bib_coup_nodes) # utilser cette ligne pour sortir un objet.
}
nodes_BE_extended_cocit <- cocitation_newiD2_nodes(edges_BE_extended_cocit)
write.csv(nodes_BE_extended_cocit, file = "Networks/nodes_BE_extended_cocit.csv", row.names=FALSE)

#per year
BE_extended[,annee_regrouped := "<1980"]
BE_extended[Annee_Bibliographique >= 1980 & Annee_Bibliographique < 1990,annee_regrouped := "80-89"]
BE_extended[Annee_Bibliographique >= 1990 & Annee_Bibliographique < 2000,annee_regrouped := "90-99"]
BE_extended[Annee_Bibliographique >= 2000 & Annee_Bibliographique < 2010,annee_regrouped := "00-09"]
BE_extended[Annee_Bibliographique >= 2010 & Annee_Bibliographique < 2020,annee_regrouped := "10-19"]
BE_extended_70 <- BE_extended[annee_regrouped == "<1980"]
BE_extended_80 <- BE_extended[annee_regrouped == "80-89"]
BE_extended_90 <- BE_extended[annee_regrouped == "90-99"]
BE_extended_00 <- BE_extended[annee_regrouped == "00-09"]
BE_extended_10 <- BE_extended[annee_regrouped == "10-19"]
edges_BE_extented_cocit_70 <- cocitation_newiD2_edges(BE_extended_70, 5)
edges_BE_extented_cocit_80 <- cocitation_newiD2_edges(BE_extended_80, 7)
edges_BE_extented_cocit_90 <- cocitation_newiD2_edges(BE_extended_90, 10)
edges_BE_extented_cocit_00 <- cocitation_newiD2_edges(BE_extended_00, 10)
edges_BE_extented_cocit_10 <- cocitation_newiD2_edges(BE_extended_10, 10)
write.csv(edges_BE_extented_cocit_70, file = "Networks/edges_BE_core_cocit_70.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_80, file = "Networks/edges_BE_core_cocit_80.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_90, file = "Networks/edges_BE_core_cocit_90.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_00, file = "Networks/edges_BE_core_cocit_00.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_10, file = "Networks/edges_BE_core_cocit_10.csv", row.names=FALSE)

rm(all_ref_temp)
gc()


######################### Weighting it (in progress) #########################

# fetching the number of citations per cited document (new_id2)
new_id2_w_nb_cit <-  coupling_autocit_BE[,.(nb_cit =.N),by=new_id2]
# getting the number of citations for all Target
bib_coup <-  merge(bib_coup,new_id2_w_nb_cit,by.x = "Target",by.y="new_id2")
setnames(bib_coup,"nb_cit","nb_cit_Target")
# getting the number of citations for all Source
bib_coup <-  merge(bib_coup,new_id2_w_nb_cit,by.x = "Source",by.y="new_id2")
setnames(bib_coup,"nb_cit","nb_cit_Source")
# Applying the normalization blindly as if it is bibliographic coupling!
bib_coup[,norm_weight := N/sqrt(nb_cit_Source*nb_cit_Target)]
bib_coup[,`:=`(nb_cit_Target = NULL, nb_cit_Source = NULL)]
# Checking the distribution 
setkey(bib_coup,norm_weight)




#henrich appear 105 times in my corpus but not in my co-citation network
henrich <-  dbGetQuery(ESH, paste0("SELECT * FROM OST_Expanded_SciHum.References7 WHERE Nom LIKE 'HENRICH-J' AND Annee=2004;")) %>%  data.table
henrich[Revue_Abbrege %like% "FDN"][ID_Art %in% BE_extended$ID_Art]
######################### Exporting with conditions #########################






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART IV : ANALYSIS OF CITATIOnS AND REFERENCES  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ANALYSIS of BE_core ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### citations and references to/of core #########################
citations_to_core <- all_ref[ItemID_Ref %in% BE_core$ItemID_Ref]
references_of_core <- all_ref[ID_Art %in% BE_core$ID_Art]
######################### info about references #########################
#replacing ItemID_Ref by ID_Art
references_of_core <- merge(references_of_core[,.(ID_Art_source_BE = ID_Art, ItemID_Ref)], all_art[,.(ID_Art_target = ID_Art, ItemID_Ref)], by = "ItemID_Ref")

#getting info about our ID_Art_source
references_of_core <- merge(references_of_core, all_art[,.(ID_Art, Titre_source=Titre, Annee_Bibliographique_source=Annee_Bibliographique, Code_Revue_source=Code_Revue)], by.x = "ID_Art_source_BE", by.y = "ID_Art")
references_of_core <- merge(references_of_core, revues[,.(Code_Revue_source=Code_Revue, Code_Discipline_source=Code_Discipline, Revue_source=Revue)], by.x = "Code_Revue_source", by.y = "Code_Revue_source", all = FALSE)
references_of_core <- merge(references_of_core, disciplines[,.(Code_Discipline_source=Code_Discipline, ESpecialite_source=ESpecialite)], by = "Code_Discipline_source", all = FALSE)
#getting info about our ID_Art_target
references_of_core <- merge(references_of_core, all_art[,.(ID_Art, Code_Revue_target=Code_Revue)], by.x = "ID_Art_target", by.y = "ID_Art")
references_of_core <- merge(references_of_core, revues[,.(Code_Revue_target=Code_Revue, Code_Discipline_target=Code_Discipline)], by.x = "Code_Revue_target", by.y = "Code_Revue_target", all = FALSE)
references_of_core <- merge(references_of_core, disciplines[,.(Code_Discipline_target=Code_Discipline, ESpecialite_target=ESpecialite)], by = "Code_Discipline_target", all = FALSE)

#grouping disciplines
references_of_core[,ESpecialite_grouped_source := "Other"]; references_of_core[ESpecialite_source %in% list_discipline$disciplines, ESpecialite_grouped_source := ESpecialite_source]
references_of_core[,ESpecialite_grouped_target:= "Other"]; references_of_core[ESpecialite_target %in% list_discipline$disciplines, ESpecialite_grouped_target := ESpecialite_target]

#share of citations by year
references_of_core[,n_references_by_year:=.N, Annee_Bibliographique_source]
references_of_core[,share_references_by_year:=.N/n_references_by_year, .(Annee_Bibliographique_source,ESpecialite_grouped_target)]


######################### info about citations #########################
#replacing ItemID_Ref by ID_Art
citations_to_core <- merge(citations_to_core[,.(ID_Art_source_BE = ID_Art, ItemID_Ref)], all_art[,.(ID_Art_target = ID_Art, ItemID_Ref)], by = "ItemID_Ref")

#getting info about our ID_Art_source
citations_to_core <- merge(citations_to_core, all_art[,.(ID_Art, Titre_source=Titre, Annee_Bibliographique_source=Annee_Bibliographique, Code_Revue_source=Code_Revue)], by.x = "ID_Art_source_BE", by.y = "ID_Art")
citations_to_core <- merge(citations_to_core, revues[,.(Code_Revue_source=Code_Revue, Code_Discipline_source=Code_Discipline, Revue_source=Revue)], by.x = "Code_Revue_source", by.y = "Code_Revue_source", all = FALSE)
citations_to_core <- merge(citations_to_core, disciplines[,.(Code_Discipline_source=Code_Discipline, ESpecialite_source=ESpecialite)], by = "Code_Discipline_source", all = FALSE)
#getting info about our ID_Art_target
citations_to_core <- merge(citations_to_core, all_art[,.(ID_Art, Code_Revue_target=Code_Revue)], by.x = "ID_Art_target", by.y = "ID_Art")
citations_to_core <- merge(citations_to_core, revues[,.(Code_Revue_target=Code_Revue, Code_Discipline_target=Code_Discipline)], by.x = "Code_Revue_target", by.y = "Code_Revue_target", all = FALSE)
citations_to_core <- merge(citations_to_core, disciplines[,.(Code_Discipline_target=Code_Discipline, ESpecialite_target=ESpecialite)], by = "Code_Discipline_target", all = FALSE)

#grouping disciplines
citations_to_core[,ESpecialite_grouped_source := "Other"]; citations_to_core[ESpecialite_source %in% list_discipline$disciplines, ESpecialite_grouped_source := ESpecialite_source]
citations_to_core[,ESpecialite_grouped_target:= "Other"]; citations_to_core[ESpecialite_target %in% list_discipline$disciplines, ESpecialite_grouped_target := ESpecialite_target]

#share of citations by year
citations_to_core[,n_citations_by_year:=.N, Annee_Bibliographique_source];citations_to_core[,share_citations_by_year:=.N/n_citations_by_year, .(Annee_Bibliographique_source,ESpecialite_grouped_target)]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ggplot it####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### set 1 (discipline info) #########################
#seeing distribution by disciplines of references
reference1_core <- ggplot(references_of_core[Annee_Bibliographique_source>1980 & Annee_Bibliographique_source<2016]
                          , aes(x=Annee_Bibliographique_source, y=share_references_by_year, group=ESpecialite_grouped_target, color=ESpecialite_grouped_target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Share of disicplines in references")


reference2_core <- ggplot(references_of_core[,.(.N), .(Annee_Bibliographique_source, ESpecialite_grouped_target)][N>10][Annee_Bibliographique_source>1970 & Annee_Bibliographique_source<2016]
                          , aes(x=Annee_Bibliographique_source, y=N, group=ESpecialite_grouped_target, color=ESpecialite_grouped_target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles by discipline in references")

#seeing distribution by disciplines of citations

citations1_core <- ggplot(citations_to_core[Annee_Bibliographique_source>1980 & Annee_Bibliographique_source<2016]
                          , aes(x=Annee_Bibliographique_source, y=share_citations_by_year, group=ESpecialite_grouped_target, color=ESpecialite_grouped_target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Share of disciplines in citations")

citations2_core <- ggplot(citations_to_core[,.(.N), .(Annee_Bibliographique_source, ESpecialite_grouped_target)][N>10][Annee_Bibliographique_source>1970 & Annee_Bibliographique_source<2016]
                          , aes(x=Annee_Bibliographique_source, y=N, group=ESpecialite_grouped_target, color=ESpecialite_grouped_target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles by discipline in citations")

reference1_core/citations1_core


plot5 <- reference1_core/citations1_core
saveRDS(plot5, file = "Plots/plot5.RDS")

plot6 <- reference2_core/citations2_core
saveRDS(plot6, file = "Plots/plot6.RDS")

citations_to_core[ESpecialite_grouped_source=="Behavioral Science & Complementary Psychology"][,.N,Revue_source][order(-N)][,head(.SD,10)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ANALYSIS of BE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### citations and references to/of core #########################
citations_to_BE_extended <- all_ref[ItemID_Ref %in% BE_extended$ItemID_Ref]
references_of_BE_extended <- all_ref[ID_Art %in% BE_extended$ID_Art]

######################### info about references #########################
#replacing ItemID_Ref by ID_Art
references_of_BE_extended <- merge(references_of_BE_extended[,.(ID_Art_source_BE = ID_Art, ItemID_Ref)], all_art[,.(ID_Art_target = ID_Art, ItemID_Ref)], by = "ItemID_Ref")

#getting info about our ID_Art_source
references_of_BE_extended <- merge(references_of_BE_extended, all_art[,.(ID_Art, Titre_source=Titre, Annee_Bibliographique_source=Annee_Bibliographique, Code_Revue_source=Code_Revue)], by.x = "ID_Art_source_BE", by.y = "ID_Art")
references_of_BE_extended <- merge(references_of_BE_extended, revues[,.(Code_Revue_source=Code_Revue, Code_Discipline_source=Code_Discipline, Revue_source=Revue)], by.x = "Code_Revue_source", by.y = "Code_Revue_source", all = FALSE)
references_of_BE_extended <- merge(references_of_BE_extended, disciplines[,.(Code_Discipline_source=Code_Discipline, ESpecialite_source=ESpecialite)], by = "Code_Discipline_source", all = FALSE)
#getting info about our ID_Art_target
references_of_BE_extended <- merge(references_of_BE_extended, all_art[,.(ID_Art, Code_Revue_target=Code_Revue)], by.x = "ID_Art_target", by.y = "ID_Art")
references_of_BE_extended <- merge(references_of_BE_extended, revues[,.(Code_Revue_target=Code_Revue, Code_Discipline_target=Code_Discipline)], by.x = "Code_Revue_target", by.y = "Code_Revue_target", all = FALSE)
references_of_BE_extended <- merge(references_of_BE_extended, disciplines[,.(Code_Discipline_target=Code_Discipline, ESpecialite_target=ESpecialite)], by = "Code_Discipline_target", all = FALSE)

#grouping disciplines
references_of_BE_extended[,ESpecialite_grouped_source := "Other"]; references_of_BE_extended[ESpecialite_source %in% list_discipline$disciplines, ESpecialite_grouped_source := ESpecialite_source]
references_of_BE_extended[,ESpecialite_grouped_target:= "Other"]; references_of_BE_extended[ESpecialite_target %in% list_discipline$disciplines, ESpecialite_grouped_target := ESpecialite_target]

#share of citations by year
references_of_BE_extended[,n_references_by_year:=.N, Annee_Bibliographique_source];references_of_BE_extended[,share_references_by_year:=.N/n_references_by_year, .(Annee_Bibliographique_source,ESpecialite_grouped_target)]

######################### info about citations #########################
#replacing ItemID_Ref by ID_Art
citations_to_BE_extended <- merge(citations_to_BE_extended[,.(ID_Art_source_BE = ID_Art, ItemID_Ref)], all_art[,.(ID_Art_target = ID_Art, ItemID_Ref)], by = "ItemID_Ref")

#getting info about our ID_Art_source
citations_to_BE_extended <- merge(citations_to_BE_extended, all_art[,.(ID_Art, Titre_source=Titre, Annee_Bibliographique_source=Annee_Bibliographique, Code_Revue_source=Code_Revue)], by.x = "ID_Art_source_BE", by.y = "ID_Art")
citations_to_BE_extended <- merge(citations_to_BE_extended, revues[,.(Code_Revue_source=Code_Revue, Code_Discipline_source=Code_Discipline, Revue_source=Revue)], by.x = "Code_Revue_source", by.y = "Code_Revue_source", all = FALSE)
citations_to_BE_extended <- merge(citations_to_BE_extended, disciplines[,.(Code_Discipline_source=Code_Discipline, ESpecialite_source=ESpecialite)], by = "Code_Discipline_source", all = FALSE)
#getting info about our ID_Art_target
citations_to_BE_extended <- merge(citations_to_BE_extended, all_art[,.(ID_Art, Code_Revue_target=Code_Revue)], by.x = "ID_Art_target", by.y = "ID_Art")
citations_to_BE_extended <- merge(citations_to_BE_extended, revues[,.(Code_Revue_target=Code_Revue, Code_Discipline_target=Code_Discipline)], by.x = "Code_Revue_target", by.y = "Code_Revue_target", all = FALSE)
citations_to_BE_extended <- merge(citations_to_BE_extended, disciplines[,.(Code_Discipline_target=Code_Discipline, ESpecialite_target=ESpecialite)], by = "Code_Discipline_target", all = FALSE)

#grouping disciplines
citations_to_BE_extended[,ESpecialite_grouped_source := "Other"]; citations_to_BE_extended[ESpecialite_source %in% list_discipline$disciplines, ESpecialite_grouped_source := ESpecialite_source]
citations_to_BE_extended[,ESpecialite_grouped_target:= "Other"]; citations_to_BE_extended[ESpecialite_target %in% list_discipline$disciplines, ESpecialite_grouped_target := ESpecialite_target]

#share of citations by year
citations_to_BE_extended[,n_citations_by_year:=.N, Annee_Bibliographique_source];citations_to_BE_extended[,share_citations_by_year:=.N/n_citations_by_year, .(Annee_Bibliographique_source,ESpecialite_grouped_target)]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ggplot it####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### set 1 (discipline info) #########################
#seeing distribution by disciplines of references
reference1 <- ggplot(references_of_BE_extended[Annee_Bibliographique_source>1980 & Annee_Bibliographique_source<2016]
                     , aes(x=Annee_Bibliographique_source, y=share_references_by_year, group=ESpecialite_grouped_target, color=ESpecialite_grouped_target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Share of disicplines in references")


reference2 <- ggplot(references_of_BE_extended[,.(.N), .(Annee_Bibliographique_source, ESpecialite_grouped_target)][N>10][Annee_Bibliographique_source>1970 & Annee_Bibliographique_source<2016]
                                , aes(x=Annee_Bibliographique_source, y=N, group=ESpecialite_grouped_target, color=ESpecialite_grouped_target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles by discipline in references", trans = 'log2')

#seeing distribution by disciplines of citations

citations1 <- ggplot(citations_to_BE_extended[Annee_Bibliographique_source>1980 & Annee_Bibliographique_source<2016]
                     , aes(x=Annee_Bibliographique_source, y=share_citations_by_year, group=ESpecialite_grouped_target, color=ESpecialite_grouped_target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Share of disciplines in citations")

citations2 <- ggplot(citations_to_BE_extended[,.(.N), .(Annee_Bibliographique_source, ESpecialite_grouped_target)][N>10][Annee_Bibliographique_source>1970 & Annee_Bibliographique_source<2016]
                     , aes(x=Annee_Bibliographique_source, y=N, group=ESpecialite_grouped_target, color=ESpecialite_grouped_target)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles by discipline in citations")

plot7 <- reference1/citations1
saveRDS(plot7, file = "Plots/plot7.RDS")

plot8 <- reference2/citations2
saveRDS(plot8, file = "Plots/plot8.RDS")










#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART V : ANALYSIS OF COMMUNITIES  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### TF-IDF Communities ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### ggplot #########################
Communities <- read.csv(file = "Networks/Communities_exteded_24-02-1.1.csv") %>%  data.table
Communities <- Communities[,modularity_class:=modularity_class+1]
Communities <- transform(Communities, modularity_class = as.character(modularity_class))

#seeing distribution by disciplines of our core extended
gg_plot_communities <- Communities[especialite_grouped=="Economics"|especialite_grouped=="Management", especialite_grouped:="Economics"];Communities[especialite_grouped!="Economics" & especialite_grouped!="Management", especialite_grouped:="Other"]
gg_plot_communities <- gg_plot_communities[,n_articles_by_year:=.N, .(annee_bibliographique, modularity_class)]
gg_plot_communities[,share_articles_by_year:=.N/n_articles_by_year, .(annee_bibliographique, modularity_class, especialite_grouped)]

#plot
communities_BE_extended_plot1 <- ggplot(gg_plot_communities[annee_bibliographique>=1980 & annee_bibliographique<2015]
                            , aes(x=annee_bibliographique, y=share_articles_by_year, group=especialite_grouped, color=especialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles") +
  facet_grid(rows = vars(modularity_class))


communities_BE_extended_plot2 <- ggplot(gg_plot_communities[annee_bibliographique>=1980 & annee_bibliographique<2015]
                            , aes(x=annee_bibliographique, y=n_articles_by_year, group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles")

plot9 <- communities_BE_extended_plot1
saveRDS(plot9, file = "Plots/plot9.RDS")

plot10 <- communities_BE_extended_plot2
saveRDS(plot10, file = "Plots/plot10.RDS")

######################### communities info #########################
setkey(gg_plot_communities, modularity_class, indegree)
#topc aritlces
gg_plot_communities[, tail(.SD, 3), .(modularity_class)][,.(titre, modularity_class)]

#mean year
gg_plot_communities[, mean(annee_bibliographique), modularity_class][order(V1)]

#top authors
gg_plot_communities_aut <- merge(gg_plot_communities, all_aut[,.(ID_Art, Nom)], by.x = "Id", by.y = "ID_Art", all.x = TRUE, all.y = FALSE)
#Remove second name
gg_plot_communities_aut <- gg_plot_communities_aut[, name_short:= paste0(unlist(strsplit(Nom,"-"))[1],"-", substr(unlist(strsplit(Nom,"-"))[2],1,1) 
), by=list(Id,Nom)]
#Upper all
gg_plot_communities_aut$name_short <- toupper(gg_plot_communities_aut$name_short)
gg_plot_communities_aut <- gg_plot_communities_aut[,n_articles_by_aut:=.N, .(modularity_class, name_short)]
gg_plot_communities_aut <- gg_plot_communities_aut[,sum_citations_by_aut:=sum(indegree), .(modularity_class, name_short)]
#most present authors by communities
setkey(gg_plot_communities_aut, modularity_class, n_articles_by_aut)
gg_plot_communities_aut[,.N,.(modularity_class, name_short)][,tail(.SD, 3), modularity_class]
       
ggplot(gg_plot_communities_aut[,.N,.(modularity_class, name_short)][,tail(.SD, 5), modularity_class], 
        aes(x=name_short, y=N))+
  geom_bar(stat='identity', fill="forest green") + 
  facet_wrap(~modularity_class, scales = "free_y") + 
  coord_flip()

#most cited authors by communities
setkey(gg_plot_communities_aut, modularity_class, sum_citations_by_aut)
gg_plot_communities_aut[,.N,.(modularity_class, name_short, sum_citations_by_aut)][,tail(.SD, 3), modularity_class]

ggplot(gg_plot_communities_aut[,.N,.(modularity_class, name_short, sum_citations_by_aut)][,tail(.SD, 5), modularity_class], 
       aes(x=name_short, y=N))+
  geom_bar(stat='identity', fill="forest green") + 
  facet_wrap(~modularity_class, scales = "free_y") + 
  coord_flip()


#ref of communities
ref_communities <- gg_plot_communities[,.(ID_Art=Id,modularity_class, Annee_source=annee_bibliographique)]
ref_communities <- get_disc_ref(ref_communities)
ref_communities <- ref_communities[,n_ref:=.N, .(modularity_class, Annee_source)]
ref_communities <- ref_communities[ESpecialite=="Psychology", n_psycho:=.N, .(modularity_class, Annee_source)][order(n_psycho)]
ref_communities <- ref_communities[ESpecialite=="Psychology"]
ref_communities <- ref_communities[, share_psy:=(n_psycho/n_ref)]
ref_communities[,.(share_psy), .(modularity_class, Annee_source)]

ref_communities_disc <- ggplot(ref_communities
                                        , aes(x=Annee_source, y=share_psy, group=modularity_class, color=modularity_class)) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.90, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2020)) +
  scale_y_continuous("Number of articles")

ref_communities[,.N,.(modularity_class, ESpecialite)][order(N)][,tail(.SD, 3), modularity_class]
ggplot(ref_communities[,.N,.(modularity_class, ESpecialite)][order(N)][,tail(.SD, 3), modularity_class], 
       aes(x=ESpecialite, y=N))+
  geom_bar(stat='identity', fill="forest green") + 
  facet_wrap(~modularity_class, scales = "free_y") + 
  coord_flip()





######################### tf #########################
Communities <- read.csv(file = "Networks/Communities_exteded_24-02-1.1.csv") %>%  data.table
colnames(Communities)[colnames(Communities)=="Id"] <- "ID_Art"

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
plot11 <- bind_tf_idf(Communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:12)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf words in HOPE communities",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 6, scales = "free") +
  coord_flip()

saveRDS(plot11, file = "Plots/plot11.RDS")




















#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART VI : IGRAPH  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Plotting #########################
#keeping only the mostcited articles
BE_core_autocit <- BE_core_autocit[,n_citations:=.N,(ID_Art_Target)]
BE_core_autocit <- BE_core_autocit[,.(ID_Art_Source,ID_Art_Target, Titre_Source)]

#color
adjency_BE_core_autocit[,color:="green"]
adjency_BE_core_autocit[ESpecialite == "Economics", color:="red"]
adjency_BE_core_autocit[ESpecialite == "Behavioral Science & Complementary Psychology", color:="blue"]

#getting the graph
g <- graph_from_data_frame(BE_core_autocit,  directed=TRUE, vertices=adjency_BE_core_autocit)
#keeping the main component
g <- induced_subgraph(g, V(g)[components(g)$members == which.max(components(g)$csize)])
#layout
l <- layout_with_fr(g)
#plot
network1 <-plot(g, layout=l, vertex.size=5, vertex.label=NA, edge.arrow.size=.1)
saveRDS(network1, file = "Plots/network1.RDS")

######################### Communities #########################
#communities
cfg <- cluster_louvain(as.undirected(g))
#plot
network2 <- plot(cfg, g, vertex.size=5, vertex.label=NA, edge.arrow.size=.1)
saveRDS(network2, file = "Plots/network2.RDS")

#transform in table
igraph_membership <- cfg$membership
igraph_nodes <- as_ids(V(g))
igraph_communities <- data.frame(igraph_nodes,igraph_membership)
#merging
igraph_communities <- merge(x = igraph_communities, y = adjency_BE_core_autocit[,.(Id,Titre, Annee_Bibliographique, ESpecialite)], by.x = "igraph_nodes", by.y = "Id") %>% as.data.table()


######################### Distribution #########################

#grouping disicplines
igraph_communities[, especialite_grouped:="Other"]
igraph_communities[ESpecialite=="Economics"|ESpecialite=="Management", especialite_grouped:="Economics"]
igraph_communities[ESpecialite=="Behavioral Science & Complementary Psychology", especialite_grouped:="Psychology"]
#counting things
igraph_communities <- igraph_communities[,n_articles_by_year:=.N, .(Annee_Bibliographique, igraph_membership)]
igraph_communities[,share_articles_by_year:=.N/n_articles_by_year, .(Annee_Bibliographique, igraph_membership, especialite_grouped)]
igraph_communities <- igraph_communities[,.community_size_by_year:=.N, .(Annee_Bibliographique, igraph_membership)]
#communities as character
igraph_communities <- igraph_communities[,igraph_membership := as.character(igraph_membership)]

#plot
igraph_communities_plot1 <- ggplot(igraph_communities[Annee_Bibliographique>=1980 & Annee_Bibliographique<2015]
                                        , aes(x=Annee_Bibliographique, y=share_articles_by_year, group=especialite_grouped, color=especialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles") +
  facet_grid(rows = vars(igraph_membership))


igraph_communities_plot2 <- ggplot(igraph_communities[Annee_Bibliographique>=1980 & Annee_Bibliographique<2015]
                                        , aes(x=Annee_Bibliographique, y=.community_size_by_year, group=igraph_membership, color=igraph_membership)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles")

plot12 <- igraph_communities_plot1
saveRDS(plot12, file = "Plots/plot12.RDS")

plot13 <- igraph_communities_plot2
saveRDS(plot13, file = "Plots/plot13.RDS")


######################### Words #########################

igraph_communities <- igraph_communities %>% group_by(igraph_membership) %>% summarise(titre = paste(Titre, collapse=", "))
igraph_communities <- VCorpus(VectorSource(igraph_communities$titre))

igraph_communities <- tm_map(igraph_communities, stripWhitespace)
igraph_communities <- tm_map(igraph_communities, removePunctuation)
igraph_communities <- tm_map(igraph_communities, content_transformer(tolower))
igraph_communities <- tm_map(igraph_communities, removeWords, stopwords("english"))
igraph_communities <- tm_map(igraph_communities, stemDocument)

igraph_communities <- DocumentTermMatrix(igraph_communities)

#Communities <- DocumentTermMatrix(Communities, control = list(weighting = weightTfIdf))



# convert dtm into a df
igraph_communities <- tidy(igraph_communities)

# take the product of tf and idf and create new column labeled "tf_idf". Graph it. 
plot14 <- bind_tf_idf(igraph_communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:12)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf words in HOPE communities",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 6, scales = "free") +
  coord_flip()

saveRDS(plot14, file = "Plots/plot14.RDS")
















######################### Plotting #########################
#keeping only the mostcited articles
BE_core_autocit <- BE_core_extended_autocit[,n_citations:=.N,(ID_Art_Target)][n_citations>25]
BE_core_autocit <- BE_core_autocit[,.(ID_Art_Source,ID_Art_Target, Titre_Source)]

#color
node_BE_core_extended_autocit[,color:="green"]
node_BE_core_extended_autocit[ESpecialite == "Economics", color:="red"]
node_BE_core_extended_autocit[ESpecialite == "Behavioral Science & Complementary Psychology", color:="blue"]

#getting the graph
g <- graph_from_data_frame(BE_core_autocit,  directed=TRUE, vertices=node_BE_core_extended_autocit)
#keeping the main component
g <- induced_subgraph(g, V(g)[components(g)$members == which.max(components(g)$csize)])
#layout
l <- layout_with_fr(g)
#plot
network3 <- plot(g, layout=l, vertex.size=2.5, vertex.label=NA, edge.arrow.size=.1)
saveRDS(network3, file = "Plots/network3.RDS")

######################### Communities #########################
#communities
cfg <- cluster_louvain(as.undirected(g))
#plot
network4 <- plot(cfg, g, vertex.size=5, vertex.label=NA, edge.arrow.size=.1)
saveRDS(network4, file = "Plots/network4.RDS")

#transform in table
igraph_membership <- cfg$membership
igraph_nodes <- as_ids(V(g))
igraph_communities <- data.frame(igraph_nodes,igraph_membership)
#merging
igraph_communities <- merge(x = igraph_communities, y = adjency_BE_core_autocit[,.(Id,Titre, Annee_Bibliographique, ESpecialite)], by.x = "igraph_nodes", by.y = "Id") %>% as.data.table()


######################### Distribution #########################

#grouping disicplines
igraph_communities[, especialite_grouped:="Other"]
igraph_communities[ESpecialite=="Economics"|ESpecialite=="Management", especialite_grouped:="Economics"]
igraph_communities[ESpecialite=="Behavioral Science & Complementary Psychology", especialite_grouped:="Psychology"]
#counting things
igraph_communities <- igraph_communities[,n_articles_by_year:=.N, .(Annee_Bibliographique, igraph_membership)]
igraph_communities[,share_articles_by_year:=.N/n_articles_by_year, .(Annee_Bibliographique, igraph_membership, especialite_grouped)]
igraph_communities <- igraph_communities[,.community_size_by_year:=.N, .(Annee_Bibliographique, igraph_membership)]
#communities as character
igraph_communities <- igraph_communities[,igraph_membership := as.character(igraph_membership)]

#plot
igraph_communities_plot3 <- ggplot(igraph_communities[Annee_Bibliographique>=1980 & Annee_Bibliographique<2015]
                                   , aes(x=Annee_Bibliographique, y=share_articles_by_year, group=especialite_grouped, color=especialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles") +
  facet_grid(rows = vars(igraph_membership))


igraph_communities_plot4 <- ggplot(igraph_communities[Annee_Bibliographique>=1980 & Annee_Bibliographique<2015]
                                   , aes(x=Annee_Bibliographique, y=.community_size_by_year, group=igraph_membership, color=igraph_membership)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.90, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years") +
  scale_y_continuous("Number of articles")

plot15 <- igraph_communities_plot3
saveRDS(plot15, file = "Plots/plot15.RDS")

plot16 <- igraph_communities_plot4
saveRDS(plot16, file = "Plots/plot16.RDS")


######################### Words #########################

igraph_communities <- igraph_communities %>% group_by(igraph_membership) %>% summarise(titre = paste(Titre, collapse=", "))
igraph_communities <- VCorpus(VectorSource(igraph_communities$titre))

igraph_communities <- tm_map(igraph_communities, stripWhitespace)
igraph_communities <- tm_map(igraph_communities, removePunctuation)
igraph_communities <- tm_map(igraph_communities, content_transformer(tolower))
igraph_communities <- tm_map(igraph_communities, removeWords, stopwords("english"))
igraph_communities <- tm_map(igraph_communities, stemDocument)

igraph_communities <- DocumentTermMatrix(igraph_communities)

#Communities <- DocumentTermMatrix(Communities, control = list(weighting = weightTfIdf))



# convert dtm into a df
igraph_communities <- tidy(igraph_communities)

# take the product of tf and idf and create new column labeled "tf_idf". Graph it. 
plot17 <- bind_tf_idf(igraph_communities, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))),
         chapter = factor(document, levels = 1:12)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf words in HOPE communities",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 6, scales = "free") +
  coord_flip()

saveRDS(plot17, file = "Plots/plot17.RDS")











































#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART VI : CEMETERY  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#changing column names
coupling_autocit_BE <- merge(coupling_autocit_BE[,.(ID_Art_Source = ID_Art, ItemID_Ref_Target = ItemID_Ref, Annee_Target = Annee)], all_art[,.(Titre_Source = Titre, Annee_Bibliographique, Code_Revue, ID_Art)], by.x = "ID_Art_Source", by.y = "ID_Art")
#getting ID_Art
coupling_autocit_BE <- merge(coupling_autocit_BE, all_art[,.(ID_Art_Target = ID_Art, ItemID_Ref)], by.x = "ItemID_Ref_Target", by.y = "ItemID_Ref")

#TESTESTESTESTETESTETSTETETSTSESTESTESTESTETESTETSTETETSTSESTESTESTESTETESTETSTETETSTSESTESTESTESTETESTETSTETETSTSESTESTESTESTETESTETSTETETSTS
#Trouver le TOP 10 des articles en periodes
#divide by decades
coupling_autocit_BE[,annee_regrouped := "<1980"]
coupling_autocit_BE[Annee_Bibliographique >= 1980 & Annee_Bibliographique < 1990,annee_regrouped := "80-89"]
coupling_autocit_BE[Annee_Bibliographique >= 1990 & Annee_Bibliographique < 2000,annee_regrouped := "90-99"]
coupling_autocit_BE[Annee_Bibliographique >= 2000 & Annee_Bibliographique < 2010,annee_regrouped := "00-09"]
coupling_autocit_BE[Annee_Bibliographique >= 2010 & Annee_Bibliographique < 2019,annee_regrouped := "10-19"]

#keeping the most common references by decades
coupling_autocit_BE <- coupling_autocit_BE[,n_citations:=.N,.(annee_regrouped, ID_Art_Target)]
setkey(coupling_autocit_BE, annee_regrouped, n_citations)
coupling_autocit_BE <- coupling_autocit_BE[,.(ID_Art_Target,annee_regrouped, n_citations)]
coupling_autocit_BE <- unique(coupling_autocit_BE)
coupling_autocit_BE <- coupling_autocit_BE[, tail(.SD, 5), .(annee_regrouped)]
coupling_autocit_BE <- unique(coupling_autocit_BE$ID_Art_Target) #I got my list of most_cited articles within my corpus




#TESTESTESTESTETESTETSTETETSTSESTESTESTESTETESTETSTETETSTSESTESTESTESTETESTETSTETETSTSESTESTESTESTETESTETSTETETSTSESTESTESTESTETESTETSTETETSTS



#keeping the most common references
coupling_autocit_BE <- coupling_autocit_BE[,n_citations:=.N,(ID_Art_Target)][n_citations>150]

coupling_autocit_BE <- coupling_autocit_BE[,.(ID_Art_Source, ID_Art_Target)]
coupling_autocit_BE <- unique(coupling_autocit_BE$ID_Art_Target) #I got my list of most_cited articles within my corpus

######getting the references of these articles
coupling_autocit_BE <- all_ref[ID_Art %in% coupling_autocit_BE]
######keeping only relevnat columns
coupling_autocit_BE <- coupling_autocit_BE[,.(ID_Art,ItemID_Ref)]


#### Weird stuff to transform into list ####
coupling_autocit_BE <- graph_from_data_frame(coupling_autocit_BE, directed=TRUE, vertices=NULL)
coupling_autocit_BE <- bibcoupling(coupling_autocit_BE)
coupling_autocit_BE <- graph.adjacency(coupling_autocit_BE)
coupling_autocit_BE <- get.edgelist(coupling_autocit_BE) %>% data.table
write.csv(coupling_autocit_BE, file = "Networks/coupling_autocit_BE.csv", row.names=FALSE)

coupling_autocit_BE <- all_art[ID_Art %in% coupling_autocit_BE$ID_Art & ID_Art %in% coupling_autocit_BE$ID_Art]
