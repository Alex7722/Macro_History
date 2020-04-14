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
######################### Directory ***************************
setwd("/projects/data/alexandre/Macro")

######################### Functions ***************************
`%notin%` <- Negate(`%in%`)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
######################### From xml to DT ***************************
# Give the input file name to the function.
from_xml_to_df <- function(x ### x=df of edges
){
  #econlit <- read_xml("BE_JEL6K005.xml")
  econlit <- read_xml(paste0('',x,".xml",''))
  
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
  macro1 <- data.table(Title = title, Year = year, Journal = journal, Vol = vol, No = no, Pages = pages, PubType = pubType)
  
  #Keep only articles
  macro1 <- macro1[PubType=="Journal Article"]
  return(macro1) # utilser cette ligne pour sortir un objet.
}
macro1 <- from_xml_to_df("Econlit/Macro1")
macro2 <- from_xml_to_df("Econlit/Macro2")
macro3 <- from_xml_to_df("Econlit/Macro3")
macro4 <- from_xml_to_df("Econlit/Macro4")
macro5 <- from_xml_to_df("Econlit/Macro5")
macro6 <- from_xml_to_df("Econlit/Macro6")
macro7 <- from_xml_to_df("Econlit/Macro7")
macro8 <- from_xml_to_df("Econlit/Macro8")
macro9 <- from_xml_to_df("Econlit/Macro9")
macro10 <- from_xml_to_df("Econlit/Macro10")

dt_JEL_Articles <- rbind(macro1,macro2,macro3,macro4,macro5,macro6,macro7,macro8,macro9,macro10)
rm(macro1,macro2,macro3,macro4,macro5,macro6,macro7,macro8,macro9,macro10)
gc()

######################### Getting the BD ***************************
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
setkey(all_art, ItemID_Ref)
setkey(all_ref, ItemID_Ref)
#auteurs
all_aut <-  dbGetQuery(ESH, paste0("SELECT OST_Expanded_SciHum.Articles.ID_Art, OST_Expanded_SciHum.Articles.Titre, OST_Expanded_SciHum.Articles.Annee_Bibliographique, OST_Expanded_SciHum.Articles.Code_Revue, OST_Expanded_SciHum.Articles.ItemID_Ref, OST_Expanded_SciHum.Auteurs.Nom
                                   FROM OST_Expanded_SciHum.Articles
                                   JOIN OST_Expanded_SciHum.Auteurs ON OST_Expanded_SciHum.Articles.ID_Art=OST_Expanded_SciHum.Auteurs.ID_Art")) %>%  data.table

issueID <- read.csv("/projects/digital_history/behavioral economics/data/revueID.csv",sep=";") %>% data.table()
#get IssueID for matching
all_art <- merge(all_art, issueID[,.(IssueID,Volume)], by= "IssueID")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Finding the JEL articles in the BD####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Getting all_art running smoothly ***************************
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

######################### Do the same for dt of JEL ***************************
dt_JEL_Articles[,Journal := toupper(Journal)]
dt_JEL_Articles <- transform(dt_JEL_Articles, Year.V1 = as.numeric(Year.V1))
#collapse columns
dt_JEL_Articles <- dt_JEL_Articles[, ID_by_pub := paste(dt_JEL_Articles$Year.V1, dt_JEL_Articles$Journal, dt_JEL_Articles$Pages, dt_JEL_Articles$Vol)]

######################### Merging everything ***************************
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

######################### Cleaning everything and only keeping the ID_Art ***************************
rm(all_art2)
gc()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Now we got our list of articles JEL coded; find the core####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Find the bibliography of our JEL articles ***************************
#Find references
BE_ref <- merge(BE[,.(ID_Art, ItemID_Ref_source = ItemID_Ref, Titre, Annee_Bibliographique, Code_Revue)], all_ref[,.(ID_Art, ItemID_Ref_target = ItemID_Ref)], by="ID_Art")

#Find articles that are at least referenced N times
BE_ref <- BE_ref[,.(.N), by = "ItemID_Ref_target"][order(N)][N>5]
setkey(BE_ref, ItemID_Ref_target)
setkey(all_art, ItemID_Ref)

######################### Get the information of all our new articles and calling it "core" ***************************
BE_ref <- merge(BE_ref[,.(ItemID_Ref=ItemID_Ref_target, citations_by_JEL=N)], all_art[,.(ID_Art, ItemID_Ref, Titre, Annee_Bibliographique, Code_Revue)], by.x = "ItemID_Ref", by.y = "ItemID_Ref", all = FALSE)
BE_ref <- merge(BE_ref, revues, by = "Code_Revue", all = FALSE)
BE_core <- merge(BE_ref, disciplines[,.(Code_Discipline, ESpecialite)], by = "Code_Discipline", all = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Getting more from our core ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### citations and references to/of core ***************************
citations_to_core <- all_ref[ItemID_Ref %in% BE_core$ItemID_Ref]
references_of_core <- all_ref[ID_Art %in% BE_core$ID_Art]

######################### info about citations to ***************************
BE_core_extended_citations <- citations_to_core[,.(number_of_articles_cited_in_core = .N), ID_Art][order(number_of_articles_cited_in_core)][number_of_articles_cited_in_core>10]

######################### info about references ***************************
BE_core_extended_references <- references_of_core[,.(number_of_citations_by_core = .N), ItemID_Ref][order(number_of_citations_by_core)][number_of_citations_by_core>10]
#replacing ItemID_Ref by ID_Art
BE_core_extended_references <- merge(BE_core_extended_references, all_art[,.(ID_Art, ItemID_Ref)], by = "ItemID_Ref")
BE_core_extended_references <- BE_core_extended_references[,.(ID_Art, number_of_citations_by_core)]

######################### binding ***************************
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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART II : Plotting our corpus and extended corpus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################### Graph **********************
ggplot(BE_extended[,.(.N), .(Annee_Bibliographique)][order(N)]
       , aes(x=Annee_Bibliographique, y=N)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1970, 2018)) +
  scale_y_continuous("Number of Articles")
ggsave("Graphs/Corpus2.png", width=286, height=215, units = "mm")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ggplot CORE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### set 1 (corpus info) ***************************
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

######################### set 2 (auteurs info) ***************************
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
######################### set 1 (corpus info) ***************************
setkey(BE_extended, Annee_Bibliographique)
#seeing distribution by years of our core extended
corpus_extended_core1 <- ggplot(BE_extended, aes(x=Annee_Bibliographique)) + 
  geom_histogram(color="black", fill="white")

######################### set 2 (discipline info) ***************************
#seeing distribution of disciplines of our core extended
BE_extended[,.(.N), .(ESpecialite)][order(N)]
BE_extended[,.(.N), .(Annee_Bibliographique, ESpecialite)]

#seeing distribution by disciplines of our core extended
corpus_extended_core2 <- ggplot(BE_extended, aes(x=Annee_Bibliographique, color=ESpecialite_grouped, fill=ESpecialite_grouped)) +
  geom_histogram()


corpus_extended_core3 <- ggplot(BE_extended
                                , aes(x=Annee_Bibliographique, y=share_of_discipline, group=ESpecialite_grouped, color=ESpecialite_grouped)) +
  geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95, span = 0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Years", limits = c(1980, 2016)) +
  scale_y_continuous("Share disicpline")


######################### set 2 (auteurs info) ***************************
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
######################### Mapping it ***************************
BE_core_autocit <- all_ref[ItemID_Ref %in% BE_core$ItemID_Ref & ID_Art %in% BE_core$ID_Art]
BE_core_autocit <- merge(BE_core_autocit[,.(ID_Art_Source = ID_Art, ItemID_Ref_Target = ItemID_Ref, Annee_Target = Annee)], all_art[,.(Titre_Source = Titre, Annee_Bibliographique, Code_Revue, ID_Art)], by.x = "ID_Art_Source", by.y = "ID_Art")
BE_core_autocit <- merge(BE_core_autocit, all_art[,.(ID_Art_Target = ID_Art, ItemID_Ref)], by.x = "ItemID_Ref_Target", by.y = "ItemID_Ref")

setkey(BE_core_autocit, ID_Art_Source)

#### Edges %%%%
edges_BE_core_autocit <- BE_core_autocit[,.(ID_Art_Source,ID_Art_Target)]
#### Nodes %%%%
adjency_BE_core_autocit<- merge(BE_core[,.(ID_Art, Titre, Annee_Bibliographique, Code_Revue, citations_by_JEL)], revues[,.(Code_Revue, Code_Discipline)], by="Code_Revue")
adjency_BE_core_autocit <- merge(adjency_BE_core_autocit[,.(ID_Art, Titre, Annee_Bibliographique, Code_Revue, Code_Discipline)], disciplines[,.(ESpecialite, Code_Discipline)], by="Code_Discipline")
adjency_BE_core_autocit <- adjency_BE_core_autocit[,.(Id = ID_Art, Titre, Annee_Bibliographique, ESpecialite)]

write.csv(edges_BE_core_autocit, file = "Networks/edges_BE_core_autocit.csv", row.names=FALSE)
write.csv(adjency_BE_core_autocit, file = "Networks/adjency_BE_core_autocit.csv", row.names=FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Let's draw the extended core####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### Mapping it ***************************
BE_core_extended_autocit <- all_ref[ItemID_Ref %in% BE_extended$ItemID_Ref & ID_Art %in% BE_extended$ID_Art]
BE_core_extended_autocit <- merge(BE_core_extended_autocit[,.(ID_Art_Source = ID_Art, ItemID_Ref_Target = ItemID_Ref, Annee_Target = Annee)], all_art[,.(Titre_Source = Titre, Annee_Bibliographique, Code_Revue, ID_Art)], by.x = "ID_Art_Source", by.y = "ID_Art")
BE_core_extended_autocit <- merge(BE_core_extended_autocit, all_art[,.(ID_Art_Target = ID_Art, ItemID_Ref)], by.x = "ItemID_Ref_Target", by.y = "ItemID_Ref")

setkey(BE_core_extended_autocit, ID_Art_Source)

#### Edges %%%%
edge_BE_core_extended_autocit <- BE_core_extended_autocit[,.(ID_Art_Source,ID_Art_Target)]
#### Nodes %%%%
node_BE_core_extended_autocit<- merge(BE_extended[,.(ID_Art, Titre, Annee_Bibliographique, Code_Revue, core)], revues[,.(Code_Revue, Code_Discipline)], by="Code_Revue")
node_BE_core_extended_autocit <- merge(node_BE_core_extended_autocit[,.(ID_Art, Titre, Annee_Bibliographique, Code_Revue, Code_Discipline)], disciplines[,.(ESpecialite, Code_Discipline)], by="Code_Discipline")
node_BE_core_extended_autocit <- node_BE_core_extended_autocit[,.(Id = ID_Art, Titre, Annee_Bibliographique, ESpecialite)]
node_BE_core_extended_autocit[,ESpecialite_grouped := "Other"]; node_BE_core_extended_autocit[ESpecialite %in% list_discipline$disciplines, ESpecialite_grouped := ESpecialite]

write.csv(edge_BE_core_extended_autocit, file = "Networks/edges_BE_extended_core_autocit.csv", row.names=FALSE)
write.csv(node_BE_core_extended_autocit, file = "Networks/nodes_BE_extended_core_autocit.csv", row.names=FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### CoCitation ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
all_ref_temp <-  dbGetQuery(ESH, paste0("SELECT Annee, Nom, ID_Art, New_id2 
                                        FROM OST_Expanded_SciHum.References7 WHERE New_id2!=0;")) %>%  data.table


######################### Coupling it (in progress) ***************************
cocitation_base <- all_ref_temp[ID_Art %in% BE_extended$ID_Art]
cocitation_base <- cocitation_base[ID_Art!=New_id2]
cocitation_base <-cocitation_base[, head(.SD, 1), .(ID_Art,New_id2)]
cocitation_base <- cocitation_base[,.(ID_Art,New_id2)]
cocitation_base <- merge(cocitation_base, BE_extended[,.(ID_Art, Annee_Bibliographique)], by = "ID_Art", all.x = TRUE, all.y = FALSE)
cocitation_base <-  cocitation_base[,nb_ref2 :=.N,by=ID_Art][nb_ref2>1][,nb_ref2:=NULL]
cocitation_base[,.N,.(ID_Art, New_id2)][order(N)]

#Getting Edges
cocitation_newiD2_edges <- function(x, y ### x=df of corpus, y=minimum number of connections
){
  cocitation_edgesf <- x[,.(ID_Art, New_id2)]
  setkey(cocitation_edgesf, ID_Art, New_id2)
  # creating all links between cocited docs
  cocitation_edgesf <- cocitation_edgesf[,list(Target = rep(New_id2[1:(length(New_id2)-1)],(length(New_id2)-1):1)
                                               , Source = rev(New_id2)[sequence((length(New_id2)-1):1)])
                                         ,by = ID_Art]
  # remove loop
  cocitation_edgesf <- cocitation_edgesf[Source!=Target]
  # counting the number of identical links across citing articles
  cocitation_edgesf <- cocitation_edgesf[,.N, .(Source,Target)]
  cocitation_edgesf <- cocitation_edgesf[N>y]
  # fetching the number of citations per cited document (new_id2)
  new_id2_w_nb_cit <-  x[,.(nb_cit =.N),New_id2]
  # getting the number of citations for all Target
  cocitation_edgesf <-  merge(cocitation_edgesf,new_id2_w_nb_cit,by.x = "Target",by.y="New_id2")
  setnames(cocitation_edgesf,"nb_cit","nb_cit_Target")
  # getting the number of citations for all Source
  cocitation_edgesf <-  merge(cocitation_edgesf,new_id2_w_nb_cit,by.x = "Source",by.y="New_id2")
  setnames(cocitation_edgesf,"nb_cit","nb_cit_Source")
  
  cocitation_edgesf[,weighted_edge := N/sqrt(nb_cit_Target*nb_cit_Source)]
  colnames(cocitation_edgesf)[colnames(cocitation_edgesf) == "weighted_edge"] <- "Weight"
  
  return(cocitation_edgesf) # utilser cette ligne pour sortir un objet.
}
edges_BE_core_cocit <- cocitation_newiD2_edges(cocitation_base, 10)

write.csv(edges_BE_core_cocit, file = "Networks/edges_BE_extended_cocit.csv", row.names=FALSE)
edges_BE_core_cocit[order(Weight)]
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
nodes_BE_core_cocit <- cocitation_newiD2_nodes(edges_BE_core_cocit)
write.csv(nodes_BE_core_cocit, file = "Networks/nodes_BE_extended_cocit.csv", row.names=FALSE)

#per year
cocitation_base[,annee_regrouped := "<1980"]
cocitation_base[Annee_Bibliographique >= 1980 & Annee_Bibliographique < 1990,annee_regrouped := "80-89"]
cocitation_base[Annee_Bibliographique >= 1990 & Annee_Bibliographique < 2000,annee_regrouped := "90-99"]
cocitation_base[Annee_Bibliographique >= 2000 & Annee_Bibliographique < 2010,annee_regrouped := "00-09"]
cocitation_base[Annee_Bibliographique >= 2010 & Annee_Bibliographique < 2020,annee_regrouped := "10-19"]
BE_extended_70 <- cocitation_base[annee_regrouped == "<1980"]
BE_extended_80 <- cocitation_base[annee_regrouped == "80-89"]
BE_extended_90 <- cocitation_base[annee_regrouped == "90-99"]
BE_extended_00 <- cocitation_base[annee_regrouped == "00-09"]
BE_extended_10 <- cocitation_base[annee_regrouped == "10-19"]
edges_BE_extented_cocit_70 <- cocitation_newiD2_edges(BE_extended_70, 3)
edges_BE_extented_cocit_80 <- cocitation_newiD2_edges(BE_extended_80, 3)
edges_BE_extented_cocit_90 <- cocitation_newiD2_edges(BE_extended_90, 5)
edges_BE_extented_cocit_00 <- cocitation_newiD2_edges(BE_extended_00, 10)
edges_BE_extented_cocit_10 <- cocitation_newiD2_edges(BE_extended_10, 10)
write.csv(edges_BE_extented_cocit_70, file = "Networks/edges_BE_extended_cocit_70.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_80, file = "Networks/edges_BE_extended_cocit_80.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_90, file = "Networks/edges_BE_extended_cocit_90.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_00, file = "Networks/edges_BE_extended_cocit_00.csv", row.names=FALSE)
write.csv(edges_BE_extented_cocit_10, file = "Networks/edges_BE_extended_cocit_10.csv", row.names=FALSE)

nodes_BE_core_cocit_70 <- cocitation_newiD2_nodes(edges_BE_extented_cocit_70)
write.csv(nodes_BE_core_cocit_70, file = "Networks/nodes_BE_extended_cocit_70.csv", row.names=FALSE)
nodes_BE_core_cocit_80 <- cocitation_newiD2_nodes(edges_BE_extented_cocit_80)
write.csv(nodes_BE_core_cocit_80, file = "Networks/nodes_BE_extended_cocit_80.csv", row.names=FALSE)
nodes_BE_core_cocit_90 <- cocitation_newiD2_nodes(edges_BE_extented_cocit_90)
write.csv(nodes_BE_core_cocit_90, file = "Networks/nodes_BE_extended_cocit_90.csv", row.names=FALSE)
nodes_BE_core_cocit_00 <- cocitation_newiD2_nodes(edges_BE_extented_cocit_00)
write.csv(nodes_BE_core_cocit_00, file = "Networks/nodes_BE_extended_cocit_00.csv", row.names=FALSE)
nodes_BE_core_cocit_10 <- cocitation_newiD2_nodes(edges_BE_extented_cocit_10)
write.csv(nodes_BE_core_cocit_10, file = "Networks/nodes_BE_extended_cocit_10.csv", row.names=FALSE)

rm(all_ref_temp)
gc()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Coupling ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
all_ref_temp <-  dbGetQuery(ESH, paste0("SELECT Annee, Nom, ID_Art, New_id2 
                                        FROM OST_Expanded_SciHum.References7 WHERE New_id2!=0;")) %>%  data.table


######################### Coupling it (in progress) ***************************
coupling_base <- all_ref_temp[ID_Art %in% BE_extended$ID_Art] %>%  data.table
coupling_base <- coupling_base[ID_Art!=New_id2]
coupling_base <-coupling_base[, head(.SD, 1), .(ID_Art,New_id2)]
coupling_base <- coupling_base[,.(ID_Art,New_id2)]
coupling_base <- merge(coupling_base, BE_extended[,.(ID_Art, Annee_Bibliographique)], by = "ID_Art", all.x = TRUE, all.y = FALSE)
coupling_base <-  coupling_base[,nb_ref2 :=.N,by=ID_Art][nb_ref2>1][,nb_ref2:=NULL]
coupling_base <-  coupling_base[,nb_ref2 :=.N,by=New_id2][nb_ref2>1][,nb_ref2:=NULL]

coupling_base[,.N,.(ID_Art, New_id2)][order(N)]
coupling_base[New_id2<2]




#Getting Edges
coupling_newiD2_edges <- function(x, y ### x=df of corpus, y=minimum number of connections
){
  cocitation_edgesf <- x[,.(ID_Art, New_id2)]
  setkey(cocitation_edgesf, New_id2, ID_Art)
  
  cocitation_edgesf <-  cocitation_edgesf[,nb_ref2 :=.N,by=New_id2][nb_ref2>1][,nb_ref2:=NULL]
  # creating all links between cocited docs
  cocitation_edgesf <- cocitation_edgesf[,list(Target = rep(ID_Art[1:(length(ID_Art)-1)],(length(ID_Art)-1):1)
                                               , Source = rev(ID_Art)[sequence((length(ID_Art)-1):1)])
                                         ,by = New_id2]
  # remove loop
  cocitation_edgesf <- cocitation_edgesf[Source!=Target]
  # counting the number of identical links across citing articles
  cocitation_edgesf <- cocitation_edgesf[,.N, .(Source,Target)]
  cocitation_edgesf <- cocitation_edgesf[N>y]
  # fetching the number of ref per cited document (new_id2)
  new_id2_w_nb_cit <-  x[,.(nb_cit =.N), ID_Art]
  # getting the number of ref for all Target
  cocitation_edgesf <-  merge(cocitation_edgesf,new_id2_w_nb_cit,by.x = "Target",by.y="ID_Art")
  setnames(cocitation_edgesf,"nb_cit","nb_cit_Target")
  # getting the number of ref for all Source
  cocitation_edgesf <-  merge(cocitation_edgesf,new_id2_w_nb_cit,by.x = "Source",by.y="ID_Art")
  setnames(cocitation_edgesf,"nb_cit","nb_cit_Source")
  
  cocitation_edgesf[,weighted_edge := N/sqrt(nb_cit_Target*nb_cit_Source)]
  colnames(cocitation_edgesf)[colnames(cocitation_edgesf) == "weighted_edge"] <- "Weight"
  
  return(cocitation_edgesf) # utilser cette ligne pour sortir un objet.
}

edges_BE_core_cocit <- coupling_newiD2_edges(coupling_base, 0)

write.csv(edges_BE_core_cocit, file = "Networks/edges_BE_extended_cocit.csv", row.names=FALSE)
edges_BE_core_cocit[order(Weight)]
#Getting Nodes
coupling_newiD2_nodes <- function(x ### x=df of edges
){
  bib_coup_nodes <- BE_extended[ID_Art %in% x$Target | ID_Art %in% x$Source, .(ID_Art, Annee_Bibliographique, ESpecialite, Titre, Revue)]
  colnames(bib_coup_nodes)[colnames(bib_coup_nodes) == "ID_Art"] <- "Id"
  return(bib_coup_nodes) # utilser cette ligne pour sortir un objet.
}
nodes_BE_core_cocit <- cocitation_newiD2_nodes(edges_BE_core_cocit)
write.csv(nodes_BE_core_cocit, file = "Networks/nodes_BE_extended_cocit.csv", row.names=FALSE)

#per year
coupling_base[,annee_regrouped := "<1980"]
coupling_base[Annee_Bibliographique >= 1980 & Annee_Bibliographique < 1990,annee_regrouped := "80-89"]
coupling_base[Annee_Bibliographique >= 1990 & Annee_Bibliographique < 2000,annee_regrouped := "90-99"]
coupling_base[Annee_Bibliographique >= 2000 & Annee_Bibliographique < 2010,annee_regrouped := "00-09"]
coupling_base[Annee_Bibliographique >= 2010 & Annee_Bibliographique < 2020,annee_regrouped := "10-19"]
BE_extended_70 <- coupling_base[annee_regrouped == "<1980"]
BE_extended_80 <- coupling_base[annee_regrouped == "80-89"]
BE_extended_90 <- coupling_base[annee_regrouped == "90-99"]
BE_extended_00 <- coupling_base[annee_regrouped == "00-09"]
BE_extended_10 <- coupling_base[annee_regrouped == "10-19"]
edges_BE_extented_coupling_70 <- coupling_newiD2_edges(BE_extended_70, 3)
edges_BE_extented_coupling_80 <- coupling_newiD2_edges(BE_extended_80, 3)
edges_BE_extented_coupling_90 <- coupling_newiD2_edges(BE_extended_90, 5)
edges_BE_extented_coupling_00 <- coupling_newiD2_edges(BE_extended_00, 10)
edges_BE_extented_coupling_10 <- coupling_newiD2_edges(BE_extended_10, 10)
write.csv(edges_BE_extented_coupling_70, file = "Networks/edges_BE_extented_coupling_70.csv", row.names=FALSE)
write.csv(edges_BE_extented_coupling_80, file = "Networks/edges_BE_extented_coupling_80.csv", row.names=FALSE)
write.csv(edges_BE_extented_coupling_90, file = "Networks/edges_BE_extented_coupling_90.csv", row.names=FALSE)
write.csv(edges_BE_extented_coupling_00, file = "Networks/edges_BE_extented_coupling_00.csv", row.names=FALSE)
write.csv(edges_BE_extented_coupling_10, file = "Networks/edges_BE_extented_coupling_10.csv", row.names=FALSE)

nodes_BE_extended_coupling_70 <- coupling_newiD2_nodes(edges_BE_extented_coupling_70)
write.csv(nodes_BE_extended_coupling_70, file = "Networks/nodes_BE_extended_coupling_70.csv", row.names=FALSE)
nodes_BE_extended_coupling_80 <- coupling_newiD2_nodes(edges_BE_extented_coupling_80)
write.csv(nodes_BE_extended_coupling_80, file = "Networks/nodes_BE_extended_coupling_80.csv", row.names=FALSE)
nodes_BE_extended_coupling_90 <- coupling_newiD2_nodes(edges_BE_extented_coupling_90)
write.csv(nodes_BE_extended_coupling_90, file = "Networks/nodes_BE_extended_coupling_90.csv", row.names=FALSE)
nodes_BE_extended_coupling_00 <- coupling_newiD2_nodes(edges_BE_extented_coupling_00)
write.csv(nodes_BE_extended_coupling_00, file = "Networks/nodes_BE_extended_coupling_00.csv", row.names=FALSE)
nodes_BE_extended_coupling_10 <- coupling_newiD2_nodes(edges_BE_extented_coupling_10)
write.csv(nodes_BE_extended_coupling_10, file = "Networks/nodes_BE_extended_coupling_10.csv", row.names=FALSE)

rm(all_ref_temp)
gc()





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PART IV : ANALYSIS OF CITATIOnS AND REFERENCES  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ANALYSIS of BE_core ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### citations and references to/of core ***************************
citations_to_core <- all_ref[ItemID_Ref %in% BE_core$ItemID_Ref]
references_of_core <- all_ref[ID_Art %in% BE_core$ID_Art]
######################### info about references ***************************
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


######################### info about citations***************************
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
######################### set 1 (discipline info) ***************************
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




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ANALYSIS of BE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
######################### citations and references to/of core ***************************
citations_to_BE_extended <- all_ref[ItemID_Ref %in% BE_extended$ItemID_Ref]
references_of_BE_extended <- all_ref[ID_Art %in% BE_extended$ID_Art]

######################### info about references ***************************
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

######################### info about citations ***************************
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
######################### set 1 (discipline info) ***************************
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
######################### ggplot ***************************
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

######################### communities info ***************************
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


######################### tf ***************************
Communities <- read.csv(file = "Networks/Communities_exteded_24-02-1.1.csv") %>%  data.table
colnames(Communities)[colnames(Communities)=="Id"] <- "ID_Art"

require(tm)
require(bibliometrix)
require(RColorBrewer)
library(janeaustenr)
library(tidytext)

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



