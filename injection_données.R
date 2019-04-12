setwd("C:/Users/Acer/Desktop/programmation/donnees_bio500_2019")
library(RSQLite)
tables.db <- dbConnect(SQLite(), dbname="tables.db")
#creation de la table cours
cours_sql<- "CREATE TABLE cours (sigle CHAR(6),pratique BOLEAN(1),credits INTEGER(1),concentration BOLEAN(1),option BOLEAN(1),PRIMARY KEY(sigle));"
dbSendQuery(tables.db,cours_sql)
#assemblage et injection des donnÃ©es
bd_cours<-rbind(bd_cours_1,bd_cours_2,bd_cours_3,bd_cours_4,bd_cours_5,bd_cours_6,bd_cours_7)
bd_cours<-unique(bd_cours)
dbWriteTable(tables.db,append=TRUE,name="cours",value=bd_cours, row.names=FALSE)
#vérification
sql_requete <- "SELECT *FROM cours;"
apercu<-dbGetQuery(tables.db,sql_requete)
apercu

#creation de la table etudiants
etudiants_sql <- "CREATE TABLE etudiants (id VARCHAR,prenom VARCHAR,nom VARCHAR,sexe CHAR(1),naissance INTEGER(4),faune BOLEAN(1),programme BOLEAN(1),pays_naissance CHAR(2),region_naissance INTEGER(2),diete CHAR(2),PRIMARY KEY(id) FOREIGN KEY(id) REFERENCES liens(etudiant1));"
dbSendQuery(tables.db,etudiants_sql)
#assemblage et injection des donnÃ©es
bd_etudiants<-rbind(bd_etudiants_1,bd_etudiants_2,bd_etudiants_3,bd_etudiants_4,bd_etudiants_5,bd_etudiants_6,bd_etudiants_7)
dbWriteTable(tables.db,append=TRUE,name="etudiants",value=bd_etudiants, row.names=FALSE)
#vérification
sql_requete2 <- "SELECT *FROM etudiants;"
apercu2<-dbGetQuery(tables.db,sql_requete2)
apercu2

#creation de la table liens
liens_sql <- "CREATE TABLE liens (sigle CHAR(6),etudiant1 VARCHAR,etudiant2 VARCCHAR,session CHAR(3),id VARCHAR,PRIMARY KEY(id) FOREIGN KEY(sigle) REFERENCES cours(sigle), FOREIGN KEY(etudiant1) REFERENCES etudiants (id) ON DELETE CASCADE, FOREIGN KEY(etudiant2) REFERENCES etudiants (id) ON DELETE CASCADE);"
dbSendQuery(tables.db,liens_sql)
#mettre données ensemble
bd_liens<-rbind(bd_liens_1,bd_liens_2,bd_liens_3,bd_liens_4,bd_liens_5,bd_liens_6,bd_liens_7)
bd_liens<-unique(bd_liens)
#Liens dupliquer
liens_dup<-bd_liens[,c(1,3,2,4)]
names(liens_dup)[2]<-'etudiant1'
names(liens_dup)[3]<-'etudiant2'
bd_liens<- rbind(bd_liens,liens_dup)
bd_liens<-unique(bd_liens)
#id
bd_liens[,5]<- paste0(bd_liens$sigle,'-',bd_liens$etudiant1,"-", bd_liens$etudiant2,'-',bd_liens$session)
names(bd_liens)[5]<-'id'
#injection de données
dbWriteTable(tables.db,append=TRUE,name="liens",value=bd_liens, row.names=FALSE)
#vérification
sql_requete3 <- "SELECT *FROM liens;"
apercu3<-dbGetQuery(tables.db,sql_requete3)
apercu3

#creation de la table liens avec fréquence
liens_freq_sql <- "CREATE TABLE liensfreq (Var1 VARCHAR,Var2 VARCCHAR,Freq INTEGER(2),id VARCHAR,PRIMARY KEY(id) FOREIGN KEY(Var1) REFERENCES etudiants (id) ON DELETE CASCADE, FOREIGN KEY(Var2) REFERENCES etudiants (id) ON DELETE CASCADE);"
dbSendQuery(tables.db,liens_freq_sql)
#Créer table liens avec fréquences
mat_liens<-table(bd_liens$etudiant1, bd_liens$etudiant2)
mat_liens<-as.matrix(mat_liens)
bd_liens_freq<-data.frame(mat_liens)
bd_liens_freq<-subset(bd_liens_freq,bd_liens_freq$Var1%in%bd_etudiants$id==T)
bd_liens_freq1<-data.frame(table(bd_liens$etudiant1,bd_liens$etudiant2))
bd_liens_freq1<-subset(bd_liens_freq1,bd_liens_freq1$Var1%in%bd_etudiants$id==T)
for (j in 1:length(bd_liens_freq1$Var1)) {
  if (bd_liens_freq1[j,1]==bd_liens_freq1[j,2]) bd_liens_freq=bd_liens_freq[-j,]
}
#id
bd_liens_freq[,4]<- paste0(bd_liens_freq$Var1,'-',bd_liens_freq$Var2)
names(bd_liens_freq)[4]<-'id'
#injection de données
dbWriteTable(tables.db,append=TRUE,name="liensfreq",value=bd_liens_freq, row.names=FALSE)
#vérification
sql_requete3 <- "SELECT *FROM liensfreq;"
apercu3<-dbGetQuery(tables.db,sql_requete3)
apercu3
