#produire igraph
install.packages("igraph")
library(igraph)

mat_liens<-table(bd_liens$etudiant1, bd_liens$etudiant2)
mat_liens<-as.matrix(mat_liens)
mat_liens_classe<-mat_liens[levels(bd_etudiants$id),levels(bd_etudiants$id)]
mat_liens_classe_table<-as.matrix(mat_liens_classe)
mat_liens_classe_table

g<-graph.adjacency(mat_liens)

#Créer le tableau des liens entre les étudiants de la classe
library(knitr)
library(kableExtra)
install.packages('kableExtra', repos='https://cran.ma.imperial.ac.uk/')
writeLines(knitr::kable(mat_liens_classe, format = 'latex'),con = 'matrice_etudiants.tex')  %>%
  row_spec(0, angle = -45)
# Je veux le mettre dans latex
# \input{matrice_etudiants.tex}


# Calculer le degré
deg <- apply(mat_liens, 2, sum) + apply(mat_liens, 1, sum)
# Le rang pour chaque noeud
rk <- rank(deg)
# Faire un code de couleur
col.vec <- heat.colors(149)
# Attribuer aux noeuds la couleur
V(g)$color = col.vec[rk]
# Refaire la figure
plot(g, vertex.label=NA, edge.arrow.mode = 0,
     vertex.frame.color = NA)
# Faire un code de ctaille
col.vec <- seq(10, 25, length.out = 149)
# Attribuer aux noeuds la couleur
V(g)$size = col.vec[rk]
#distance entre les noeuds
distances(g)

#mettre les étudiants classe au centre des étudiants hors classe
layout_in_circles <- function(g, group=1) {
  layout <- lapply(split(V(g), group), function(x) {
    layout_in_circle(induced_subgraph(g,x))
  })
  layout <- Map(`*`, layout, seq_along(layout))
  x <- matrix(0, nrow=vcount(g), ncol=2)
  split(x, group) <- layout
  x
}

z<-row.names(mat_liens)
z<-data.frame(z)
k<-as.vector(bd_etudiants$id)
k<-data.frame(k)
j<-z$z%in%k$bd_etudiants.id
j<-charmatch(z, k, nomatch =F)
for (j in 1:155) {
  if (z[j,1]%in%k$k) z[j,1]=F
}
z
edge.betweenness(g)
col.vec1 <- heat.colors(5)
degrade<-c('Nombre de liens minimal','Peu de liens','Nombre moyen de liens','Beaucoup de liens','Nombre de liens maximal')

plot(g,vertex.label.cex=0.5,vertex.label.color="black",edge.arrow.mode = 0,
     vertex.frame.color = NA,vertex.size=12,edge.width=0.1,
     layout=layout_in_circles(g,group =is.na(z)))
legend(bty = "n",
       legend=degrade,x=-3.2,y=1.5,
       fill=col.vec1, border=NA)
#ouvrir pdf
pdf("graph.reseau.pdf",width = 6,height = 6) 
# 2. créer graph
plot(g, vertex.label=NA,edge.arrow.mode = 0,
     vertex.frame.color = NA,vertex.size=12,edge.width=0.1,
     layout=layout_in_circles(g,group =!is.na(z)))
legend(bty = "n",
       legend=degrade,x=-1.79,y=1.53,
       fill=col.vec1, border=NA)
# fermer pdf
dev.off() 

plot(g, vertex.label=NA,edge.arrow.mode = 0,
     vertex.frame.color = NA,vertex.size=12,edge.width=0.1,
     layout=layout_in_circles(g,group =!is.na(z)))
legend(bty = "n",
       legend=degrade,x=-3.5,y=1.5,
       fill=col.vec1, border=NA)
#pour envoyer en pdf pareil comme sur rstudio: dev.copy2pdf(file = "graph.test.pdf")
#Même chose pour juste les étudiants de la classe
gg<-graph.adjacency(mat_liens_classe)
# Calculer le degré
deggg <- apply(mat_liens_classe, 2, sum) + apply(mat_liens_classe, 1, sum)
# Le rang pour chaque noeud
rkgg <- rank(deggg)
# Faire un code de couleur
col.vecgg <- heat.colors(26)
# Attribuer aux noeuds la couleur
V(gg)$color = col.vecgg[rkgg]
# Faire un code de ctaille
col.vecgg <- seq(10, 25, length.out = 26)
# Attribuer aux noeuds la couleur
V(gg)$size = col.vecgg[rkgg]
plot(gg)
#Mettre en pdf
#ouvrir pdf
pdf("graph.reseau.classe.pdf",width = 6,height = 6) 
# 2. créer graph
plot(gg, vertex.label=rownames(mat_liens_classe),vertex.label.cex=0.65,vertex.label.color="black",edge.arrow.mode = 0,
     vertex.frame.color = NA,vertex.size=16,edge.width=0.1,layout = layout.kamada.kawai(gg))
legend(bty = "n",
       legend=degrade,x=-1.79,y=1.53,
       fill=col.vec1, border=NA)
# fermer pdf
dev.off()

plot(gg, vertex.label=rownames(mat_liens_classe),vertex.label.cex=0.65,vertex.label.color="black",edge.arrow.mode = 0,
     vertex.frame.color = NA,vertex.size=16,edge.width=0.1, layout = layout.kamada.kawai(gg))
legend(bty = "n",
       legend=degrade,x=-1.79,y=1.53,
       fill=col.vec1, border=NA)
#distance entre les noeuds
distances(gg)

#"valentine_glaus","audrey_vallee","alexandra_coutu","jonathan_lorrain",            
#"daphne_dufour","anne-sophie_neron","francois_tremblay","gabriel_boilard",             
#"josiane_cote-audet","kathryne_moreau","elyse_paquette","benjamin_mercier",            
#"noura_barro","cyrille_viens","pier-andre_hinse","emile_chouinard",             
#"alexandre_carbonneau","vincent_boucher","emilie_desjardins","jenny-ann_rioux",             
#"eloise_roy","maria-elisa_aparicio-velasco","eliane_lego","alexandre_racine",            
#"isabelle_banville","carole-anne_dumaine"

#Autres figures
#nombre de liens avec des personnes différentes en fonction du sexe
bd_liens_freqs<-data.frame(mat_liens)
bd_liens_freq_sans0<-subset(bd_liens_freqs,Freq>0)
liens_etudiants<-table(bd_liens_freq_sans0$Var1)
liens_etudiants<-data.frame(liens_etudiants)
liens_etudiants<-subset(liens_etudiants,liens_etudiants$Var1%in%bd_etudiants$id==T)
liens_etudiants[,3]<-c('f','h','h','f','f','h','f','a','f','f','f','f','h','f','h','h','f','f','h','f','f','f','f','h','f','h')
for (j in 1:26) {
  if (liens_etudiants[j,3]=='f') liens_etudiants[j,3]='femme'
  else if (liens_etudiants[j,3]=='h') liens_etudiants[j,3]='homme'
  else if (liens_etudiants[j,3]=='a') liens_etudiants[j,3]='autre'
}
names(liens_etudiants)[3]<-'sexe'
plot(as.factor(liens_etudiants$sexe),liens_etudiants$Freq,text(x = 19, y = 2.5, labels = 'P-value=0,5783'))
liens_etudiants_cy<-liens_etudiants[-8,]
mod.sexe<-lm(liens_etudiants$Freq~liens_etudiants$sexe)
summary(mod.sexe)
anova(mod.sexe)
#ouvrir pdf
pdf("graph.sexe.pdf",width = 6,height = 6) 
# 2. créer graph
plot(as.factor(liens_etudiants$sexe),liens_etudiants$Freq)
legend('topright',legend = 'P-value=0,578')
# fermer pdf
dev.off()
#sans autre
mod.sexe.cy<-lm(liens_etudiants_cy$Freq~liens_etudiants_cy$sexe)
summary(mod.sexe.cy)
t.test(liens_etudiants_cy$Freq~liens_etudiants_cy$sexe)
plot(as.factor(liens_etudiants_cy$sexe),liens_etudiants_cy$Freq)

#Figure des liens en fonction de la région de naissance
#création de nouvelle table avec région et liens sans table liens avec frequences
sql_requete <- "SELECT liens.etudiant1, liens.etudiant2, et1.region_naissance AS 'region_naissance_et1', et2.region_naissance AS 'region_naissance_et2' FROM liens INNER JOIN etudiants AS et1 ON liens.etudiant1 = et1.id INNER JOIN etudiants AS et2 ON liens.etudiant2 = et2.id;"
liens_region <- dbGetQuery(tables.db,sql_requete)
#table avec fréquences des liens
liens_region_freq<-data.frame(table(liens_region$etudiant1,liens_region$etudiant2))
#ordonner les liens de la même façon les frequences et région
liens_region_unique<-unique(liens_region)
liens_region_unique<-liens_region_unique[order(liens_region_unique$etudiant1),]
liens_region_freq<-liens_region_freq[order(liens_region_freq$Var1),]
liens_region_unique<-liens_region_unique[order(liens_region_unique$etudiant2),]
liens_region_freq<-liens_region_freq[order(liens_region_freq$Var2),]

##Figure des liens en fonction de la région de naissance
#création de nouvelle table avec région et liens à partir de la table liensfreq de sql
sql_requete <- "SELECT liensfreq.Freq, liensfreq.Var1, liensfreq.Var2, et1.region_naissance AS 'region_naissance_et1', et2.region_naissance AS 'region_naissance_et2' FROM liensfreq INNER JOIN etudiants AS et1 ON liensfreq.Var1 = et1.id INNER JOIN etudiants AS et2 ON liensfreq.Var2 = et2.id;"
liens_region1 <- dbGetQuery(tables.db,sql_requete)
#mettre si même région ou pas
liens_region<-liens_region1
for (j in 1:650) {
  if (is.na(liens_region1[j,4])==T) liens_region[j,6]=NA
  else if (is.na(liens_region1[j,5])==T) liens_region[j,6]=NA
  else if (liens_region1[j,4]==liens_region1[j,5]) liens_region[j,6]=1
  else if (liens_region1[j,4]!=liens_region1[j,5]) liens_region[j,6]=0
}
names(liens_region)[6]<-'meme_region'
fff<-glm(liens_region$Freq~liens_region$meme_region,family = 'poisson')
summary(fff)
anova(fff)
plot(liens_region$meme_region, liens_region$Freq)
graph.3<-plot(jitter(liens_region$meme_region),liens_region$Freq,xlab = 'Région de naissance identique ou pas',ylab = 'Fréquence des liens')
legend('topright',legend = 'P-value=0,748')
t.test(liens_region$Freq~liens_region$meme_region)
#ouvrir pdf
pdf("graph.region.pdf",width = 6,height = 6) 
# 2. créer graph
plot(jitter(liens_region$meme_region),liens_region$Freq,xlab = 'Région de naissance identique ou pas',ylab = 'Fréquence des liens')
legend('topright',legend = 'P-value=0,748')
# fermer pdf
dev.off()
#Sans les liens de fréquence 0
liens_region0<-subset(liens_region, liens_region$Freq>0)
plot(jitter(liens_region0$meme_region),liens_region0$Freq,xlab = 'Région de naissance identique ou pas',ylab = 'Fréquence des liens')
legend('topright',legend = 'P-value=0,0625')
zzz<-glm(liens_region0$Freq~liens_region0$meme_region,family = 'poisson')
summary(zzz)
t.test(liens_region0$Freq~liens_region0$meme_region)

#controler pour la probabilité d'être en liens avec quelqu'un de la même région en prenant comme supposition que la distribution de la classe est représentative de l'école en moyenne et que chaque région est représentée également
levels(bd_etudiants$region_naissance)
pond<-data.frame(table(bd_etudiants$region_naissance))
pond0<-((pond[1,2]/26)+(pond[2,2]/26)+(pond[3,2]/26)+(pond[4,2]/26)+(pond[5,2]/26)+(pond[6,2]/26)+(pond[7,2]/26)+(pond[8,2]/26)+(pond[9,2]/26))/9
pond1<-1-pond0
for (j in 1:length(liens_region$Freq)) {
  if (is.na(liens_region[j,6])==T) liens_region[j,1]=liens_region[j,1]*pond0
  else if (liens_region[j,6]==0) liens_region[j,1]=liens_region[j,1]*pond0
  else if (liens_region[j,6]==1) liens_region[j,1]=liens_region[j,1]*pond1
}
plot(liens_region$Freq~liens_region$meme_region)
fff<-glm(liens_region$Freq~liens_region$meme_region,family = 'poisson')
summary(fff)
t.test(liens_region0$Freq~liens_region0$meme_region)
boxplot(liens_region$Freq~liens_region$meme_region)
