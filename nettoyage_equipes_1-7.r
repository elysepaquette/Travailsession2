

#Equipe 1 Jonathan
bd_liens_1<-read.csv2('bd_liens.csv',sep = ';',header = T)
bd_etudiants_1<-read.csv2('bd_etudiants.csv',sep = ';',header = T)
bd_cours_1<-read.csv2('bd_cours.csv',sep = ';',header = T)

#Nom et nombre des colonnes
colnames(bd_liens_1) = tolower(colnames(bd_liens_1))
colnames(bd_etudiants_1) = tolower(colnames(bd_etudiants_1))
colnames(bd_cours_1) = tolower(colnames(bd_cours_1))
names(bd_etudiants_1)[9]<-'region_naissance'

#Modification accents
bd_liens_1<-data.frame(data.frame(t(iconv(t(bd_liens_1), to="ASCII//TRANSLIT"))))
bd_etudiants_1<-data.frame(data.frame(t(iconv(t(bd_etudiants_1), to="ASCII//TRANSLIT"))))

#Mettre tout en minuscule
bd_liens_1<-data.frame(data.frame(t(tolower(t(bd_liens_1)))))
bd_etudiants_1<-data.frame(data.frame(t(tolower(t(bd_etudiants_1)))))
bd_cours_1<-data.frame(data.frame(t(tolower(t(bd_cours_1)))))

#Liens dupliquer
liens_dup_1<-bd_liens_1[,c(1,3,2,4)]
names(liens_dup_1)[2]<-'etudiant1'
names(liens_dup_1)[3]<-'etudiant2'
bd_liens_1 <- rbind(bd_liens_1,liens_dup_1)

#changer en charactere
bd_liens_1[] <- lapply(bd_liens_1, as.character)

#corriger les noms
for (i in 1:326) {
  if (bd_liens_1[i,2]=='maria_elisa_aparicio_velasco') bd_liens_1[i,2]='maria-elisa_aparicio-velasco'
  else if (bd_liens_1[i,2]=='dave_thibouthot_ste-croix') bd_liens_1[i,2]='dave_thibouthot-ste-croix'
  else if (bd_liens_1[i,2]=='marc-antoine_paul-ouellete') bd_liens_1[i,2]='marc-antoine_paul-ouellet'
  else if (bd_liens_1[i,2]=='guilenne_alejandra_toro') bd_liens_1[i,2]='guilenne_alejandra-toro'
}
for (i in 1:326) {
  if (bd_liens_1[i,3]=='maria_elisa_aparicio_velasco') bd_liens_1[i,3]='maria-elisa_aparicio-velasco'
  else if (bd_liens_1[i,3]=='dave_thibouthot_ste-croix') bd_liens_1[i,3]='dave_thibouthot-ste-croix'
  else if (bd_liens_1[i,3]=='marc-antoine_paul-ouellete') bd_liens_1[i,3]='marc-antoine_paul-ouellet'
  else if (bd_liens_1[i,3]=='guilenne_alejandra_toro') bd_liens_1[i,3]='guilenne_alejandra-toro'
}

bd_cours_1[5,2]<-1
bd_cours_1[6,2]<-0
bd_cours_1[15,2]<-1
bd_cours_1[16,2]<-0
bd_cours_1[4,2]<-0
bd_cours_1[10,2]<-0
bd_cours_1[11,2]<-1
bd_cours_1[13,2]<-1
bd_cours_1[14,2]<-0
bd_cours_1[2,4]<-0
bd_cours_1[4,4]<-0

#Equipe 2


bd_liens_2<-read.csv2('Données_Liens_Anne-So_Daphné_Francois_Formulaire.csv',sep = ';',header = T)
bd_etudiants_2<-read.csv2('Données_Étudiants_Anne-So_Daphné_Francois_Formulaire.csv',sep = ';',header = T)
bd_cours_2<-read.csv2('Données_Cours_Anne-So_Daphné_Francois_Formulaire.csv',sep = ';',header = T,stringsAsFactors = F)

#Nom et nombre des colonnes
colnames(bd_liens_2) = tolower(colnames(bd_liens_2))
colnames(bd_etudiants_2) = tolower(colnames(bd_etudiants_2))
colnames(bd_cours_2) = tolower(colnames(bd_cours_2))
bd_etudiants_2<-data.frame(id=c('daphne_dufour','anne-sophie_neron','francois_tremblay'),bd_etudiants_2)

#Modification accents
bd_liens_2<-data.frame(data.frame(t(iconv(t(bd_liens_2), to="ASCII//TRANSLIT"))))
bd_etudiants_2<-data.frame(data.frame(t(iconv(t(bd_etudiants_2), to="ASCII//TRANSLIT"))))

#Mettre tout en minuscule
bd_liens_2<-data.frame(data.frame(t(tolower(t(bd_liens_2)))))
bd_etudiants_2<-data.frame(data.frame(t(tolower(t(bd_etudiants_2)))))
bd_cours_2<-data.frame(data.frame(t(tolower(t(bd_cours_2)))))

#Remettre les variable dans le format charactere
bd_cours_2[] <- lapply(bd_cours_2, as.character)
bd_etudiants_2[] <- lapply(bd_etudiants_2, as.character)
bd_liens_2[] <- lapply(bd_liens_2, as.character)

#changer les noms
for (i in 1:133) {
  if (bd_liens_2[i,2]=='gabriel_boilard ') bd_liens_2[i,2]='gabriel_boilard'
}
for (i in 1:133) {
  if (bd_liens_2[i,3]=='gabriel_boilard ') bd_liens_2[i,3]='gabriel_boilard'
}
#changer les faux pour des 0 et les vrai pour des 1
#bd_curs
for (i in 1:54) {
  if (bd_cours_2[i,2]=='faux') bd_cours_2[i,2]=0
  else if (bd_cours_2[i,2]=='vrai') bd_cours_2[i,2]=1
}
for (i in 1:54) {
  if (bd_cours_2[i,4]=='faux') bd_cours_2[i,4]=0
  else if (bd_cours_2[i,4]=='vrai') bd_cours_2[i,4]=1
}
for (i in 1:54) {
  if (bd_cours_2[i,5]=='faux') bd_cours_2[i,5]=0
  else if (bd_cours_2[i,5]=='vrai') bd_cours_2[i,5]=1
}
#bd etudiant faux vrai
for (i in 1:3) {
  if (bd_etudiants_2[i,6]=='faux') bd_etudiants_2[i,6]=0
  else if (bd_etudiants_2[i,6]=='vrai') bd_etudiants_2[i,6]=1
}
for (i in 1:3) {
  if (bd_etudiants_2[i,7]=='faux') bd_etudiants_2[i,7]=0
  else if (bd_etudiants_2[i,7]=='vrai') bd_etudiants_2[i,7]=1
}
#bd etudiants male femmelle
for (i in 1:3) {
  if (bd_etudiants_2[i,4]=='m') bd_etudiants_2[i,4]='h'
}

#Liens dupliquer
liens_dup_2<-bd_liens_2[,c(1,3,2,4)]
names(liens_dup_2)[2]<-'etudiant1'
names(liens_dup_2)[3]<-'etudiant2'
bd_liens_2 <- rbind(bd_liens_2,liens_dup_2)

bd_cours_2[10,4]<-0
bd_cours_2[33,4]<-0
bd_cours_2[40,4]<-0

#Equipe 3


bd_liens_3<-read.csv2('Données_Liens_GB_JCA_KM_EP.csv',sep = ',',header = T)
bd_etudiants_3<-read.csv2('Données_Etudiant_GB_JCA_KM_EP.csv',sep = ',',header = T)
bd_cours_3<-read.csv2('Données_Cours_GB_JCA_KM_EP.csv',sep = ',',header = T)

#enlever ligne na
bd_liens_3<-bd_liens_3[-c(25,26),]
#Nom et nombre des colonnes et organisation
colnames(bd_liens_3) = tolower(colnames(bd_liens_3))
colnames(bd_etudiants_3) = tolower(colnames(bd_etudiants_3))
colnames(bd_cours_3) = tolower(colnames(bd_cours_3))
names(bd_liens_3)[2]<-'etudiant1'
names(bd_liens_3)[3]<-'etudiant2'
names(bd_etudiants_3)[9]<-'diete'
bd_etudiants_3<-bd_etudiants_3[,c(3,2,1,4,5,6,7,8,9,10)]
names(bd_liens_3)[1]<-'sigle'


#enlever les etudiants pas rapport
bd_etudiants_3<-bd_etudiants_3[c(4,8,18,20),]

#ajouter info. region et pays et autre vrai faux etc.
bd_etudiants_3$region_naissance<-c(16,0,16,0)
bd_etudiants_3$pays_origine<-c('ca','ca','ca',NA)
bd_etudiants_3$id<-c('gabriel_boilard','josiane_cote-audet','kathryne_moreau','elyse_paquette')
bd_etudiants_3$nom<-c('boilard','cote-audet','moreau','paquette')
bd_etudiants_3$programme<-c(1,1,1,1)
bd_etudiants_3$sexe<-c('h','f','f','f')
names(bd_etudiants_3)[7]<-'faune'
names(bd_etudiants_3)[10]<-'pays_naissance'
names(bd_etudiants_3)[5]<-'naissance'
bd_etudiants_3$naissance<-c(1990,1997,1997,1996)
bd_etudiants_3$faune<-c(0,1,1,0)

#changer en caractere
bd_liens_3[] <- lapply(bd_liens_3, as.character)
bd_liens_3[85:89,1]<-c('bio109','bio109','bio109','bio109','bio109')
names(bd_cours_3)[2]<-'sigle'
bd_cours_3<-bd_cours_3[,c(2,3,4,5,6)]
names(bd_cours_3)[4]<-'concentration'

#Modification accents
bd_liens_3<-data.frame(data.frame(t(iconv(t(bd_liens_3), to="ASCII//TRANSLIT"))))
bd_etudiants_3<-data.frame(data.frame(t(iconv(t(bd_etudiants_3), to="ASCII//TRANSLIT"))))

#Mettre tout en minuscule
bd_liens_3<-data.frame(data.frame(t(tolower(t(bd_liens_3)))))
bd_etudiants_3<-data.frame(data.frame(t(tolower(t(bd_etudiants_3)))))
bd_cours_3<-data.frame(data.frame(t(tolower(t(bd_cours_3)))))

#Liens dupliquer
liens_dup_3<-bd_liens_3[,c(1,3,2,4)]
names(liens_dup_3)[2]<-'etudiant1'
names(liens_dup_3)[3]<-'etudiant2'
bd_liens_3 <- rbind(bd_liens_3,liens_dup_3)

#changer en charactere
bd_liens_3[] <- lapply(bd_liens_3, as.character)
bd_cours_3[] <- lapply(bd_cours_3, as.character)

#corriger les noms
for (i in 1:344) {
  if (bd_liens_3[i,2]=='inconnue') bd_liens_3[i,2]='marie-eve_lachance-foisy'
  else if (bd_liens_3[i,2]=='josiane_cote_audet') bd_liens_3[i,2]='josiane_cote-audet'
  else if (bd_liens_3[i,2]=='laurie_chene_cote') bd_liens_3[i,2]='laurie_chene-cote'
  else if (bd_liens_3[i,2]=='maria_elisa_aparicio_velasco') bd_liens_3[i,2]='maria-elisa_aparicio-velasco'
  else if (bd_liens_3[i,2]=='hadjara-madoccoura-no_barro') bd_liens_3[i,2]='noura_barro'
  else if (bd_liens_3[i,2]=='emile_choiunard') bd_liens_3[i,2]='emile_chouinard'
}
for (i in 1:344) {
  if (bd_liens_3[i,3]=='emile_choiunard') bd_liens_3[i,3]='emile_chouinard'
  else if (bd_liens_3[i,3]=='josiane_cote_audet') bd_liens_3[i,3]='josiane_cote-audet'
  else if (bd_liens_3[i,3]=='laurie_chene_cote') bd_liens_3[i,3]='laurie_chene-cote'
  else if (bd_liens_3[i,3]=='maria_elisa_aparicio_velasco') bd_liens_3[i,3]='maria-elisa_aparicio-velasco'
  else if (bd_liens_3[i,3]=='hadjara-madoccoura-no_barro') bd_liens_3[i,3]='noura_barro'
  else if (bd_liens_3[i,3]=='inconnue') bd_liens_3[i,3]='marie-eve_lachance-foisy'
}
#corriger vrai faux table cours
for (i in 1:22) {
  if (bd_cours_3[i,5]=='f') bd_cours_3[i,5]=0
  else if (bd_cours_3[i,5]=='v') bd_cours_3[i,5]=1
}
for (i in 1:22) {
  if (bd_cours_3[i,4]=='f') bd_cours_3[i,4]=0
  else if (bd_cours_3[i,4]=='v') bd_cours_3[i,4]=1
}
for (i in 1:22) {
  if (bd_cours_3[i,2]=='f') bd_cours_3[i,2]=0
  else if (bd_cours_3[i,2]=='v') bd_cours_3[i,2]=1
}

bd_etudiants_3[4,9]<-NA
bd_cours_3[4,4]<-0
bd_cours_3[2,4]<-0
bd_cours_3[16,2]<-1
bd_cours_3[15,2]<-0
bd_cours_3[10,3]<-2

#ordre table + NA pour r?gion ?lyse et josiane
bd_etudiants_3<-bd_etudiants_3[,c('id','prenom','nom','sexe','naissance','faune','programme','pays_naissance','region_naissance','diete')]
bd_etudiants_3$region_naissance<-c(16,NA,16,NA)

#Equipe 4

bd_liens_4<-read.csv2('data_liens_BM_CV_NB.csv',sep = ';',header = T)
bd_etudiants_4<-read.csv2('data_etudiants_BM_CV_NB.csv',sep = ';',header = T)
bd_cours_4<-read.csv2('data_cours_BM_CV_NB.csv',sep = ';',header = T)

#Nom et nombre des colonnes
colnames(bd_liens_4) = tolower(colnames(bd_liens_4))
colnames(bd_etudiants_4) = tolower(colnames(bd_etudiants_4))
colnames(bd_cours_4) = tolower(colnames(bd_cours_4))

#table etudiants
bd_etudiants_4$region_naissance<-c(16,NA,5)

#Modification accents
bd_liens_4<-data.frame(data.frame(t(iconv(t(bd_liens_4), to="ASCII//TRANSLIT"))))
bd_etudiants_4<-data.frame(data.frame(t(iconv(t(bd_etudiants_4), to="ASCII//TRANSLIT"))))

#Mettre tout en minuscule
bd_liens_4<-data.frame(data.frame(t(tolower(t(bd_liens_4)))))
bd_etudiants_4<-data.frame(data.frame(t(tolower(t(bd_etudiants_4)))))
bd_cours_4<-data.frame(data.frame(t(tolower(t(bd_cours_4)))))

#Liens dupliquer
#pas besoin

#changer en charactere
bd_liens_4[] <- lapply(bd_liens_4, as.character)

#corriger les noms
for (i in 1:324) {
  if (bd_liens_4[i,2]=='timon_janzing_b...') bd_liens_4[i,2]='timon_janzing-bachelet'
  else if (bd_liens_4[i,2]=='mari_elisa_aparicio') bd_liens_4[i,2]='maria-elisa_aparicio-velasco'
  else if (bd_liens_4[i,2]=='vincent moreau') bd_liens_4[i,2]='vincent_moreau'
  else if (bd_liens_4[i,2]=='erika_boisvert ') bd_liens_4[i,2]='erika_boisvert'
} 
for (i in 1:324) {
  if (bd_liens_4[i,3]=='timon_janzing_b...') bd_liens_4[i,3]='timon_janzing-bachelet'
  else if (bd_liens_4[i,3]=='mari_elisa_aparicio') bd_liens_4[i,3]='maria-elisa_aparicio-velasco'
  else if (bd_liens_4[i,3]=='vincent moreau') bd_liens_4[i,3]='vincent_moreau'
  else if (bd_liens_4[i,3]=='erika_boisvert ') bd_liens_4[i,3]='erika_boisvert'
} 
bd_cours_4[14,2]<-0
bd_cours_4[2,4]<-0
bd_cours_4[12,4]<-0
bd_cours_4[17,4]<-0
bd_cours_4<-bd_cours_4[-7,]
bd_etudiants_4[2,8]<-NA

#Equipe 5


bd_liens_5<-read.csv2('Base_Donnees_Liens.csv',sep = ';',header = T)
bd_etudiants_5<-read.csv2('Base_Donnees_Etudiants.csv',sep = ';',header = T)
bd_cours_5<-read.csv2('Base_Donnees_Cours.csv',sep = ';',header = T)

#Nom et nombre des colonnes
colnames(bd_liens_5) = tolower(colnames(bd_liens_5))
colnames(bd_etudiants_5) = tolower(colnames(bd_etudiants_5))
colnames(bd_cours_5) = tolower(colnames(bd_cours_5))
bd_liens_5<-bd_liens_5[,c(1,2,3,4)]
names(bd_liens_5)[2]<-'etudiant1'
names(bd_liens_5)[3]<-'etudiant2'
bd_etudiants_5<-bd_etudiants_5[c(1,2,3,47),]
names(bd_etudiants_5)[5]<-'naissance'
names(bd_etudiants_5)[9]<-'faune'
bd_etudiants_5<-bd_etudiants_5[,1:10]
bd_etudiants_5$faune<-c(0,1,1,1)
bd_etudiants_5$programme<-c(1,1,1,1)
bd_etudiants_5$region_naissance<-c(5,13,5,5)
names(bd_cours_5)[2]<-'credits'
bd_cours_5<-bd_cours_5[,c(1,3,2,4,5)]

#Modification accents
bd_liens_5<-data.frame(data.frame(t(iconv(t(bd_liens_5), to="ASCII//TRANSLIT"))))
bd_etudiants_5<-data.frame(data.frame(t(iconv(t(bd_etudiants_5), to="ASCII//TRANSLIT"))))

#Mettre tout en minuscule
bd_liens_5<-data.frame(data.frame(t(tolower(t(bd_liens_5)))))
bd_etudiants_5<-data.frame(data.frame(t(tolower(t(bd_etudiants_5)))))
bd_cours_5<-data.frame(data.frame(t(tolower(t(bd_cours_5)))))

#duplication liens
#pas besoin

#changer en charactere
bd_liens_5[] <- lapply(bd_liens_5, as.character)
bd_cours_5[] <- lapply(bd_cours_5, as.character)

#corriger les noms
for (i in 1:490) {
  if (bd_liens_5[i,2]=='mc_lenny_jean') bd_liens_5[i,2]='mc-lenny_jean'
  else if (bd_liens_5[i,2]=='maria_elisa_aparicio') bd_liens_5[i,2]='maria-elisa_aparicio-velasco'
  else if (bd_liens_5[i,2]=='mclenny_jean') bd_liens_5[i,2]='mc-lenny_jean'
  else if (bd_liens_5[i,2]=='elise_paquette') bd_liens_5[i,2]='elyse_paquette'
  else if (bd_liens_5[i,2]=='gabi_dupont') bd_liens_5[i,2]='gaby_dupont'
  else if (bd_liens_5[i,2]=='carolle-anne_dumaine') bd_liens_5[i,2]='carole-anne_dumaine'
} 
for (i in 1:490) {
  if (bd_liens_5[i,3]=='mc_lenny_jean') bd_liens_5[i,3]='mc-lenny_jean'
  else if (bd_liens_5[i,3]=='maria_elisa_aparicio') bd_liens_5[i,3]='maria-elisa_aparicio-velasco'
  else if (bd_liens_5[i,3]=='mclenny_jean') bd_liens_5[i,3]='mc-lenny_jean'
  else if (bd_liens_5[i,3]=='elise_paquette') bd_liens_5[i,3]='elyse_paquette'
  else if (bd_liens_5[i,3]=='gabi_dupont') bd_liens_5[i,3]='gaby_dupont'
  else if (bd_liens_5[i,3]=='carolle-anne_dumaine') bd_liens_5[i,3]='carole-anne_dumaine'
} 
#changer bot 400 liens
for (i in 1:490) {
  if (bd_liens_5[i,1]=='bot 400') bd_liens_5[i,1]='bot400'
}
#corriger vrai faux table cours
for (i in 1:24) {
  if (bd_cours_5[i,5]=='f') bd_cours_5[i,5]=0
  else if (bd_cours_5[i,5]=='v') bd_cours_5[i,5]=1
}
for (i in 1:24) {
  if (bd_cours_5[i,4]=='f') bd_cours_5[i,4]=0
  else if (bd_cours_5[i,4]=='v') bd_cours_5[i,4]=1
}
for (i in 1:24) {
  if (bd_cours_5[i,2]=='f') bd_cours_5[i,2]=0
  else if (bd_cours_5[i,2]=='v') bd_cours_5[i,2]=1
}


bd_cours_5[5,4]<-0
bd_cours_5[7,4]<-0
bd_cours_5[1,4]<-0
bd_cours_5[6,4]<-0
bd_cours_5[24,5]<-1
#ordre table
bd_etudiants_5<-bd_etudiants_5[,c(1,2,3,4,5,9,8,6,7,10)]

#Equipe 6

bd_liens_6<-read.csv2('Données_Liens_Desjardins_Rioux_Roi_Aparicio.csv',sep = ';',header = T)
bd_etudiants_6<-read.csv2('Données_Étudiants_Desjardins_Rioux_Roi_Aparicio.csv',sep = ';',header = T)
bd_cours_6<-read.csv2('Données_cours_Desjardins_Rioux_Roi_Aparicio.csv',sep = ';',header = T)

#Nom et nombre des colonnes
colnames(bd_liens_6) = tolower(colnames(bd_liens_6))
colnames(bd_etudiants_6) = tolower(colnames(bd_etudiants_6))
colnames(bd_cours_6) = tolower(colnames(bd_cours_6))
names(bd_liens_6)[3]<-'etudiant1'
names(bd_liens_6)[4]<-'etudiant2'
names(bd_liens_6)[1]<-'sigle'
bd_liens_6<-bd_liens_6[,c('sigle','etudiant1','etudiant2','session')]
names(bd_cours_6)[3]<-'credits'

#en caractere liens
bd_liens_6[] <- lapply(bd_liens_6, as.character)

#sigle dans liens
for(i in 1:162){
  if(bd_liens_6[i,1]=='ZOO 106') bd_liens_6[i,1]='ZOO106'
  else if(bd_liens_6[i,1]=='TSB 303') bd_liens_6[i,1]='TSB303'
  else if(bd_liens_6[i,1]=='BIO 109') bd_liens_6[i,1]='BIO109'
  else if(bd_liens_6[i,1]=='BCM 113') bd_liens_6[i,1]='BCM113'
  else if(bd_liens_6[i,1]=='ECL 516') bd_liens_6[i,1]='ECL516'
  else if(bd_liens_6[i,1]=='ECL 527') bd_liens_6[i,1]='ECL527'
  else if(bd_liens_6[i,1]=='ECL 510') bd_liens_6[i,1]='ECL510'
  else if(bd_liens_6[i,1]=='ECL 403') bd_liens_6[i,1]='ECL403'
  else if(bd_liens_6[i,1]=='ECL 406') bd_liens_6[i,1]='ECL406'
  else if(bd_liens_6[i,1]=='ECL 515') bd_liens_6[i,1]='ECL515'
  else if(bd_liens_6[i,1]=='ECL 535') bd_liens_6[i,1]='ECL535'
  else if(bd_liens_6[i,1]=='BOT 400') bd_liens_6[i,1]='BOT400'
  else if(bd_liens_6[i,1]=='ECL 603') bd_liens_6[i,1]='ECL603'
  else if(bd_liens_6[i,1]=='BIO 500') bd_liens_6[i,1]='BIO500'
}


#Modification accents
bd_liens_6<-data.frame(data.frame(t(iconv(t(bd_liens_6), to="ASCII//TRANSLIT"))))
bd_etudiants_6<-data.frame(data.frame(t(iconv(t(bd_etudiants_6), to="ASCII//TRANSLIT"))))

#Mettre tout en minuscule
bd_liens_6<-data.frame(data.frame(t(tolower(t(bd_liens_6)))))
bd_etudiants_6<-data.frame(data.frame(t(tolower(t(bd_etudiants_6)))))
bd_cours_6<-data.frame(data.frame(t(tolower(t(bd_cours_6)))))

#Liens dupliquer
liens_dup_6<-bd_liens_6[,c(1,3,2,4)]
names(liens_dup_6)[2]<-'etudiant1'
names(liens_dup_6)[3]<-'etudiant2'
bd_liens_6 <- rbind(bd_liens_6,liens_dup_6)

#changer en charactere
bd_liens_6[] <- lapply(bd_liens_6, as.character)

#corriger les noms
for (i in 1:324) {
  if (bd_liens_6[i,2]=='andreanne_deshormeaux') bd_liens_6[i,2]='andreanne_desormeaux'
  else if (bd_liens_6[i,2]=='cyrile_viens') bd_liens_6[i,2]='cyrille_viens'
  else if (bd_liens_6[i,2]=='josiane_cote-audet ') bd_liens_6[i,2]='josiane_cote-audet'
  else if (bd_liens_6[i,2]=='emile_choinard') bd_liens_6[i,2]='emile_chouinard'
  else if (bd_liens_6[i,2]==' elyse_paquette') bd_liens_6[i,2]='elyse_paquette'
  else if (bd_liens_6[i,2]==' etienne_fafard-couture') bd_liens_6[i,2]='etienne_fafard-couture'
  else if (bd_liens_6[i,2]==' laurie_lavergne') bd_liens_6[i,2]='laurie_lavergne'
} 
for (i in 1:324) {
  if (bd_liens_6[i,3]=='andreanne_deshormeaux') bd_liens_6[i,3]='andreanne_desormeaux'
  else if (bd_liens_6[i,3]=='cyrile_viens') bd_liens_6[i,3]='cyrille_viens'
  else if (bd_liens_6[i,3]=='josiane_cote-audet ') bd_liens_6[i,3]='josiane_cote-audet'
  else if (bd_liens_6[i,3]=='emile_choinard') bd_liens_6[i,3]='emile_chouinard'
  else if (bd_liens_6[i,3]==' elyse_paquette') bd_liens_6[i,3]='elyse_paquette'
  else if (bd_liens_6[i,3]==' etienne_fafard-couture') bd_liens_6[i,3]='etienne_fafard-couture'
  else if (bd_liens_6[i,3]==' laurie_lavergne') bd_liens_6[i,3]='laurie_lavergne'
} 

bd_cours_6[15,4]<-0
bd_cours_6[12,4]<-0
bd_cours_6[13,4]<-0

#>Equipe 7

bd_liens_7<-read.csv2('donnees_Liens_IB_CAD_EL_AR.csv',sep = ';',header = T)
bd_etudiants_7<-read.csv2('donnees_Étudiants_IB_CAD_EL_AR.csv',sep = ';',header = T)
bd_cours_7<-read.csv2('donnees_Cours_IB_CAD_EL_AR.csv',sep = ';',header = T)

#Nom et nombre des colonnes
colnames(bd_liens_7) = tolower(colnames(bd_liens_7))
colnames(bd_etudiants_7) = tolower(colnames(bd_etudiants_7))
colnames(bd_cours_7) = tolower(colnames(bd_cours_7))
names(bd_liens_7)[3]<-'etudiant1'
names(bd_liens_7)[4]<-'etudiant2'
names(bd_liens_7)[1]<-'sigle'
names(bd_liens_7)[2]<-'session'
bd_liens_7<-bd_liens_7[,c('sigle','etudiant1','etudiant2','session')]
bd_etudiants_7[,4]<-c('f','h','f','f')
names(bd_cours_7)[2]<-'pratique'
names(bd_cours_7)[1]<-'sigle'
names(bd_etudiants_7)[2]<-'prenom'

#Modification accents
bd_liens_7<-data.frame(data.frame(t(iconv(t(bd_liens_7), to="ASCII//TRANSLIT"))))
bd_etudiants_7<-data.frame(data.frame(t(iconv(t(bd_etudiants_7), to="ASCII//TRANSLIT"))))

#Mettre tout en minuscule
bd_liens_7<-data.frame(data.frame(t(tolower(t(bd_liens_7)))))
bd_etudiants_7<-data.frame(data.frame(t(tolower(t(bd_etudiants_7)))))
bd_cours_7<-data.frame(data.frame(t(tolower(t(bd_cours_7)))))

#Liens dupliquer
liens_dup_7<-bd_liens_7[,c(1,3,2,4)]
names(liens_dup_7)[2]<-'etudiant1'
names(liens_dup_7)[3]<-'etudiant2'
bd_liens_7 <- rbind(bd_liens_7,liens_dup_7)

#changer en charactere
bd_liens_7[] <- lapply(bd_liens_7, as.character)

#corriger les noms
for (i in 1:316) {
  for (j in 2:3) {
    if (bd_liens_7[i,j]=='maria_elisa_aparicio') bd_liens_7[i,j]='maria-elisa_aparicio-velasco'
    else if (bd_liens_7[i,j]=='francois_coursol_de_carufel') bd_liens_7[i,j]='francois_coursoldecarufel'
    else if (bd_liens_7[i,j]=='daphnee_dufour') bd_liens_7[i,j]='daphne_dufour'
  }
}

bd_cours_7[17,3]<-3
bd_cours_7[17,4]<-1
bd_cours_7[16,4]<-1
bd_cours_7[5,3]<-3
bd_cours_7[12,4]<-0
bd_cours_7[13,2]<-0
bd_cours_7[1,3]<-1

#assemblage donnees cours
bd_cours<-rbind(bd_cours_1,bd_cours_2,bd_cours_3,bd_cours_4,bd_cours_5,bd_cours_6,bd_cours_7)
bd_cours<-unique(bd_cours)

#assemblage donnees etudiants
bd_etudiants<-rbind(bd_etudiants_1,bd_etudiants_2,bd_etudiants_3,bd_etudiants_4,bd_etudiants_5,bd_etudiants_6,bd_etudiants_7)

#assemblage donnees liens
#mettre donnees ensemble
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
