# main.wd<-getwd()
library(chron) # per le date
#install.packages("glmnet")
#install.packages("epitools")
library(descrittive)
source("Descrittive.ok.r")
options(max.print=1000000)

############################################################
# 
# dati<-read.csv2("Italia-Tunisia.csv",fileEncoding="latin1")
# head(dati)
# dati<-read.csv2("Italia-Tunisia.csv",fileEncoding="iso-8859-1")
dati<-read.csv2("Italia-Tunisia_sistemato.csv",fileEncoding="UTF-8",stringsAsFactors = TRUE)
summary(dati)
table(dati$ANNEE_etude,dati$pays,exclude = NULL)
View(dati)
library(dplyr)
head(dati)
glimpse(dati)
sum(duplicated(dati))
dati %>%
  count(profession)
is.na(dati)
sum(is.na(dati))
library(visdat)
vis_miss(dati, cluster = TRUE)
#distribuzione genere
tab_gnr <- dati %>% group_by(genre, pays) %>%
  summarize(Freq = n()) %>% mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))
tab_gnr
#visualizzazione distribuzione genere
library(ggplot2)
library(tidyverse)
library(scales)
perc.genre <- round(100*prop.table(table(dati$pays, dati$genre),1),0)
perc.genre
barplot(perc.fumeur, beside =  TRUE, legend.text = TRUE, col = c("green", "red"))

dati.miei<-data.frame(pays=rep(c("italie","tunisie"),each=2))
dati.miei$genre<-rep(c("Femme","Homme"),2)
dati.miei$pourcentages <-c(48,52,53,47)
dati.miei

ggplot(dati.miei, aes(x=genre, y=pourcentages, fill=pays)) +  
  geom_bar(position="dodge", stat="identity", color="black")+
  geom_text(aes(x=genre, y=pourcentages, label = percent(pourcentages/100), vjust=-0.5), position = position_dodge(width=0.9))+
  labs(y = "pourcentages", x = "genre", title = "p.value = 0.074")
descrittive(pays~genre,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)

#distribuzione genere per età

ggplot(dati, 
       aes(x = pays, y = Age, 
           fill = genre)) + 
  geom_boxplot() + labs(title = "age p.value <0.001")
descrittive(pays~Age,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)
#distribuzione età per paese
ggplot(dati, 
       aes(x = pays, y = Age, 
           fill = pays)) + 
  geom_boxplot()+ labs(title = "p.value <0.001")

#distribuzione scolarità per paese
dati$annee_etude<-as.numeric(as.character(dati$ANNEE_etude))
table(dati$annee_etude,dati$pays,exclude = NULL)
ggplot(dati, 
       aes(x = pays, y = annee_etude)) + 
  geom_bar(position="dodge", stat="identity", color="red")+ labs(title = "p.value <0.001")
descrittive(pays~annee_etude,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)

#distribuzione per luogo abitativo
paese.abitazione <- round(100*prop.table(table(dati$pays, dati$lieu_residence),1),0)
barplot(paese.abitazione, beside =  TRUE, legend.text = TRUE, col = c("green", "red"), main = "p.value <0.001")
descrittive(pays~lieu_residence,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)
#fumatori
perc.fumeur <- round(100*prop.table(table(dati$pays, dati$fumeur),1),0)
perc.fumeur
barplot(perc.fumeur, beside =  TRUE, legend.text = TRUE, col = c("green", "red"))

dati.miei<-data.frame(pays=rep(c("italie","tunisie"),each=3))
dati.miei$fumo<-rep(c("oui actuel","non","ancien fumeur"),2)
dati.miei$percentuali <-c(18, 55, 27, 12, 53, 35)
dati.miei

ggplot(dati.miei, aes(x=fumo, y=percentuali, fill=pays)) +  
  geom_bar(position="dodge", stat="identity", color="black")+
  geom_text(aes(x=fumo, y=percentuali, label = percent(percentuali/100), vjust=-1), position = position_dodge(width=0.9))+
  labs(y = "pourcentages", x = "fumeur", title = "p.value = 0.001")
descrittive(pays~fumeur,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)

library(compareGroups)
res1<- compareGroups(pays~atcd_arthrite+atcd_cardiopathie+atcd_hta+atcd_bpco+atcd_asthme+atcd_diabete, data=dati)
res1c <- createTable(res1)

descrittive(pays~atcd_arthrite+atcd_cardiopathie+atcd_hta+atcd_bpco+atcd_asthme+atcd_diabete, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)

res2<- compareGroups(pays~risque_deces+ris_brulure_gravite_variable+risk_difficulte_resp+risk_traumatisme+risk_blessure_grav_variable, data=dati)
res2c <- createTable(res2)
descrittive(pays~risque_deces+ris_brulure_gravite_variable+risk_difficulte_resp+risk_traumatisme+risk_blessure_grav_variable, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)

res3<- compareGroups(pays~equipement_protection+equipe_medicale+equipe_paramedicale+borne_a_incendie+protection_civile, data=dati)
res3c <- createTable(res3)
descrittive(pays~equipement_protection+equipe_medicale+equipe_paramedicale+borne_a_incendie+protection_civile, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)



perc.strategie <- round(100*prop.table(table(dati$pays, dati$stratégie_gestion_niveau_regional),1),0)
perc.strategie
dati.miei1<-data.frame(pays=rep(c("italie","tunisie"),each=3))
dati.miei1$strategie<-rep(c("Ne sais pas","Non","oui"),2)
dati.miei1$percentuali <-c(0, 16, 84, 21, 30, 49)
dati.miei1
ggplot(dati.miei1, aes(x=strategie, y=percentuali, fill=pays)) +  
  geom_bar(position="dodge", stat="identity", color="black")+
  geom_text(aes(x=strategie, y=percentuali, label = percent(percentuali/100), vjust=-1), position = position_dodge(width=0.9))+
  labs(y = "pourcentages", x = "perception de la stratégie",title = "p.value <0.001")
descrittive(pays~stratégie_gestion_niveau_regional,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)

res4<- compareGroups(pays~pompier+militaire+cellule_crise+ministere_sante, data=dati)
res4c <- createTable(res4)
descrittive(pays~pompier+militaire+cellule_crise+ministere_sante, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)

res5<- compareGroups(pays~factor(tous_hopitaux_impliques)+hopital_benarous_gene_implique+urgences_impliques+autre_etablissement_impliques, data=dati)
res5c <- createTable(res5)
descrittive(pays~factor(tous_hopitaux_impliques)+factor(hopital_benarous_gene_implique)+factor(urgences_impliques)+factor(autre_etablissement_impliques), data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)

res6<- compareGroups(pays~perception_Accidents_circulation+perception_aliments_nocifs+perception_medic_psychoactifs+perception_pollution_environnementale+perception_pauvrete
                      +perception_desastres_naturel+perception_terrorisme+perception_chomage+perception_principales_maladies_epidemies+perception_risque_nucleaire+perception_harcelement+perception_criminalite+perception_marginalisation+perception_telephone_portable, data=dati)
res6c <- createTable(res6)
descrittive(pays~perception_Accidents_circulation+perception_aliments_nocifs+perception_medic_psychoactifs+perception_pollution_environnementale+perception_pauvrete
            +perception_desastres_naturel+perception_terrorisme+perception_chomage+perception_principales_maladies_epidemies+perception_risque_nucleaire+perception_harcelement+perception_criminalite+perception_marginalisation+perception_telephone_portable, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)

res7<- compareGroups(pays~danger_envir_inondation_glissement+danger_envir_bruit+danger_envir_odeurs+danger_envir_transport_mat_dangereuse
                    +danger_envir_dechets_decharge+danger_envir_dechets_dangereux+danger_envir_pollution_air+danger_envir_phen_metereologique+danger_envir_incendies+danger_envir_pollut_eau+danger_envir_industrie_danger
                    +danger_envir_tremblement_terre+danger_envir_pollution_alimentaire+danger_envir_ligne_electrique+danger_envir_telephone_portable+danger_envir_radon, data=dati)
res7c <- createTable(res7)
descrittive(pays~danger_envir_inondation_glissement+danger_envir_bruit+danger_envir_odeurs+danger_envir_transport_mat_dangereuse
            +danger_envir_dechets_decharge+danger_envir_dechets_dangereux+danger_envir_pollution_air+danger_envir_phen_metereologique+danger_envir_incendies+danger_envir_pollut_eau+danger_envir_industrie_danger
            +danger_envir_tremblement_terre+danger_envir_pollution_alimentaire+danger_envir_ligne_electrique+danger_envir_telephone_portable+danger_envir_radon, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)

perc.strategie <- round(100*prop.table(table(dati$pays, dati$vous_sentez_vous_informe),1),0)
perc.strategie
dati.miei2<-data.frame(pays=rep(c("italie","tunisie"),each=2))
dati.miei2$pericoli<-rep(c("non","oui"),2)
dati.miei2$percentuali <-c(28, 72, 70, 30)
dati.miei2
ggplot(dati.miei2, aes(x=pericoli, y=percentuali, fill=pays)) +  
  geom_bar(position="dodge", stat="identity", color="black")+
  geom_text(aes(x=pericoli, y=percentuali, label = percent(percentuali/100), vjust=-1), position = position_dodge(width=0.9))+
  labs(y = "pourcentages", x = "vous sentez-vous informé des dangers ?", title = "p.value <0.001")
descrittive(pays~vous_sentez_vous_informe,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)

perc.strategie <- round(100*prop.table(table(dati$pays, dati$situation_environ_communaute),1),0)
perc.strategie
dati.miei3<-data.frame(pays=rep(c("italie","tunisie"),each=5))
dati.miei3$sit.ambientale<-rep(c("Acceptable", "Grave et irrévirsible", "je ne sais pas", "Mauvaise mais améliorable", "Tres bonne"),2)
dati.miei3$percentuali <-c(15, 31, 7, 35, 12, 36, 4, 0, 58, 2)
dati.miei3
ggplot(dati.miei3, aes(x=sit.ambientale, y=percentuali, fill=pays)) +  
  geom_bar(position="dodge", stat="identity", color="black")+
  geom_text(aes(x=sit.ambientale, y=percentuali, label = percent(percentuali/100), vjust=-1), position = position_dodge(width=0.9))+
  labs(y = "pourcentages", x = "perception de la situation environnementale", title = "p.value <0.001")
descrittive(pays~situation_environ_communaute,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)

perc.strategie <- round(100*prop.table(table(dati$pays, dati$quittez_zone),1),0)
perc.strategie
dati.miei4<-data.frame(pays=rep(c("italie","tunisie"),each=2))
dati.miei4$quit.zone<-rep(c("non","oui"),2)
dati.miei4$percentuali <-c(50, 50, 54, 46)
dati.miei4
ggplot(dati.miei4, aes(x=quit.zone, y=percentuali, fill=pays)) +  
  geom_bar(position="dodge", stat="identity", color="black")+
  geom_text(aes(x=quit.zone, y=percentuali, label = percent(percentuali/100), vjust=-1), position = position_dodge(width=0.9))+
  labs(y = "pourcentages", x = "quitteriez-vous votre zone ?",title = "p.value = 0.127")
descrittive(pays~quittez_zone,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)


res8<- compareGroups(pays~cause_quitter_proche_lieu_travail+cause_quitter_famille+cause_quitter_region_origine+cause_quitter_centre_plus_grand
                      +cause_quitter_zone_plus_saine+cause_quitter_meilleurs_services+cause_quitter_securite, data=dati)
res8c <- createTable(res8)
descrittive(pays~cause_quitter_proche_lieu_travail+cause_quitter_famille+cause_quitter_region_origine+cause_quitter_centre_plus_grand
            +cause_quitter_zone_plus_saine+cause_quitter_meilleurs_services+cause_quitter_securite, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)

perc.strategie <- round(100*prop.table(table(dati$pays, dati$entendu_parler_pb_sante_region),1),0)
perc.strategie
dati.miei5<-data.frame(pays=rep(c("italie","tunisie"),each=2))
dati.miei5$prob.amb<-rep(c("non","oui"),2)
dati.miei5$percentuali <-c(3, 97, 36, 64)
dati.miei5
ggplot(dati.miei5, aes(x=prob.amb, y=percentuali, fill=pays)) +  
  geom_bar(position="dodge", stat="identity", color="black")+
  geom_text(aes(x=prob.amb, y=percentuali, label = percent(percentuali/100), vjust=-1), position = position_dodge(width=0.9))+
  labs(y = "pourcentages", x = "Avez-vous entendu parler de problèmes de santé existant dans votre région?",  title = "p.value <0.001")
descrittive(pays~entendu_parler_pb_sante_region,data=dati,methodFac="fisher",methodNum="kruskal",digits=0,digits.pval=3,copy=FALSE)

res9<- compareGroups(pays~entendu_famille_collegue+entendu_association+entendu_etude_scientifique+entendu_journaux
                      +entendu_institut_public, data=dati)
res9c <- createTable(res9)
descrittive(pays~entendu_famille_collegue+entendu_association+entendu_etude_scientifique+entendu_journaux
            +entendu_institut_public, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)

res10<- compareGroups(pays~responsable_instit_local+responsable_citoyen+responsable_service_sante_local+responsable_gestion_industrie+responsable_instit_national, data=dati)
res10c <- createTable(res10)
descrittive(pays~responsable_instit_local+responsable_citoyen+responsable_service_sante_local+responsable_gestion_industrie+responsable_instit_national, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)

res11<- compareGroups(pays~prob_malade_allergie+prob_malade_mal_respir_aigue+prob_malade_resp_chronique+prob_malade_mdie_cardiovasculaire+prob_malade_infertilite+prob_malade_cancer+prob_malade_leucemie+prob_malade_malformatio_congen, data=dati)
res11c <- createTable(res11)
descrittive(pays~prob_malade_allergie+prob_malade_mal_respir_aigue+prob_malade_resp_chronique+prob_malade_mdie_cardiovasculaire+prob_malade_infertilite+prob_malade_cancer+prob_malade_leucemie+prob_malade_malformatio_congen, data=dati,methodFac="fisher",
            methodNum="kruskal",digits=2,digits.pval=3,copy=TRUE)
library(knitr)
library(kableExtra)

