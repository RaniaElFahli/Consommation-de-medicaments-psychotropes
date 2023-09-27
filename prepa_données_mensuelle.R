# chargement des packages 

library(tidyverse); library(dplyr) ; library(readxl)
library(stringr)

# en amont on transpose les données en ligne -> colonnes en ajoutant une colonne
# indiquant le sexe & le seuil de délivrance pour pas se perdre 
# je sépare les onglets par territoire pour faire par étapes et vérifier
# les chiffres au fur et à mesure, mais pas obligatoire 

##### EMS ####
  # préparation des données pour chaque territoire puis jointure 

conso_mensuelle <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                              sheet = "N05B_ems")
conso_mensuelle <- conso_mensuelle %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "B_1")


conso_mensuelle$temps <- as.factor(conso_mensuelle$temps)
conso_mensuelle$sexe <- as.factor(conso_mensuelle$sexe)

conso_mensuelle$territoire = "Eurométropole"

conso_mensuelle <- conso_mensuelle %>%
  rename("N05_B" = "B_1")

N05B_ems <- conso_mensuelle 

  #N05A
N05A_ems <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                   sheet = "N05A_ems" )

N05A_ems <- N05A_ems %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_A") 


N05A_ems$temps <- as.factor(N05A_ems$temps)
N05A_ems$sexe <- as.factor(N05A_ems$sexe)

N05A_ems$territoire = "Eurométropole"

  #N05C
N05C_ems <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                       sheet = "N05C_ems" )

N05C_ems <- N05C_ems %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_C")


N05C_ems$temps <- as.factor(N05C_ems$temps)
N05C_ems$sexe <- as.factor(N05C_ems$sexe)

N05C_ems$territoire = "Eurométropole"

  #N06A

N06A_ems <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                       sheet = "N06A_ems" )

N06A_ems <- N06A_ems %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N06A")


N06A_ems$temps <- as.factor(N06A_ems$temps)
N06A_ems$sexe <- as.factor(N06A_ems$sexe)

N06A_ems$territoire = "Eurométropole"

# on récupère que la colonne correspondant aux effectifs de chaque médicament 

N05A_ems <- N05A_ems %>%
  select(N05_A)

N05C_ems <- N05C_ems %>%
  select(N05_C)

N06A_ems <- N06A_ems %>%
  select(N06A)

# on les rajoute à la table "N05B_ems" qui devient la table total Eurométropole 

Eurométropole <- cbind(N05B_ems, N05A_ems, N05C_ems, N06A_ems)
Eurométropole$âge <- as.factor(Eurométropole$âge)
Eurométropole$sexe <- as.factor(Eurométropole$sexe)
Eurométropole$deliv_re <- as.factor(Eurométropole$deliv_re)



# on découpe la variable "temps" en une variable mois, une variable année 

Eurométropole <- Eurométropole %>%
  mutate(mois = str_sub(temps, -2, -1))

Eurométropole <- Eurométropole %>%
  mutate(année = str_sub(temps,1,4))

Eurométropole$mois <- as.factor(Eurométropole$mois)
Eurométropole$année<- as.factor(Eurométropole$année)


# ajout de la population des consommants 

pop_consommants <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                       sheet = "pop_conso" )
pop_consommants <- pop_consommants %>%
  mutate_if(is.character, as.factor)
pop_consommants$année <- as.character(pop_consommants$année) # rappel : lorsqu'une variable est numérique, on passe d'abord par character pour la transformer ensuite en factor. 
pop_consommants$année <- as.factor(pop_consommants$année)

pop_conso_ems <- pop_consommants %>%
  dplyr::filter(territoire == "Eurométropole") %>%
  droplevels() %>%
  select(-c(territoire)) # on enlève ensuite la variable territoire pour ne pas faire doublon avec celle déjà présente dans la table Eurométropole 

Eurométropole <- Eurométropole %>%
  left_join(pop_conso_ems, by = c("année", "âge", "sexe")) # il faut joindre à la fois par année, âge et sexe pour que les effectifs de la population se répètent pour chaque 
                                                          # ligne correspondant par exemple aux hommes âgées de 0 à 29 ans pour l'année 2018 


# on peut désormais calculés des taux bruts p.100 000hab rapportés sur la population des consommants 

Eurométropole <- Eurométropole %>%
  mutate(tx_brut_N05A = (N05_A/pop_conso)*1000,
         tx_brut_N05B = (N05_B/pop_conso)*1000,
         tx_brut_N05C = (N05_C/pop_conso)*1000,
         tx_brut_N06A = (N06A/pop_conso)*1000)

##### Grand Est #### 
# on récupère le script au dessus, on remplace "ems" par "ge" 

#N05B

N05B_ge <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                       sheet = "N05B_ge" )

N05B_ge <- N05B_ge %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_B") 


N05B_ge$temps <- as.factor(N05B_ge$temps)
N05B_ge$sexe <- as.factor(N05B_ge$sexe)

N05B_ge$territoire = "Grand-Est"

#N05A
N05A_ge <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                       sheet = "N05A_ge" )

N05A_ge <- N05A_ge %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_A") 


N05A_ge$temps <- as.factor(N05A_ge$temps)
N05A_ge$sexe <- as.factor(N05A_ge$sexe)

N05A_ge$territoire = "Grand-Est"

#N05C
N05C_ge <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                       sheet = "N05C_ge" )

N05C_ge <- N05C_ge %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_C")


N05C_ge$temps <- as.factor(N05C_ge$temps)
N05C_ge$sexe <- as.factor(N05C_ge$sexe)

N05C_ge$territoire = "Grand-Est"

#N06A

N06A_ge <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                       sheet = "N06A_ge" )

N06A_ge <- N06A_ge %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N06A")


N06A_ge$temps <- as.factor(N06A_ge$temps)
N06A_ge$sexe <- as.factor(N06A_ge$sexe)

N06A_ge$territoire = "Grand-Est"

# on récupère que la colonne correspondant aux effectifs de chaque médicament 

N05A_ge <- N05A_ge %>%
  select(N05_A)

N05C_ge <- N05C_ge %>%
  select(N05_C)

N06A_ge <- N06A_ge %>%
  select(N06A)

Grand_Est <- cbind(N05B_ge, N05A_ge, N05C_ge, N06A_ge)

# on crée une base finale contenant tous les territoires pour pouvoir les comparer 

Grand_Est$âge <- as.factor(Grand_Est$âge)
Grand_Est$sexe <- as.factor(Grand_Est$sexe)
Grand_Est$deliv_re <- as.factor(Grand_Est$deliv_re)



# on découpe la variable "temps" en une variable mois, une variable année 

Grand_Est <- Grand_Est %>%
  mutate(mois = str_sub(temps, -2, -1))

Grand_Est <- Grand_Est %>%
  mutate(année = str_sub(temps,1,4))

Grand_Est$mois <- as.factor(Grand_Est$mois)
Grand_Est$année<- as.factor(Grand_Est$année)


# ajout de la population des consommants 

pop_consommants <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                              sheet = "pop_conso" )
pop_consommants <- pop_consommants %>%
  mutate_if(is.character, as.factor)
pop_consommants$année <- as.character(pop_consommants$année) # rappel : lorsqu'une variable est numérique, on passe d'abord par character pour la transformer ensuite en factor. 
pop_consommants$année <- as.factor(pop_consommants$année)

pop_conso_ge <- pop_consommants %>%
  dplyr::filter(territoire == "Grand-Est") %>%
  droplevels() %>%
  select(-c(territoire)) # on enlève ensuite la variable territoire pour ne pas faire doublon avec celle déjà présente dans la table Grand-Est 

Grand_Est <- Grand_Est %>%
  left_join(pop_conso_ge, by = c("année", "âge", "sexe")) 

# on peut désormais calculés des taux bruts p.100 000hab rapportés sur la population des consommants 

Grand_Est <- Grand_Est %>%
  mutate(tx_brut_N05A = (N05_A/pop_conso)*1000,
         tx_brut_N05B = (N05_B/pop_conso)*1000,
         tx_brut_N05C = (N05_C/pop_conso)*1000,
         tx_brut_N06A = (N06A/pop_conso)*1000)

#### Bas-Rhin ####
# on récupère le script au dessus, on remplace "_ge" par "_br" et "Grand-Est" par "Bas-Rhin", "Grand_Est" par "Bas_Rhin" et on exécute 


#N05B

N05B_br <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                      sheet = "N05B_br" )

N05B_br <- N05B_br %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_B") 


N05B_br$temps <- as.factor(N05B_br$temps)
N05B_br$sexe <- as.factor(N05B_br$sexe)

N05B_br$territoire = "Bas-Rhin"

#N05A
N05A_br <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                      sheet = "N05A_br" )

N05A_br <- N05A_br %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_A") 


N05A_br$temps <- as.factor(N05A_br$temps)
N05A_br$sexe <- as.factor(N05A_br$sexe)

N05A_br$territoire = "Bas-Rhin"

#N05C
N05C_br <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                      sheet = "N05C_br" )

N05C_br <- N05C_br %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_C")


N05C_br$temps <- as.factor(N05C_br$temps)
N05C_br$sexe <- as.factor(N05C_br$sexe)

N05C_br$territoire = "Bas-Rhin"

#N06A

N06A_br <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                      sheet = "N06A_br" )

N06A_br <- N06A_br %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N06A")


N06A_br$temps <- as.factor(N06A_br$temps)
N06A_br$sexe <- as.factor(N06A_br$sexe)

N06A_br$territoire = "Bas-Rhin"

# on récupère que la colonne correspondant aux effectifs de chaque médicament 

N05A_br <- N05A_br %>%
  select(N05_A)

N05C_br <- N05C_br %>%
  select(N05_C)

N06A_br <- N06A_br %>%
  select(N06A)

Bas_Rhin <- cbind(N05B_br, N05A_br, N05C_br, N06A_br)

# on crée une base finale contenant tous les territoires pour pouvoir les comparer 

Bas_Rhin$âge <- as.factor(Bas_Rhin$âge)
Bas_Rhin$sexe <- as.factor(Bas_Rhin$sexe)
Bas_Rhin$deliv_re <- as.factor(Bas_Rhin$deliv_re)



# on découpe la variable "temps" en une variable mois, une variable année 

Bas_Rhin <- Bas_Rhin %>%
  mutate(mois = str_sub(temps, -2, -1))

Bas_Rhin <- Bas_Rhin %>%
  mutate(année = str_sub(temps,1,4))

Bas_Rhin$mois <- as.factor(Bas_Rhin$mois)
Bas_Rhin$année<- as.factor(Bas_Rhin$année)


# ajout de la population des consommants 

pop_consommants <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                              sheet = "pop_conso" )
pop_consommants <- pop_consommants %>%
  mutate_if(is.character, as.factor)
pop_consommants$année <- as.character(pop_consommants$année) # rappel : lorsqu'une variable est numérique, on passe d'abord par character pour la transformer ensuite en factor. 
pop_consommants$année <- as.factor(pop_consommants$année)

pop_conso_br <- pop_consommants %>%
  dplyr::filter(territoire == "Bas-Rhin") %>%
  droplevels() %>%
  select(-c(territoire)) # on enlève ensuite la variable territoire pour ne pas faire doublon avec celle déjà présente dans la table Bas-Rhin 

Bas_Rhin <- Bas_Rhin %>%
  left_join(pop_conso_br, by = c("année", "âge", "sexe")) 

# on peut désormais calculés des taux bruts p.100 000hab rapportés sur la population des consommants 

Bas_Rhin <- Bas_Rhin %>%
  mutate(tx_brut_N05A = (N05_A/pop_conso)*1000,
         tx_brut_N05B = (N05_B/pop_conso)*1000,
         tx_brut_N05C = (N05_C/pop_conso)*1000,
         tx_brut_N06A = (N06A/pop_conso)*1000)

##### France métropolitaine ####
  # même procédé, en remplaçant _br par _fm, "Bas_Rhin" par "France_métro" et "Bas-Rhin" par "France" (= level de la variable territoire pour l'ajout de la pop conso)

#N05B

N05B_fm <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                      sheet = "N05B_fm" )

N05B_fm <- N05B_fm %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_B") 


N05B_fm$temps <- as.factor(N05B_fm$temps)
N05B_fm$sexe <- as.factor(N05B_fm$sexe)

N05B_fm$territoire = "France"

#N05A
N05A_fm <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                      sheet = "N05A_fm" )

N05A_fm <- N05A_fm %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_A") 


N05A_fm$temps <- as.factor(N05A_fm$temps)
N05A_fm$sexe <- as.factor(N05A_fm$sexe)

N05A_fm$territoire = "France"

#N05C
N05C_fm <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                      sheet = "N05C_fm" )

N05C_fm <- N05C_fm %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N05_C")


N05C_fm$temps <- as.factor(N05C_fm$temps)
N05C_fm$sexe <- as.factor(N05C_fm$sexe)

N05C_fm$territoire = "France"

#N06A

N06A_fm <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                      sheet = "N06A_fm" )

N06A_fm <- N06A_fm %>%
  pivot_longer(!c(temps, deliv_re, sexe), names_to = "âge", values_to = "N06A")


N06A_fm$temps <- as.factor(N06A_fm$temps)
N06A_fm$sexe <- as.factor(N06A_fm$sexe)

N06A_fm$territoire = "France"

# on récupère que la colonne correspondant aux effectifs de chaque médicament 

N05A_fm <- N05A_fm %>%
  select(N05_A)

N05C_fm <- N05C_fm %>%
  select(N05_C)

N06A_fm <- N06A_fm %>%
  select(N06A)

France_metro <- cbind(N05B_fm, N05A_fm, N05C_fm, N06A_fm)

# on crée une base finale contenant tous les territoires pour pouvoir les comparer 

France_metro$âge <- as.factor(France_metro$âge)
France_metro$sexe <- as.factor(France_metro$sexe)
France_metro$deliv_re <- as.factor(France_metro$deliv_re)



# on découpe la variable "temps" en une variable mois, une variable année 

France_metro <- France_metro %>%
  mutate(mois = str_sub(temps, -2, -1))

France_metro <- France_metro %>%
  mutate(année = str_sub(temps,1,4))

France_metro$mois <- as.factor(France_metro$mois)
France_metro$année<- as.factor(France_metro$année)


# ajout de la population des consommants 

pop_consommants <- read_excel("~/Conso médicaments/R project & scripts/conso_mensuelle.xlsx", 
                              sheet = "pop_conso" )
pop_consommants <- pop_consommants %>%
  mutate_if(is.character, as.factor)
pop_consommants$année <- as.character(pop_consommants$année) # rappel : lorsqu'une variable est numérique, on passe d'abord par character pour la transformer ensuite en factor. 
pop_consommants$année <- as.factor(pop_consommants$année)

pop_conso_fm <- pop_consommants %>%
  dplyr::filter(territoire == "France") %>%
  droplevels() %>%
  select(-c(territoire)) # on enlève ensuite la variable territoire pour ne pas faire doublon avec celle déjà présente dans la table France 

France_metro <- France_metro %>%
  left_join(pop_conso_fm, by = c("année", "âge", "sexe")) 

# on peut désormais calculés des taux bruts p.100 000hab rapportés sur la population des consommants 

France_metro <- France_metro %>%
  mutate(tx_brut_N05A = (N05_A/pop_conso)*1000,
         tx_brut_N05B = (N05_B/pop_conso)*1000,
         tx_brut_N05C = (N05_C/pop_conso)*1000,
         tx_brut_N06A = (N06A/pop_conso)*1000)


#### Base finale FM - GE - BR - EMS ####

# les tables de chaque territoire sont structurées de la même manière (1728 lignes et 16 variables) et les variables sont dans le même ordre pour chaque table
# on peut simplement rassembler toutes les tables par lignes (rbind), la variable "territoire" créée au fur et à mesure sur chacune des tables permettra de savoir à quel territoire correspondent les lignes

Base_totale <- rbind(Eurométropole, Grand_Est, Bas_Rhin, France_metro)

Base_totale$territoire <- Base_totale$territoire %>%
  fct_recode(
    "Grand Est" = "Grand-Est", 
    "Eurométropole de Strasbourg" = "Eurométropole"
  )

pop_consommants$territoire <- pop_consommants$territoire %>%
  fct_recode(
    "Grand Est" = "Grand-Est", 
    "Eurométropole de Strasbourg" = "Eurométropole"
  )
# soit une base totale de 6912 lignes et 16 variables :) 

writexl::write_xlsx(Base_totale, path = "base_totale_mensuel_cor.xlsx")


##### + Infra-EMS ####