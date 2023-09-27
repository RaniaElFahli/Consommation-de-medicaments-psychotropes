library(its2es)
###### Femmes ####
femmes_3039 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "femmes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "30-39 ans")

femmes_3039$temps_obs <- as.numeric(femmes_3039$temps_obs)
femmes_3039 <- femmes_3039 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

femmes_3039$mois <- as.numeric(femmes_3039$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(femmes_3039$année == "2020" & femmes_3039$mois == "3" | femmes_3039$année == "2021")[1]
fit <- its_poisson(data=femmes_3039,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)


femmes_0_29 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "femmes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "0-29 ans")

femmes_0_29$temps_obs <- as.numeric(femmes_0_29$temps_obs)
femmes_0_29 <- femmes_0_29 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

femmes_0_29$mois <- as.numeric(femmes_0_29$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(femmes_0_29$année == "2020" & femmes_0_29$mois == "3" | femmes_0_29$année == "2021")[1]

fit <- its_poisson(data=femmes_0_29,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)

femmes_4049 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "femmes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "40-49 ans")

femmes_4049$temps_obs <- as.numeric(femmes_4049$temps_obs)
femmes_4049 <- femmes_4049 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

femmes_4049$mois <- as.numeric(femmes_4049$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(femmes_4049$année == "2020" & femmes_4049$mois == "3" | femmes_4049$année == "2021")[1]
fit <- its_poisson(data=femmes_4049,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)


femmes_5059 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "femmes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "50-59 ans")

femmes_5059$temps_obs <- as.numeric(femmes_5059$temps_obs)
femmes_5059 <- femmes_5059 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

femmes_5059$mois <- as.numeric(femmes_5059$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(femmes_5059$année == "2020" & femmes_5059$mois == "3" | femmes_5059$année == "2021")[1]
fit <- its_poisson(data=femmes_5059,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)


femmes_6069 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "femmes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "60-69 ans")

femmes_6069$temps_obs <- as.numeric(femmes_6069$temps_obs)
femmes_6069 <- femmes_6069 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

femmes_6069$mois <- as.numeric(femmes_6069$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(femmes_6069$année == "2020" & femmes_6069$mois == "3" | femmes_6069$année == "2021")[1]
fit <- its_poisson(data=femmes_6069,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)



femmes_70 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "femmes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "70 ans ou +")

femmes_70$temps_obs <- as.numeric(femmes_70$temps_obs)
femmes_70 <- femmes_70 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

femmes_70$mois <- as.numeric(femmes_70$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(femmes_70$année == "2020" & femmes_70$mois == "3" | femmes_70$année == "2021")[1]
fit <- its_poisson(data=femmes_70,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)



##### hommes ####

hommes_3039 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "hommes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "30-39 ans")

hommes_3039$temps_obs <- as.numeric(hommes_3039$temps_obs)
hommes_3039 <- hommes_3039 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

hommes_3039$mois <- as.numeric(hommes_3039$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(hommes_3039$année == "2020" & hommes_3039$mois == "3" | hommes_3039$année == "2021")[1]
fit <- its_poisson(data=hommes_3039,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)


hommes_0_29 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "hommes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "0-29 ans")

hommes_0_29$temps_obs <- as.numeric(hommes_0_29$temps_obs)
hommes_0_29 <- hommes_0_29 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

hommes_0_29$mois <- as.numeric(hommes_0_29$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(hommes_0_29$année == "2020" & hommes_0_29$mois == "3" | hommes_0_29$année == "2021")[1]

fit <- its_poisson(data=hommes_0_29,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)

hommes_4049 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "hommes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "40-49 ans")

hommes_4049$temps_obs <- as.numeric(hommes_4049$temps_obs)
hommes_4049 <- hommes_4049 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

hommes_4049$mois <- as.numeric(hommes_4049$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(hommes_4049$année == "2020" & hommes_4049$mois == "3" | hommes_4049$année == "2021")[1]
fit <- its_poisson(data=hommes_4049,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)


hommes_5059 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "hommes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "50-59 ans")

hommes_5059$temps_obs <- as.numeric(hommes_5059$temps_obs)
hommes_5059 <- hommes_5059 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

hommes_5059$mois <- as.numeric(hommes_5059$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(hommes_5059$année == "2020" & hommes_5059$mois == "3" | hommes_5059$année == "2021")[1]
fit <- its_poisson(data=hommes_5059,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)


hommes_6069 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "hommes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "60-69 ans")

hommes_6069$temps_obs <- as.numeric(hommes_6069$temps_obs)
hommes_6069 <- hommes_6069 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

hommes_6069$mois <- as.numeric(hommes_6069$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(hommes_6069$année == "2020" & hommes_6069$mois == "3" | hommes_6069$année == "2021")[1]
fit <- its_poisson(data=hommes_6069,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)



hommes_70 <- data_std_ind_menseul_tsp %>%
  dplyr::filter(territoire == "Eurométropole de Strasbourg") %>%
  droplevels()%>%
  dplyr::filter(sexe == "hommes" ) %>%
  droplevels() %>%
  dplyr::filter(âge == "70 ans ou +")

hommes_70$temps_obs <- as.numeric(hommes_70$temps_obs)
hommes_70 <- hommes_70 %>%
  mutate(covid = case_when(
    temps_obs < 27 ~ "0", 
    temps_obs >= 27 ~ "1"
  ))

hommes_70$mois <- as.numeric(hommes_70$mois)

N05A<- as.formula("N05A ~ temps_obs")
start_interr <- which(hommes_70$année == "2020" & hommes_70$mois == "3" | hommes_70$année == "2021")[1]
fit <- its_poisson(data=hommes_70,form=N05A,offset_name="pop_conso", 
                   time_name = "temps_obs",intervention_start_ind=start_interr,over_dispersion=TRUE, 
                   freq=12, seasonality= "full", impact_model = "full",counterfactual = TRUE)


