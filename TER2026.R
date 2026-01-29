library(mice)
library(dplyr)
library(tidyr)


data<-read.csv2("BDD_2026.csv", fileEncoding = "latin1")
summary(data)
str(data)

# conversion des colonnes de coûts chr -> num
data$cout.baseline=as.numeric(data$cout.baseline)
data$cout.aprés=as.numeric(data$cout.aprés)

data[data==""]<-NA
data[data==" "]<-NA

data <- data %>%
  mutate(cout.baseline = ifelse(visite != "Baseline", 0, cout.baseline),
         cout.aprés = ifelse(visite == "Baseline", 0, cout.aprés))

pred_matrice <- data %>% select(-Identifiant.patient)
imputation<- mice(pred_matrice, m = 1, method = 'pmm', seed = 123, print = FALSE)
df_complet <- complete(imputation)

# on remet l'identifiant du patient
df_complet$Identifiant.patient <- data$Identifiant.patient


colSums(is.na(df_complet))

#-------calcul de l'utilité-----


calculer_utilite <- function(mob, soin, act, doul, anx) {
  score_brut <- (mob + soin + act + doul + anx - 5) 
  utilite <- 1 - (score_brut * 0.05) 
  return(utilite)
}

df_complet <- df_complet %>%
  mutate(
    utilite = calculer_utilite(EQ5D.mobilité, EQ5D.soins, EQ5D.activites, 
                               EQ5D.douleur, EQ5D.anxiete)
  )

#-----calcul des QALY-----
df_final <- df_complet %>% 
  arrange(Identifiant.patient, mois) %>%
  group_by(Identifiant.patient) %>%
  mutate(
    delta_temps = (mois - lag(mois, default = 0)) / 12,
    utilite_moyenne = (utilite + lag(utilite, default = first(utilite))) / 2,
    qaly_periode = delta_temps * utilite_moyenne
  ) %>%
  summarise(
    bras = first(bras),
    cout_total = sum(cout.baseline + cout.aprés, na.rm = TRUE),
    qaly_total = sum(qaly_periode, na.rm = TRUE)
  )
aggregate(cbind(cout_total, qaly_total) ~ bras, data = df_final, mean) #comparaison rapide des deux groupes
