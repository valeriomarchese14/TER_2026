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
