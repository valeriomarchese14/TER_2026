library(dplyr)
library(tidyr)
library(mice)

#----nettoyage-----
df <- read.csv("BDD_2026.csv", sep = ";", fileEncoding = "latin1", stringsAsFactors = TRUE)

# Gestion des vides
df[df == ""] <- NA

# Gestion des zéros structurels
df <- df %>%
  mutate(
    cout.baseline = ifelse(visite != "Baseline", 0, cout.baseline),
    cout.aprés = ifelse(visite == "Baseline", 0, cout.aprés)
  )

#----imputation----
pred_matrix <- df %>% select(-Identifiant.patient)
imputed_obj <- mice(pred_matrix, m = 1, method = 'pmm', seed = 123, print = FALSE)
df_complet <- complete(imputed_obj)
df_complet$Identifiant.patient <- df$Identifiant.patient

#----calcul utilité-----

df_complet <- df_complet %>%
  mutate(
    # --- MOBILITÉ ---
    c_mob = case_when(
      EQ5D.mobilité == 1 ~ 0,
      EQ5D.mobilité == 2 ~ 0.03759,  
      EQ5D.mobilité == 3 ~ 0.04774,  
      EQ5D.mobilité == 4 ~ 0.17949,  
      EQ5D.mobilité == 5 ~ 0.32509   
    ),
    
    # --- AUTONOMIE ---
    c_soins = case_when(
      EQ5D.soins == 1 ~ 0,
      EQ5D.soins == 2 ~ 0.03656,
      EQ5D.soins == 3 ~ 0.050781,
      EQ5D.soins == 4 ~ 0.172251,
      EQ5D.soins == 5 ~ 0.258331
    ),
    
    # --- ACTIVITÉS ---
    c_act = case_when(
      EQ5D.activites == 1 ~ 0,
      EQ5D.activites == 2 ~ 0.03313,
      EQ5D.activites == 3 ~ 0.03979,
      EQ5D.activites == 4 ~ 0.15689,
      EQ5D.activites == 5 ~ 0.24005
    ),
    
    # --- DOULEUR / GÊNE ---
    c_doul = case_when(
      EQ5D.douleur == 1 ~ 0,
      EQ5D.douleur == 2 ~ 0.02198,
      EQ5D.douleur == 3 ~ 0.04704,
      EQ5D.douleur == 4 ~ 0.26374,
      EQ5D.douleur == 5 ~ 0.44399
    ),
    
    # --- ANXIÉTÉ / DÉPRESSION ---
    c_anx = case_when(
      EQ5D.anxiete == 1 ~ 0,
      EQ5D.anxiete == 2 ~ 0.02046,
      EQ5D.anxiete == 3 ~ 0.04683,
      EQ5D.anxiete == 4 ~ 0.20005,
      EQ5D.anxiete == 5 ~ 0.25803
    ),
    
   
    # Utilité = 1 - (somme des décréments)
    utilite = 1 - (c_mob + c_soins + c_act + c_doul + c_anx)
  )

# -----QALY----
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
  qaly_total = sum(qaly_periode, na.rm = TRUE) # Somme des QALYs de chaque période
)


# Comparaison rapide des moyennes par bras
aggregate(cbind(cout_total, qaly_total) ~ bras, data = df_final, mean)
