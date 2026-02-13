library(dplyr)
library(tidyr)
library(mice)
library(dplyr)
library(gtsummary)
library(margins)

#----analyse descriptive----
df <- read.csv("BDD_2026.csv", sep = ";", fileEncoding = "latin1", stringsAsFactors = TRUE)

#Les individus ont l'air de ne pas avoir changé d'imc, de statut tabgagique et de profession.
#On se concentre alors sur leurs données en baseline
df_baseline <- df %>% 
  filter(visite == "Baseline") %>%
  select(bras, `Age.a.l.inclusion`, sexe, IMC, `statut.tabagique`, Profession)

stats_cont <- df_baseline %>%
  group_by(bras) %>%
  summarise(
    N = n(),
    Age_Moy = mean(`Age.a.l.inclusion`, na.rm = TRUE),
    Age_SD  = sd(`Age.a.l.inclusion`, na.rm = TRUE),
    Age_med = median(`Age.a.l.inclusion`, na.rm = TRUE),
    IMC_Moy = mean(IMC, na.rm = TRUE),
    IMC_SD  = sd(IMC, na.rm = TRUE),
    IMC_med= median(IMC, na.rm=TRUE)
  )

print(stats_cont)

#on remarque un surpoids général, un age tres varié avec un grand ecart type

# 3. Statistiques pour les variables Catégorielles (Nombre et %)
print("--- Répartition Sexe ---")
table_sexe <- table(df_baseline$bras, df_baseline$sexe)
print(table_sexe)
colSums(table_sexe)
print(prop.table(table_sexe, margin = 1) * 100) # Pourcentages par ligne

#il y a plus de femmes que d'homme généralement mais l'écart est grand dans le groupe B 

print("--- Répartition Tabac ---")
table_tabac <- table(df_baseline$bras, df_baseline$statut.tabagique)
print(table_tabac)
colSums(table_tabac)
print(prop.table(table_tabac, margin = 1) * 100)
#----nettoyage-----

# Gestion des vides
df[df == ""] <- NA

# Gestion des zéros structurels
df <- df %>%
  mutate(
    cout.baseline = ifelse(visite != "Baseline", 0, cout.baseline),
    cout.aprés = ifelse(visite == "Baseline", 0, cout.aprés)
  )

#----imputation----
# Séparation des bras
df_A <- df %>% filter(bras == "A")
df_B <- df %>% filter(bras == "B")

# Imputation séparée 
imp_A <- mice(df_A %>% select(-Identifiant.patient), m = 5, method = 'pmm', seed = 123, print = FALSE)
imp_B <- mice(df_B %>% select(-Identifiant.patient), m = 5, method = 'pmm', seed = 123, print = FALSE)

# Création des bases complètes
df_complet_A <- complete(imp_A, 1)
df_complet_B <- complete(imp_B, 1)

# Réintégration de l'identifiant et du bras
# On reprend la colonne ID des dataframes originaux (df_A et df_B)
df_complet_A$Identifiant.patient <- df_A$Identifiant.patient
df_complet_A$bras <- "A"

df_complet_B$Identifiant.patient <- df_B$Identifiant.patient
df_complet_B$bras <- "B"

#Recombinaison finale
df_complet <- bind_rows(df_complet_A, df_complet_B)


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
    
    qaly_periode = delta_temps* utilite_moyenne
  ) %>%
summarise(
  bras = first(bras),
  Age = first(`Age.a.l.inclusion`), 
  sexe = first(sexe),
  IMC = first(IMC),
  cout_total = sum(cout.baseline + cout.aprés, na.rm = TRUE),
  qaly_total = sum(qaly_periode, na.rm = TRUE) # Somme des QALYs de chaque période
)


# Comparaison rapide des moyennes par bras
aggregate(cbind(cout_total, qaly_total) ~ bras, data = df_final, mean)

# Modèle 1 : gamma
model_cout_simple <- glm(cout_total ~ bras, 
                         data = df_final, 
                         family = Gamma(link = "log"))
summary(model_cout_simple)
margins(model_cout_simple)

# Modèle 2 : Modèle ajusté (Gamma + variables cliniques)
# Est-ce que le coût dépend aussi de l'âge ou de l'IMC initial ?
model_cout_ajuste <- glm(cout_total ~ bras + Age + sexe + IMC, 
                         data = df_final, 
                         family = Gamma(link = "log"))
summary(model_cout_ajuste)
margins(model_cout_ajuste)

# Modèle 3 : Changement de distribution (Inverse-Gaussienne)
model_cout_invG <- glm(cout_total ~ bras, 
                       data = df_final, 
                       family = inverse.gaussian(link = "log"))

summary(model_cout_invG)
margins(model_cout_invG)

# Le meilleur modèle est le premier ( aic plus bas). Les autres variables autres que le bras
# n'influence pas le cout. La distribution est bien gamma.
#En moyenne, un individu du bras B coute 9308 euros de moins mais qaly plus faible


# Modèle GLM Gaussien pour l'efficacité 
model_qaly_gauss <- glm(qaly_total ~ bras, 
                        data = df_final, 
                        family = gaussian(link = "identity"))

summary(model_qaly_gauss)
margins(model_qaly_gauss)
shapiro.test(residuals(model_qaly_gauss)) #les qaly suivent bien une loi normale.
# On perd 0.13 qaly en moyenne dans le bras b

delta_E <- coef(model_qaly_gauss)["brasB"]
summary_margins_cout <- summary(margins(model_cout_simple))
delta_C <- summary_margins_cout$AME[summary_margins_cout$factor == "brasB"]

# Calcul du RDCR (ICER)
ICER <- delta_C / delta_E

#1 qaly gagné dans le bras A représente 70000 euros. le bras A est mieux mais plus cher.
#le bras B est moins bien mais moins cher.
margins(model_cout)
