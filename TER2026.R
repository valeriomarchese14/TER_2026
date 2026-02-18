library(dplyr)
library(tidyr)
library(mice)
library(dplyr)
library(gtsummary)
library(margins)
library(ggplot2)
library(data.table)
library(hesim)
library(scales)
library(betareg)

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

# Comparaison de l'Age
t.test(Age.a.l.inclusion ~ bras, data = df_baseline, var.equal = TRUE)

# Comparaison de l'IMC
t.test(IMC ~ bras, data = df_baseline, var.equal = TRUE)

#on remarque un surpoids général, un age tres varié avec un grand ecart type

# 3. Statistiques pour les variables Catégorielles (Nombre et %)
print("--- Répartition Sexe ---")
table_sexe <- table(df_baseline$bras, df_baseline$sexe)
print(table_sexe)
barplot(t(table_sexe), 
        main = "Répartition Hommes/Femmes par Bras",
        xlab = "Bras de traitement", 
        ylab = "Nombre de patients",
        col = c("#ff9999", "#9999ff"), 
        border = "white",           
        space = 0.25,                 
        legend.text = TRUE)
colSums(table_sexe)
print(prop.table(table_sexe, margin = 1) * 100) # Pourcentages par ligne

test_sexe <- chisq.test(table_sexe)
print(test_sexe)

#il y a plus de femmes que d'homme généralement mais l'écart est grand dans le groupe B 

print("--- Répartition Tabac ---")
table_tabac <- table(df_baseline$bras, df_baseline$statut.tabagique)
print(table_tabac)
colSums(table_tabac)
print(prop.table(table_tabac, margin = 1) * 100)

test_tabac <- chisq.test(table_tabac)
print(test_tabac)


#---- tableau 1 -----
table1 <- df_baseline %>%
  tbl_summary(
    by = bras, 
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", # Moyenne (Écart-type)
      all_categorical() ~ "{n} ({p}%)"    # Effectif (Pourcentage)
    ),
    label = list(
      Age.a.l.inclusion ~ "Âge à l'inclusion",
      statut.tabagique ~ "Statut Tabagique"
    )
  ) %>%
  add_p() %>% # Ajoute les p-valeurs
  bold_labels()
table1

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

df_long_A <- complete(imp_A, action = "long")

# Réintégration de l'Identifiant 
df_long_A$Identifiant.patient <- df_A$Identifiant.patient[df_long_A$.id]

# Calcul des utilités par imputation
df_long_A <- df_long_A %>%
  mutate(
    c_mob = case_when(EQ5D.mobilité == 1 ~ 0, EQ5D.mobilité == 2 ~ 0.03759, EQ5D.mobilité == 3 ~ 0.04774, EQ5D.mobilité == 4 ~ 0.17949, EQ5D.mobilité == 5 ~ 0.32509, TRUE ~ 0),
    c_soins = case_when(EQ5D.soins == 1 ~ 0, EQ5D.soins == 2 ~ 0.03656, EQ5D.soins == 3 ~ 0.050781, EQ5D.soins == 4 ~ 0.172251, EQ5D.soins == 5 ~ 0.258331, TRUE ~ 0),
    c_act = case_when(EQ5D.activites == 1 ~ 0, EQ5D.activites == 2 ~ 0.03313, EQ5D.activites == 3 ~ 0.03979, EQ5D.activites == 4 ~ 0.15689, EQ5D.activites == 5 ~ 0.24005, TRUE ~ 0),
    c_doul = case_when(EQ5D.douleur == 1 ~ 0, EQ5D.douleur == 2 ~ 0.02198, EQ5D.douleur == 3 ~ 0.04704, EQ5D.douleur == 4 ~ 0.26374, EQ5D.douleur == 5 ~ 0.44399, TRUE ~ 0),
    c_anx = case_when(EQ5D.anxiete == 1 ~ 0, EQ5D.anxiete == 2 ~ 0.02046, EQ5D.anxiete == 3 ~ 0.04683, EQ5D.anxiete == 4 ~ 0.20005, EQ5D.anxiete == 5 ~ 0.25803, TRUE ~ 0),
    utilite = 1 - (c_mob + c_soins + c_act + c_doul + c_anx)
  )

# Regroupement et moyenne
df_complet_A <- df_long_A %>%
  group_by(.id) %>%
  summarise(
    Identifiant.patient = first(Identifiant.patient), 
    mois = first(mois),
    visite = first(visite),
    Age.a.l.inclusion = first(Age.a.l.inclusion),
    sexe = first(sexe),
    IMC = first(IMC),
    cout.baseline = mean(cout.baseline, na.rm = TRUE),
    cout.aprés = mean(cout.aprés, na.rm = TRUE),
    utilite = mean(utilite, na.rm = TRUE)
  ) %>%
  mutate(bras = "A") %>%
  select(-.id)

df_long_B <- complete(imp_B, action = "long")

# Réintégration de l'Identifiant 
df_long_B$Identifiant.patient <- df_B$Identifiant.patient[df_long_B$.id]

# Calcul des utilités par imputation
df_long_B <- df_long_B %>%
  mutate(
    c_mob = case_when(EQ5D.mobilité == 1 ~ 0, EQ5D.mobilité == 2 ~ 0.03759, EQ5D.mobilité == 3 ~ 0.04774, EQ5D.mobilité == 4 ~ 0.17949, EQ5D.mobilité == 5 ~ 0.32509, TRUE ~ 0),
    c_soins = case_when(EQ5D.soins == 1 ~ 0, EQ5D.soins == 2 ~ 0.03656, EQ5D.soins == 3 ~ 0.050781, EQ5D.soins == 4 ~ 0.172251, EQ5D.soins == 5 ~ 0.258331, TRUE ~ 0),
    c_act = case_when(EQ5D.activites == 1 ~ 0, EQ5D.activites == 2 ~ 0.03313, EQ5D.activites == 3 ~ 0.03979, EQ5D.activites == 4 ~ 0.15689, EQ5D.activites == 5 ~ 0.24005, TRUE ~ 0),
    c_doul = case_when(EQ5D.douleur == 1 ~ 0, EQ5D.douleur == 2 ~ 0.02198, EQ5D.douleur == 3 ~ 0.04704, EQ5D.douleur == 4 ~ 0.26374, EQ5D.douleur == 5 ~ 0.44399, TRUE ~ 0),
    c_anx = case_when(EQ5D.anxiete == 1 ~ 0, EQ5D.anxiete == 2 ~ 0.02046, EQ5D.anxiete == 3 ~ 0.04683, EQ5D.anxiete == 4 ~ 0.20005, EQ5D.anxiete == 5 ~ 0.25803, TRUE ~ 0),
    utilite = 1 - (c_mob + c_soins + c_act + c_doul + c_anx)
  )

# Regroupement et moyenne
df_complet_B <- df_long_B %>%
  group_by(.id) %>%
  summarise(
    Identifiant.patient = first(Identifiant.patient), 
    mois = first(mois),
    visite = first(visite),
    Age.a.l.inclusion = first(Age.a.l.inclusion),
    sexe = first(sexe),
    IMC = first(IMC),
    cout.baseline = mean(cout.baseline, na.rm = TRUE),
    cout.aprés = mean(cout.aprés, na.rm = TRUE),
    utilite = mean(utilite, na.rm = TRUE)
  ) %>%
  mutate(bras = "B") %>%
  select(-.id)

df_complet <- bind_rows(df_complet_A, df_complet_B)

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
  Age = first(`Age.a.l.inclusion`), 
  sexe = first(sexe),
  IMC = first(IMC),
  cout_total = sum(cout.baseline + cout.aprés, na.rm = TRUE),
  qaly_total = sum(qaly_periode, na.rm = TRUE) # Somme des QALYs de chaque période
)

#Calcul des moyennes d'utilité par bras et par mois
df_evol <- df_complet %>%
  group_by(bras, mois) %>%
  summarise(utilite_moy = mean(utilite, na.rm = TRUE))

ggplot(df_evol, aes(x = mois, y = utilite_moy, color = bras, group = bras)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("A" = "red", "B" = "green"), 
                     labels = c("A" = "Chirurgie (A)", "B" = "Médicament (B)")) +
  scale_x_continuous(breaks = c(0, 1, 6, 12)) +
  ylim(0, 1) + 
  labs(
    title = "Évolution de la Qualité de Vie sur 1 an",
    x = "Mois",
    y = "Score d'utilité moyen",
    color = "Stratégie"
  ) +
  theme_minimal()
# Comparaison rapide des moyennes par bras
aggregate(cbind(cout_total, qaly_total) ~ bras, data = df_final, mean)

#----Modèles----

# distribution des coûts
hist(df_final$cout_total, main= "distribution des coûts", xlab= "coûts totaux")

# Modèle 1 : gamma
model_cout_simple <- glm(cout_total ~ bras, 
                         data = df_final, 
                         family = Gamma(link = "log"))
summary(model_cout_simple)
margins(model_cout_simple)

# Modèle 2 : Modèle ajusté (Gamma + variables cliniques)
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

AIC(model_cout_ajuste, model_cout_simple)
AIC(model_cout_ajuste, model_cout_invG)

# Le meilleur modèle est le premier (aic plus bas). Les autres variables autres que le bras n'influence pas le cout. La distribution est bien gamma.
#En moyenne, un individu du bras B coute 9497 euros de moins mais qaly plus faible

#test sur la loi des QALY
shapiro.test(residuals(model_qaly_gauss)) 
hist(df_final$qaly_total, main= "distribution des QALY", xlab= "QALY totaux")

# Modèle GLM Gaussien pour l'efficacité 

model_qaly_gauss <- glm(qaly_total ~ bras, 
                        
                        data = df_final, 
                        
                        family = gaussian(link = "identity"))


summary(model_qaly_gauss)
margins(model_qaly_gauss)

# Modèle GLM Gaussien ajusté au sexe pour l'efficacité 

model_qaly_gauss2 <- glm(qaly_total ~ bras + sexe, 
                        
                        data = df_final, 
                        
                        family = gaussian(link = "identity"))


summary(model_qaly_gauss2)

# Modèle Log-normal pour les QALY ( ne fonctionne que parce que les QALY sont >0)
model_qaly_lognorm <- glm(qaly_total ~ bras, 
                          data = df_final, 
                          family = gaussian(link = "log"))

summary(model_qaly_lognorm)

# Modèle Beta pour les QALY
n <- nrow(df_final)
df_final$qaly_adj <- (df_final$qaly_total * (n - 1) + 0.5) / n

model_qaly_beta <- betareg(qaly_adj ~ bras, data = df_final)

summary(model_qaly_beta)

# Comparaison des AIC
AIC(model_qaly_gauss, model_qaly_lognorm)
AIC(model_qaly_lognorm,model_qaly_beta)
AIC(model_qaly_gauss, model_qaly_gauss2)



# On perd 0.135 qaly en moyenne dans le bras b

delta_E <- coef(model_qaly_gauss)["brasB"]
summary_margins_cout <- summary(margins(model_cout_simple))
delta_C <- summary_margins_cout$AME[summary_margins_cout$factor == "brasB"]

# Calcul du RDCR (ICER)
ICER <- delta_C / delta_E

#1 qaly gagné dans le bras A représente 70369 euros. le bras A est mieux mais plus cher.
#le bras B est moins bien mais moins cher.

#----Bootstrap----
B <- 1000
set.seed(2026)

cost_A_boot <- numeric(B)
cost_B_boot <- numeric(B)
qaly_A_boot <- numeric(B)
qaly_B_boot <- numeric(B)

for(i in 1:B) {
  df_boot <- df_final[sample(1:nrow(df_final), replace = TRUE), ]
  
  # Modèles
  mod_qaly <- glm(qaly_total ~ bras, data = df_boot, family = gaussian(link = "identity"))
  mod_cout <- glm(cout_total ~ bras, data = df_boot, family = Gamma(link = "log"))
  
  # Prédictions des moyennes pour le Bras A
  qaly_A_boot[i] <- predict(mod_qaly, newdata = data.frame(bras = "A"), type = "response")
  cost_A_boot[i] <- predict(mod_cout, newdata = data.frame(bras = "A"), type = "response")
  
  # Prédictions des moyennes pour le Bras B
  qaly_B_boot[i] <- predict(mod_qaly, newdata = data.frame(bras = "B"), type = "response")
  cost_B_boot[i] <- predict(mod_cout, newdata = data.frame(bras = "B"), type = "response")
}

# Formatage des données pour HESIM
df_hesim <- data.table(
  sample = rep(1:B, 2),
  strategy_id = rep(c("Bras A", "Bras B"), each = B),
  grp_id = 1, # Un seul groupe de patients ici
  c = c(cost_A_boot, cost_B_boot), # Coûts
  e = c(qaly_A_boot, qaly_B_boot)  # Efficacité (QALYs)
)

# L'analyse médico-économique avec hesim
wtp_thresholds <- seq(0, 100000, by = 5000)

cea_result <- cea(
  x = df_hesim, 
  k = wtp_thresholds, 
  sample = "sample", 
  strategy = "strategy_id", 
  grp = "grp_id", 
  e = "e", 
  c = "c"
)
summary(cea_result)

# On définit le Bras A comme traitement de référence pour la comparaison
cea_pw_result <- cea_pw(
  x = df_hesim,                  
  k = wtp_thresholds, 
  comparator = "Bras A",      
  sample = "sample", 
  strategy = "strategy_id", 
  grp = "grp_id", 
  e = "e", 
  c = "c"
)

df_delta <- cea_pw_result$delta

# Création du graphique avec ggplot
ggplot(data = df_delta, aes(x = ie, y = ic)) +
  geom_point(alpha = 0.5, color = "#2c3e50") +

  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  labs( title = "Plan Différentiel Coût-Efficacité (1000 simulations)",
    subtitle = "Comparaison : Bras B vs Bras A",
    x = "Différence d'Efficacité ",
    y = "Différence de Coûts "
  ) +
  theme_minimal()

df_ceac <- cea_pw_result$ceac
ggplot(data = df_ceac, aes(x = k, y = prob, color = strategy_id)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(labels = label_number(big.mark = " ")) +
  labs(
    title = "Courbe d'Acceptabilité (CEAC)",
    x = "Consentement à payer pour 1 QALY supplémentaire (€)",
    y = "Probabilité d'être la stratégie la plus coût-efficace",
    color = "Stratégie"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

