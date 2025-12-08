#Ne pas oublier de retélécharger le données à chaque fois 
install.packages("dyplr")
install.packages("stringr")
install.packages("haven")
install.packages("ISOweek")
install.packages("forcats")
install.packages("countrycode")
library(ggplot2)
library(countrycode)
library(forcats)
library(ISOweek)
library(haven)
library(dplyr)
library(stringr)
library(purrr)
library(scales)


# Mise en place des données ####

data<- read_dta("eu-lfs 2023 sample.dta")

data_modifiable <- data
data_modifiable_2 <- data_modifiable_2 %>%
  select(-refweek, -hhnum, -qhhnum, -hhseqnum, -intweek, -intwave, - intquest,
         -mode, - proxy, -region_2d, -region_2dw, -degurba, -cobfath, -cobmoth,
         -migreas, - countrpr, -hhspou, -hhmoth, -hhfath, -jattach, -nace2_1d,
         -nace1_1d2j, -nace2_1dpr, -nace1_1d, -nace2_1d2j, -nace1_1dpr,
         -mainclnt, -country, -countryb, -citizenship, -countryw,-seekreas, -wantreas,- availble,
         -isco08_1d, -eseg_1d) 

# (erreur pour vérifier qu'on a bien supprimé cette variable)
data_modifiable <- data_modifiable %>%
  select(-eseg_1d)

## On renomme le contenu des variabes ####
data_modifiable <- data_modifiable %>%
  mutate(
    refmonth = as_factor(refmonth),           # transforme en factor
    refmonth = na_if(refmonth, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    hhtype = as_factor(hhtype),           # transforme en factor
    hhtype = na_if(hhtype, "Not available")  # remplace "Not available" par NA
  )

# "Sex" devient une indicatrice 1=femme, 0=homme
data_modifiable <- data_modifiable %>%
  mutate(
    sex = case_when(
      sex == 1 ~ 0,
      sex == 2 ~ 1,
      TRUE ~ NA_real_
    )
  )

data_modifiable <- data_modifiable %>%
  mutate(
    ageresid = as_factor(ageresid),           # transforme en factor
    ageresid = na_if(ageresid, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    hhlink = as_factor(hhlink),           # transforme en factor
    hhlink = na_if(hhlink, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    wkstat = as_factor(wkstat),           # transforme en factor
    wkstat = na_if(wkstat, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    absreas = as_factor(absreas),           # transforme en factor
    absreas = na_if(absreas, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    empstat = as_factor(empstat),           # transforme en factor
    empstat = na_if(empstat, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    numjob = as_factor(numjob),           # transforme en factor
    numjob = na_if(numjob, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    seekwork = as_factor(seekwork),           # transforme en factor
    seekwork = na_if(seekwork, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    wantwork = as_factor(wantwork),           # transforme en factor
    wantwork = na_if(wantwork, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    actmetne = as_factor(actmetne),           # transforme en factor
    actmetne = na_if(actmetne, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    wishmore = as_factor(wishmore),           # transforme en factor
    wishmore = na_if(wishmore, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    avaireas = as_factor(avaireas),           # transforme en factor
    avaireas = na_if(avaireas, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    ilostat = as_factor(ilostat),           # transforme en factor
    ilostat = na_if(ilostat, "Not available")  # remplace "Not available" par NA
  )

data_modifiable <- data_modifiable %>%
  mutate(
    homework = as_factor(homework),
    homework = na_if(homework, "Not available"),
    
    stapro = as_factor(stapro),
    stapro = na_if(stapro, "Not available"),
    
    isco08_3d = as_factor(isco08_3d),
    isco08_3d = na_if(isco08_3d, "Not available"),
    
    eseg_2d = as_factor(eseg_2d),
    eseg_2d = na_if(eseg_2d, "Not available"),
    
    ftpt = as_factor(ftpt),
    ftpt = na_if(ftpt, "Not available"),
    
    temp = as_factor(temp),
    temp = na_if(temp, "Not available"),
    
    tempdur = as_factor(tempdur),
    tempdur = na_if(tempdur, "Not available"),
    
    tempreas = as_factor(tempreas),
    tempreas = na_if(tempreas, "Not available"),
    
    tempagcy = as_factor(tempagcy),
    tempagcy = na_if(tempagcy, "Not available"),
    
    ftptreas = as_factor(ftptreas),
    ftptreas = na_if(ftptreas, "Not available"),
    
    varitime = as_factor(varitime),
    varitime = na_if(varitime, "Not available"),
    
    lookoj = as_factor(lookoj),
    lookoj = na_if(lookoj, "Not available"),
    
    hwwish = as_factor(hwwish),
    hwwish = na_if(hwwish, "Not available"),
    
    needcare = as_factor(needcare),
    needcare = na_if(needcare, "Not available")
  )

View(data_modifiable)

## Nettoyer les noms des colonnes ####

# Extraire les labels
labels <- sapply(data_modifiable, function(x) attr(x, "label"))

# Remplacer les NULL et les "" par les noms actuels
labels[sapply(labels, is.null) | labels == ""] <- names(data_modifiable)[sapply(labels, is.null) | labels == ""]

# Appliquer les nouveaux noms
names(data_modifiable) <- labels

name_vec <- names(data_modifiable)
names(data_modifiable) <- make.unique(name_vec, sep = ".")


# Liste des moms des variables et de leur type
print(tibble::tibble(
  variable = names(data_modifiable),
  type = purrr::map_chr(data_modifiable, ~ paste(class(.x), collapse = ", "))), n=Inf) 

# 48.32959 % d'hommes,  51.67041% de femmes
prop.table(table(data_modifiable$sex)) * 100

# 45.47575 % de personnes en emploi,  39.28243 % de non employés, 15.24183 de non applicable.
prop.table(table(data_modifiable$`Being in employment`)) * 100



# On crée la table qui ne garde que les personnes en emploi
data_emp <- data_modifiable %>%
  filter(`Being in employment` == "Employed")

# Histogramme — heures habituellement travaillées
ggplot(data_emp, aes(x = `Number of hours usually worked, main job`)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 40) +
  theme_minimal() +
  labs(
    title = "Distribution des heures habituellement travaillées (emplois uniquement)",
    x = "Heures habituelles",
    y = "Fréquence"
  )

# Histogramme — heures effectivement travaillées
ggplot(data_emp, aes(x = `Number of hours actually worked, main job`)) +
  geom_histogram(fill = "darkred", color = "white", bins = 40) +
  theme_minimal() +
  labs(
    title = "Distribution des heures effectivement travaillées (emplois uniquement)",
    x = "Heures effectuées",
    y = "Fréquence"
  )

# Comparaison des densités des heures habituellement VS effectivement travaillées
ggplot() +
  geom_density(
    data = data_modifiable %>% 
      filter(`Being in employment` == "Employed"),
    aes(x = `Number of hours usually worked, main job`, 
        color = "Heures habituelles",
        fill = "Heures habituelles"),
    alpha = 0.3
  ) +
  geom_density(
    data = data_modifiable %>% 
      filter(`Being in employment` == "Employed"),
    aes(x = `Number of hours actually worked, main job`, 
        color = "Heures effectuées",
        fill = "Heures effectuées"),
    alpha = 0.3
  ) +
  scale_color_manual(values = c("Heures habituelles" = "steelblue",
                                "Heures effectuées" = "darkred")) +
  scale_fill_manual(values = c("Heures habituelles" = "steelblue",
                               "Heures effectuées" = "darkred")) +
  theme_minimal() +
  labs(
    title = "Densités des heures habituellement / effectivement travaillées",
    x = "Heures",
    y = "Densité",
    color = "Type d'heures",
    fill = "Type d'heures"
  )

# Comparaison des heures moyennes travaillées par pays

# 1. Préparer les données : heures moyennes par pays (employés seulement)
hours_by_country <- data_modifiable %>%
  filter(`Being in employment` == "Employed") %>%
  group_by(`Country of residence`) %>%
  summarise(
    mean_hours = mean(`Number of hours usually worked, main job`, na.rm = TRUE),
    n = n(),
    sd_hours = sd(`Number of hours usually worked, main job`, na.rm = TRUE),
    se_hours = sd_hours / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_hours)) %>%
  filter(n > 100)  # Garder seulement les pays avec assez d'observations

# 2. Voir les résultats
# print(hours_by_country, n = Inf)

# 3. Graphique en barres horizontales (plus lisible)
ggplot(hours_by_country, 
       aes(x = reorder(`Country of residence`, mean_hours), 
           y = mean_hours)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  geom_errorbar(aes(ymin = mean_hours - se_hours, 
                    ymax = mean_hours + se_hours), 
                width = 0.2, color = "darkgray") +
  geom_text(aes(label = round(mean_hours, 1)), 
            hjust = -0.2, size = 3) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Heures hebdomadaires moyennes par pays de résidence",
    subtitle = "Personnes en emploi seulement | Barres d'erreur = erreur standard",
    x = "Pays de résidence",
    y = "Heures habituellement travaillées (moyenne)"
  ) +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold")
  )

