#Ne pas oublier de retélécharger le données à chaque fois 
install.packages("dyplr")
install.packages("stringr")
install.packages("haven")
install.packages("ISOweek")
install.packages("forcats")
library(forcats)
library(ISOweek)
library(haven)
library(dplyr)
library(stringr)


# Mise en place des données ####

data<- read_dta("eu-lfs 2023 sample.dta")

data_modifiable <- data
data_modifiable <- data_modifiable %>%
  select(-refweek) 

## On renomme le contenu des variabes ####

data_modifiable <- data_modifiable %>%
  mutate(
    # Colonnes character
    across(where(is.character),
           ~ .x %>% na_if("Not available") %>% as_factor()),
    
    # Colonnes factor
    across(where(is.factor),
           ~ fct_na_value_to_level(.x, "Not available") %>% 
             fct_recode(!!!setNames(rep(NA, 1), "Not available")))
  )    

View(data_modifiable)

#Chaque colonne a un label qui explique le nom ESS de la colonne : je prends cette description comme nom de colonne
# Extraire les labels
labels <- sapply(data_modifiable, function(x) attr(x, "label"))

# Remplacer les NULL et les "" par les noms actuels
labels[sapply(labels, is.null) | labels == ""] <- names(data_modifiable)[sapply(labels, is.null) | labels == ""]

# Appliquer les nouveaux noms
names(data_modifiable) <- labels

name_vec <- names(data_modifiable)
names(data_modifiable) <- make.unique(name_vec, sep = ".")

