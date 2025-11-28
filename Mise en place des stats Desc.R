#Ne pas oublier de retélécharger le données à chaque fois 
install.packages("dyplr")
library(haven)
library(dplyr)

data<- read_dta("eu-lfs 2023 sample.dta")

data_modifiable <- data

#Chaque colonne a un label qui explique le nom ESS de la colonne : je prends cette description comme nom de colonne
# Extraire les labels
labels <- sapply(data_modifiable, function(x) attr(x, "label"))

# Remplacer les NULL et les "" par les noms actuels
labels[sapply(labels, is.null) | labels == ""] <- names(data_modifiable)[sapply(labels, is.null) | labels == ""]

# Appliquer les nouveaux noms
names(data_modifiable) <- labels

View(data_modifiable)
