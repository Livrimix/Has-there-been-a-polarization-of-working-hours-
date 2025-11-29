#Ne pas oublier de retélécharger le données à chaque fois 
install.packages("dyplr")
install.packages("stringr")
install.packages("haven")
install.packages("ISOweek")
install.packages("forcats")
install.packages("countrycode")
library(countrycode)
library(forcats)
library(ISOweek)
library(haven)
library(dplyr)
library(stringr)


# Mise en place des données ####

data<- read_dta("eu-lfs 2023 sample.dta")

data_modifiable <- data
data_modifiable <- data_modifiable %>%
  select(-refweek, -hhnum, -qhhnum, -hhseqnum, -intweek, -intwave, - intquest,
         -mode, - proxy, -region_2d, -region_2dw, -degurba, -cobfath, -cobmoth,
         -migreas, - countrpr, -hhspou, -hhmoth, -hhfath, -jattach, -nace2_1d,
         -nace1_1d2j, -nace2_1dpr, -nace1_1d, -nace2_1d2j, -nace1_1dpr,
         -mainclnt, -country, -countryb, -citizenship, -countryw,-seekreas, -wantreas,- availble,
         -isco08_1d, -eseg_1d) 
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


data_modifiable <- data_modifiable %>%
  mutate(
    sex = case_when(
      sex == 1 ~ "male",
      sex == 2 ~ "female",
      TRUE ~ NA_character_  # tout autre code devient NA
    ),
    sex = as_factor(sex)    # convertir en factor
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

#Chaque colonne a un label qui explique le nom ESS de la colonne : je prends cette description comme nom de colonne
# Extraire les labels
labels <- sapply(data_modifiable, function(x) attr(x, "label"))

# Remplacer les NULL et les "" par les noms actuels
labels[sapply(labels, is.null) | labels == ""] <- names(data_modifiable)[sapply(labels, is.null) | labels == ""]

# Appliquer les nouveaux noms
names(data_modifiable) <- labels

name_vec <- names(data_modifiable)
names(data_modifiable) <- make.unique(name_vec, sep = ".")

