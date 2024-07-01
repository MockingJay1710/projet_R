library(tidyverse)
library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)


#lecture des donnees
data <- read.csv("owid-covid-data.csv",sep=',')

#verification des attributs de la table `data`
names(data)

#conversion de la date en type Date
data$date<-as.Date(data$date)
class(data$date)

# Création de la colonne 'total_recovered'
data$total_recovered <- data$total_cases - data$total_deaths

# Création de la colonne 'new_recovered'
data$new_recovered <- data$new_cases - data$new_deaths


#creation d'une nouvelle table ne contenant que les donnees du Maroc
data_maroc <- data[data$location=="Morocco",]

# Remplacer les valeurs NA restantes par 0
data_maroc[is.na(data_maroc)] <- 0




#tendances
##cas confirmes
summary(data_maroc$new_cases_smoothed)
boxplot(data_maroc$new_cases_smoothed,main="Nouveaux cas",horizontal = T)
data_maroc %>%
  ggplot()+
  geom_point(aes(x=date,y=new_cases_smoothed,col="red"))+
  labs(x="Date",y="Nouveaux cas",title = "Nouveaux cas confirmes (moyennes sur 7jrs)")+
  theme_bw()

##les decedes
summary(data_maroc$new_deaths_smoothed)
boxplot(data_maroc$new_deaths_smoothed,main="Nouveaux decedes",horizontal = T)
data_maroc %>%
  ggplot()+
  geom_point(aes(x=date,y=new_deaths_smoothed,col="red"))+
  labs(x="Date",y="Nouveaux decedes",title = "Nouveaux decedes (moyennes sur 7jrs)")+
  theme_bw()

##estimation des guerisons

summary(data_maroc$new_recovered)
boxplot(data_maroc$new_recovered,main="Nouveaux gueris",horizontal = T)
data_maroc %>%
  ggplot()+
  geom_point(aes(x=date,y=new_recovered,col="red"))+
  labs(x="Date",y="Nouveaux gueris",title = "Nouveaux gueris (moyennes sur 7jrs)")+
  theme_bw()

# Filtrer les données pour supprimer les lignes où total_cases est NA ou égal à 0
data_maroc <- data_maroc %>%
  filter(!is.na(new_recovered) & new_recovered != 0)








###d'abord pour faire une comparaison on doit utiliser les variables _per_million pour de l'equite



# Sélectionner les pays d'intérêt
countries_of_interest <- c("Morocco", "New Zealand", "India", "Brazil")

# Filtrer les données pour les pays d'intérêt et les dates pertinentes
data_filtered <- data %>%
  filter(location %in% countries_of_interest & date >= "2020-01-01" & date <= "2024-01-01")

# Remplacer les valeurs manquantes par des zéros pour les variables d'intérêt
data_filtered <- data_filtered %>% 
  mutate(new_cases_smoothed_per_million = ifelse(is.na(new_cases_smoothed_per_million), 0, new_cases_smoothed_per_million),
         new_deaths_smoothed_per_million = ifelse(is.na(new_deaths_smoothed_per_million), 0, new_deaths_smoothed_per_million),
         new_tests_smoothed_per_thousand = ifelse(is.na(new_tests_smoothed_per_thousand), 0, new_tests_smoothed_per_thousand),
         new_vaccinations_smoothed_per_million = ifelse(is.na(new_vaccinations_smoothed_per_million), 0, new_vaccinations_smoothed_per_million))

# Comparaison des nouveaux cas confirmés
ggplot(data_filtered, aes(x = date, y = new_cases_smoothed_per_million, color = location)) +
  geom_line() + 
  labs(x = "Date", y = "Nouveaux cas confirmés (moyenne 7j)", title = "Comparaison des nouveaux cas confirmés entre pays") +
  theme_minimal()

# Comparaison des nouveaux décès
ggplot(data_filtered, aes(x = date, y = new_deaths_smoothed_per_million, color = location)) +
  geom_line() + 
  labs(x = "Date", y = "Nouveaux décès (moyenne 7j)", title = "Comparaison des nouveaux décès entre pays") +
  theme_minimal()

# Comparaison des tests réalisés
ggplot(data_filtered, aes(x = date, y = new_tests_smoothed_per_thousand, color = location)) +
  geom_line() + 
  labs(x = "Date", y = "Tests réalisés (moyenne 7j)", title = "Comparaison des tests réalisés entre pays") +
  theme_minimal()

# Comparaison des vaccinations
ggplot(data_filtered, aes(x = date, y = new_vaccinations_smoothed_per_million, color = location)) +
  geom_line() + 
  labs(x = "Date", y = "Vaccinations (moyenne 7j)", title = "Comparaison des vaccinations entre pays") +
  theme_minimal()

# Comparaison des guerisons
ggplot(data_filtered, aes(x = date, y = new_recovered, color = location)) +
  geom_line() + 
  labs(x = "Date", y = "Guerisons (moyenne 7j)", title = "Comparaison des guerisons entre pays") +
  theme_minimal()









##correlation
# Filtrer les données pour supprimer les lignes où les valeurs sont NA
donnees_filtrees <- data_maroc %>%
  filter(!is.na(total_cases) & total_cases != 0)

# Remplacer les valeurs NA par 0
donnees_filtrees[is.na(donnees_filtrees)] <- 0

# Sélectionner les colonnes pertinentes pour l'analyse de corrélation
variables <- donnees_filtrees %>%
  select(new_cases, new_deaths, new_tests, total_cases, total_deaths, total_tests, new_recovered, total_recovered)

# Calculer la matrice de corrélation
correlation_matrix <- cor(variables)

# Afficher la matrice de corrélation
print(correlation_matrix)


# Convertir la matrice de corrélation en un data.frame long
correlation_matrix_long <- melt(correlation_matrix)

# Plotter la heatmap
ggplot(data = correlation_matrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Corrélation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Matrice de corrélation entre les variables")




##regression lineaire

# Création de données d'exemple


# Régression linéaire: Total des Cas vs Total des Décès
model_total_cases_total_deaths <- lm(total_deaths ~ total_cases, data = donnees_filtrees)
ggplot(donnees_filtrees, aes(x = total_cases, y = total_deaths)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Total des Cas vs Total des Décès",
       x = "Total des Cas",
       y = "Total des Décès") +
  theme_minimal()

# Régression linéaire: Total Cas vs Total Guéris
model_new_cases_new_recovered <- lm(total_recovered ~ total_cases, data = donnees_filtrees)
ggplot(donnees_filtrees, aes(x = total_cases, y = total_recovered)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Total Cas vs Total Guéris",
       x = "Total Cas",
       y = "Total Guéris") +
  theme_minimal()



#hypothese nulle et infirmation
## pas de correlation entre new_cases et new_deaths
cor.test(data_maroc$new_cases, data_maroc$new_deaths, method = "pearson")


# Régression linéaire: Total Cas vs Total Guéris
model_new_cases_new_recovered <- lm(new_cases ~ new_recovered, data = donnees_filtrees)
ggplot(donnees_filtrees, aes(x = new_cases, y = new_recovered)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "nouveaux Cas vs nouveaux gueris",
       x = "nouveaux Cas",
       y = "nouveaux gueris") +
  theme_minimal()
