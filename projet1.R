library(tidyverse)
library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read.csv("owid-covid-data.csv",sep=',')


#on veut analyser juste des donnees concernant le Maroc donc on cree une sous-table 
data_maroc <- data[data$location=="Morocco",]




variables_to_include <- c("new_cases", "new_deaths","new_vaccinations","hosp_patients","icu_patients")

# Grouper les données par "location" et calculer les statistiques descriptives pour les variables spécifiques
summary_table <- data %>%
  group_by(location) %>%
  summarise_at(vars(variables_to_include), list(min = min, max = max, mean = mean, median = median),na.rm="True")

# Afficher le tableau résultant
print(summary_table)

write.csv(summary_table, "SUMMARY.csv", row.names = FALSE)




s<-as.data.frame(summary(data_maroc))
s %>%
  pivot_longer(names_from = "" )
  

data %>%
  filter(location %in% c("Morocco","France","Spain","Italy")) %>%
  group_by(location,date) %>%
  ggplot()+
  geom_point(aes(x=date,y=new_deaths,col=`location`))




# Convertir la colonne de la date en format Date
data_maroc$date <- as.Date(data_maroc$date)

data_maroc %>%
  ggplot()+
  geom_point(aes(x=date,y=new_deaths))

data$date <- as.Date(data$date)

data %>%
  filter(location %in% c("Morocco","France","Spain","Italy")) %>%
  group_by(location,date) %>%
  ggplot()+
  geom_point(aes(x=date,y=new_deaths,col=`location`))
  


#mar_data$date <- strptime(as.character(mar_data$date), format = "%Y-%m-%d")
# Tracer l'évolution des nouveaux cas en fonction de la date
ggplot(data_maroc, aes(x = date, y = new_cases_smoothed)) +
  geom_line() + 
  labs(x = "Date", y = "Nouveaux cas", title = "Évolution des nouveaux cas de COVID-19") +
  theme_minimal()


ggplot(data_maroc, aes(x = date, y = new_deaths)) +
  geom_line() + 
  labs(x = "Date", y = "Nouveaux decedes", title = "Évolution des nouveaux decedes de COVID-19") +
  theme_minimal()


data_subset <- subset(data_maroc, date >= "2021-01-29")

ggplot(data_subset, aes(x = date, y = new_vaccinations_smoothed)) +
  geom_line() + 
  labs(x = "Date", y = "Nouveaux decedes", title = "Évolution des nouveaux decedes de COVID-19") +
  theme_minimal()

summary(mar_data$new_people_vaccinated_smoothed)

ggplot()+geom_line(data=mar_data,aes(x=date,y=new_cases_smoothed,color="red"))+geom_line(data=mar_data,aes(x=date,y=new_vaccinations_smoothed,color="blue"))
+scale_y_contiuous(limits=c(0,10000))


# Calcul du coefficient de corrélation linéaire entre les variables 
correlation <- cor(mar_data$total_cases, mar_data$new_cases, use = "complete.obs")
print(correlation)

# Réalisation de régressions linéaires pour analyser les tendances
regression_model <- lm(total_cases ~ date, data = mar_data)
summary(regression_model)


# Réalisation de tests statistiques pour confirmer ou infirmer l'hypothèse nulle
cor.test(mar_data$population_density, mar_data$new_cases, method = "pearson")














# Calcul des statistiques descriptives pour les variables d'intérêt
summary_stats <- data_maroc %>%
  select(new_cases, new_deaths, new_vaccinations) %>%
  summarise(
    mean_new_cases = mean(new_cases, na.rm = TRUE),
    median_new_cases = median(new_cases, na.rm = TRUE),
    sd_new_cases = sd(new_cases, na.rm = TRUE),
    min_new_cases = min(new_cases, na.rm = TRUE),
    max_new_cases = max(new_cases, na.rm = TRUE),
    
    mean_new_deaths = mean(new_deaths, na.rm = TRUE),
    median_new_deaths = median(new_deaths, na.rm = TRUE),
    sd_new_deaths = sd(new_deaths, na.rm = TRUE),
    min_new_deaths = min(new_deaths, na.rm = TRUE),
    max_new_deaths = max(new_deaths, na.rm = TRUE),
    
    mean_new_vaccinations = mean(new_vaccinations, na.rm = TRUE),
    median_new_vaccinations = median(new_vaccinations, na.rm = TRUE),
    sd_new_vaccinations = sd(new_vaccinations, na.rm = TRUE),
    min_new_vaccinations = min(new_vaccinations, na.rm = TRUE),
    max_new_vaccinations = max(new_vaccinations, na.rm = TRUE)
  )

# Afficher les statistiques descriptives
print(summary_stats)

