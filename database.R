# ============================================================================ #
#
#   UFTM BioStat DataBase
#
#     database.R
#
#     By: Pedro Diniz Guglielmeli <pedrodinizgu@gmail.com>
#     https://github.com/paulk2jonas/UFTM-BioStat-DataBase
#
#   TODO:
#     Add friend easter eggs
#     Remove city_index
#
# ============================================================================ #

# --------------------------------- Libraries -------------------------------- #
library(tidyverse)
library(readxl)
# library(here)

# --------------------------------- Variables -------------------------------- #
n <- 1000
seed <- 42

city_list <- read_excel("./UFTM-BioStat-DataBase/Background_Data/cities.xlsx")
city_list$population <- as.numeric(city_list$population)
# To save on processing power later on
city_list_total_pop <- sum(city_list$population)

city <- c()
state <- c()
# Because, otherwise, I can't get the cities to match withe their states
# seq_len used because the lint warned about a possible edge case
set.seed(seed)
city_index <- sample(x = seq_len(nrow(city_list)),
                     size = n,
                     replace = TRUE,
                     prob = city_list$population / city_list_total_pop)
for (i in 1:n) {
  city[i] <- city_list$city[city_index[i]]
  state[i] <- city_list$state[city_index[i]]
}

state_initials <- c("Acre" = "AC",
                    "Alagoas" = "AL",
                    "Amapá" = "AP",
                    "Amazonas" = "AM",
                    "Bahia" = "BA",
                    "Ceará" = "CE",
                    "Espírito Santo" = "ES",
                    "Goiás" = "GO",
                    "Maranhão" = "MA",
                    "Mato Grosso" = "MT",
                    "Mato Grosso do Sul" = "MS",
                    "Minas Gerais" = "MG",
                    "Pará" = "PA",
                    "Paraíba" = "PB",
                    "Paraná" = "PR",
                    "Pernambuco" = "PE",
                    "Piauí" = "PI",
                    "Rio de Janeiro" = "RJ",
                    "Rio Grande do Norte" = "RN",
                    "Rio Grande do Sul" = "RS",
                    "Rondônia" = "RO",
                    "Roraima" = "RR",
                    "Santa Catarina" = "SC",
                    "São Paulo" = "SP",
                    "Sergipe" = "SE",
                    "Tocantins" = "TO",
                    "Distrito Federal" = "DF")

city_state <- paste(city, " (", state_initials[state], ")", sep = "")

age_sex_by_city <- read_excel("./UFTM-BioStat-DataBase/Background_Data/age.xlsx")
cities <- unique(age_sex_by_city$city)
ages <- unique(age_sex_by_city$age)  # ! Currently not being used

# Using pre-computed populations by sex, because computing JIT takes too long
sex_by_city <- read.csv("./UFTM-BioStat-DataBase/Background_Data/sex_by_city.csv")

sex <- c()
set.seed(seed)
for (i in 1:n) {
  if (city_state[i] %in% cities) {
    sex[i] <- sample(x = c("M", "F"),
                     size = 1,
                     prob = c(filter(sex_by_city,
                                     city == city_state[i])$male,
                              filter(sex_by_city,
                                     city == city_state[i])$female))
  } else {
    sex[i] <- sample(x = c("M", "F"),
                     size = 1,
                     prob = c(105792687, 108954822))
                     #! Qual a fonte disso?
  }
}

# Using pre-computed values for the same reason
age_by_sex <- read.csv("./UFTM-BioStat-DataBase/Background_Data/age_by_sex.csv")

age <- c()
set.seed(seed)
for (i in 1:n) {
  if (city_state[i] %in% cities) {
    if (sex[i] == "M") {
      age[i] <- sample(x = c(0:99),
                       size = 1,
                       prob = filter(age_sex_by_city,
                                     city == city_state[i])$male)
    } else {
      age[i] <- sample(x = c(0:99),
                       size = 1,
                       prob = filter(age_sex_by_city,
                                     city == city_state[i])$female)
    }
  } else {
    if (sex[i] == "M") {
      age[i] <- sample(x = c(0:99),
                       size = 1,
                       prob = age_by_sex$male)
    } else {
      age[i] <- sample(x = c(0:99),
                       size = 1,
                       prob = age_by_sex$female)
    }
  }
}

forename_list <- read.csv("./UFTM-BioStat-DataBase/Background_Data/first_name.csv") %>%
  select(1:4)

# ? Use all sex possibilities for each name?
male_names <- filter(forename_list, classification == "M") %>%
  select(-c(2:3))
female_names <- filter(forename_list, classification == "F") %>%
  select(-c(2, 4))

first_name <- c()
set.seed(seed)
for (i in 1:n) {
  if (sex[i] == "M") {
    first_name[i] <- sample(male_names$name,
                            size = 1,
                            prob = male_names$frequency_male / sum(male_names$frequency_male))
  } else if (sex[i] == "F") {
    first_name[i] <- sample(female_names$name,
                            prob = female_names$frequency_female / sum(female_names$frequency_female))
  }
}
first_name <- str_to_title(first_name)

surname_list <- read_excel("./UFTM-BioStat-DataBase/Background_Data/last_name.xlsx")

set.seed(seed)
last_name <- sample(surname_list$Surname,
                    size = n,
                    replace = TRUE,
                    prob = surname_list$Incidence / sum(surname_list$Incidence))

full_name <- paste(first_name, last_name, sep = " ")
