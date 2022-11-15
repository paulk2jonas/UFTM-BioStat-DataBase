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
#     Remove city_index and other unused variables/files
#
#   * The cities:
#       Mojuí dos Campos (PA)
#       Balneário Rincão (SC)
#       Boa Saúde (RN)
#       Pescaria Brava (SC)
#       Paraíso das Águas (MS)
#       Pinto Bandeira (RS)
#       Januário Cicco (RN)
#     Were excluded because they weren't on both lists at the same time.
# ============================================================================ #

# --------------------------------- Libraries -------------------------------- #
library(tidyverse)
library(readxl)
library(lubridate)
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

# * Not gonna delete this code right now because it was hard
city_state_list <- paste(city_list$city, " (", state_initials[city_list$state], ")", sep = "")
# sex_by_city_male <- c()
# sex_by_city_female <- c()
# for (i in 1:length(city_state_list)) {
#   sex_by_city_male[i] <- sum(filter(age_sex_by_city,
#                                     city == city_state_list[i])$male)
#   sex_by_city_female[i] <- sum(filter(age_sex_by_city,
#                                     city == city_state_list[i])$female)
# }
# tail(data.frame("city" = city_state_list,
#                 "male" = sex_by_city_male,
#                 "female" = sex_by_city_female))
# nrow(data.frame("city" = city_state_list,
#                 "male" = sex_by_city_male,
#                 "female" = sex_by_city_female))
# sex_by_city <- data.frame("city" = city_state_list,
#                 "male" = sex_by_city_male,
#                 "female" = sex_by_city_female)
# write_csv(sex_by_city, file = "./UFTM-BioStat-DataBase/Background_Data/sex_by_city.csv")

sex_by_city <- read.csv("./UFTM-BioStat-DataBase/Background_Data/sex_by_city.csv")

sex <- c()
set.seed(seed)
for (i in 1:n) {
  sex[i] <- sample(x = c("M", "F"),
                   size = 1,
                   prob = c(filter(sex_by_city,
                                   city == city_state[i])$male,
                            filter(sex_by_city,
                                   city == city_state[i])$female))
}

# Using pre-computed values for the same reason
# age_by_sex <- read.csv("./UFTM-BioStat-DataBase/Background_Data/age_by_sex.csv")
# ! Not used anymore, delete both the code and the file later

age <- c()
set.seed(seed)
for (i in 1:n) {
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
                            size = 1,
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

# Race
race_list <- read_excel("./UFTM-BioStat-DataBase/Background_Data/race.xlsx")
possible_races <- c("White", "Black", "Asian", "Multiracial", "Indigenous")

race <- c()
set.seed(seed)
for (i in 1:n) {
  race[i] <- sample(x = possible_races,
                    size = 1,
                    prob = filter(race_list,
                                  city == city_state[i])[2:6])
}

# Birth date and zodiac sign
# This generator will skip february 29th for now
# * Commented code for possible later use
# load("./UFTM-BioStat-DataBase/Background_Data/births.RData")

study_date <- Sys.Date()
# possible_births <- substr(nascimento, 1, 4)
# possible_births <- possible_births[possible_births != "0000"]
# possible_births <- possible_births[possible_births != "2902"]

zodiac_sign_list <- list(
  "Áries" = dmy(21032022) %--% dmy(20042022),
  "Touro" = dmy(21042022) %--% dmy(20052022),
  "Gêmeos" = dmy(21052022) %--% dmy(20062022),
  "Câncer" = dmy(21062022) %--% dmy(22072022),
  "Leão" = dmy(23072022) %--% dmy(22082022),
  "Virgem" = dmy(23082022) %--% dmy(22092022),
  "Libra" = dmy(23092022) %--% dmy(22102022),
  "Escorpião" = dmy(23102022) %--% dmy(21112022),
  "Sagitário" = dmy(22112022) %--% dmy(21122022),
  "Aquário" = dmy(21012022) %--% dmy(19022022),
  "Peixes" = dmy(20022022) %--% dmy(20032022)
)

load("./UFTM-BioStat-DataBase/Background_Data/birth_frequency.RData")

# birth_frequency <- table(possible_births)

birth <- c()
zodiac_sign <- c()
set.seed(seed)
for (i in 1:n) {
  birth[i] <- sample(x = names(birth_frequency),
                     size = 1,
                     prob = birth_frequency)
}
birth <- as.Date(birth, format = "%d%m")
for (i in 1:n) {
  if (birth[i] %within% interval(dmy(21012022), dmy(21122022))) {
    for (j in 1:11) {
      if (birth[i] %within% zodiac_sign_list[j]) {
        zodiac_sign[i] <- names(zodiac_sign_list[j])
        break
      }
    }
  } else {
    zodiac_sign[i] <- "Capricórnio"
  }
  if (study_date >= birth[i]) {
    birth[i] <- birth[i] - years(age[i])
  } else {
    birth[i] <- birth[i] - years(age[i] + 1)
  }
}

# Chinese Horoscope
# TODO: update it to be more precise (not only year, but day and month)

chinese_zodiac_list <- c("Macaco",
                         "Galo",
                         "Cão",
                         "Javali",
                         "Rato",
                         "Boi",
                         "Tigre",
                         "Coelho",
                         "Dragão",
                         "Serpente",
                         "Cavalo",
                         "Cabra")

chinese_sign <- c()
set.seed(seed)
for (i in 1:n) {
  chinese_sign[i] <- chinese_zodiac_list[(year(birth[i]) %% 12) + 1]
}

# Height (cm)
boys_height_table <- read_excel("./UFTM-BioStat-DataBase/Background_Data/19_heights.xlsx", sheet = 1)
girls_height_table <- read_excel("./UFTM-BioStat-DataBase/Background_Data/19_heights.xlsx", sheet = 2)

male_height <- c("mean" = 170.8, "sd" = 6.425)
female_height <- c("mean" = 158, "sd" = 6)

height <- c()
set.seed(seed)
for (i in 1:n) {
  age_in_months <- floor((birth[i] %--% study_date) / months(1))
  if (sex[i] == "M") {
    if (age_in_months <= 228) {
      height[i] <- round(rnorm(1,
                               mean = filter(boys_height_table,
                                             month == age_in_months)$mean,
                               sd = filter(boys_height_table,
                                           month == age_in_months)$standard_deviation), 1)
    } else {
      height[i] <- round(rnorm(1,
                               mean = male_height["mean"],
                               sd = male_height["sd"]), 1)
    }
  } else {
    if (age_in_months <= 228) {
      height[i] <- round(rnorm(1,
                               mean = filter(girls_height_table,
                                             month == age_in_months)$mean,
                               sd = filter(girls_height_table,
                                           month == age_in_months)$standard_deviation), 1)
    } else {
      height[i] <- round(rnorm(1,
                               mean = female_height["mean"],
                               sd = female_height["sd"]), 1)
    }
  }
}

# Sport habits (which sport and time)
# Every 0, except when age doesn't make sense, was changed to 0.0005

sports_by_sex <- read_excel("./UFTM-BioStat-DataBase/Background_Data/sports.xlsx", sheet = 1)
sports_by_age <- read_excel("./UFTM-BioStat-DataBase/Background_Data/sports.xlsx", sheet = 2)
sports_time <- read_excel("./UFTM-BioStat-DataBase/Background_Data/sports.xlsx", sheet = 3)

age_separators <- c(7, 15, 20, 25, 35, 45, 55, 65, 75)
age_group <- c()
for (i in 1:n) {
  if (age[i] < age_separators[1]) {
    age_group[i] <- "0_6"
  } else if (age_separators[1] <= age[i] && age[i] < age_separators[2]) {
    age_group[i] <- "7_14"
  } else if (age_separators[2] <= age[i] && age[i] < age_separators[3]) {
    age_group[i] <- "15_19"
  } else if (age_separators[3] <= age[i] && age[i] < age_separators[4]) {
    age_group[i] <- "20_24"
  } else if (age_separators[4] <= age[i] && age[i] < age_separators[5]) {
    age_group[i] <- "25_34"
  } else if (age_separators[5] <= age[i] && age[i] < age_separators[6]) {
    age_group[i] <- "35_44"
  } else if (age_separators[6] <= age[i] && age[i] < age_separators[7]) {
    age_group[i] <- "45_54"
  } else if (age_separators[7] <= age[i] && age[i] < age_separators[8]) {
    age_group[i] <- "55_64"
  } else if (age_separators[8] <= age[i] && age[i] < age_separators[9]) {
    age_group[i] <- "65_74"
  } else if (age_separators[9] <= age[i]) {
    age_group[i] <- "75_above"
  }
}

activity <- c()
set.seed(seed)
for (i in 1:n) {
  activity[i] <- sample(x = sports_by_sex$sport,
                     size = 1,
                     prob = sports_by_sex[[sex[i]]] *
                            sports_by_age[[age_group[i]]])
}

activity_time <- c()
set.seed(seed)
for (i in 1:n) {
  if (activity[i] == "Sedentário") {
    activity_time[i] <- NA
  } else {
    activity_time[i] <- round(rchisq(n = 1,
                                     df = filter(sports_time, sport == activity[i])$degrees_of_freedom),
                              1)
  }
}

# Weight
# TODO: ADICIONAR LÓGICA PRA CRIANÇAS
