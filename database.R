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
#     Get a new standard deviation for adults, and create a impacting score from sports
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
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}
if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}

# ---------------------------------- Sources --------------------------------- #
source("./UFTM-BioStat-DataBase/personal_info_engine.R")

# --------------------------------- Variables -------------------------------- #
n <- 1000
seed <- 42
seed_list <- seed_generator(n, seed)
language <- "pt"


# ---------------------------------------------------------------------------- #
#                           Data Loading and Managing                          #
# ---------------------------------------------------------------------------- #

# --------------------------------- Residence -------------------------------- #
city_list <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/cities.xlsx",
  col_types = c("text", "text", "numeric")
)
# To save on processing power later on
city_list_total_pop <- sum(city_list$population)

state_initials <- c(
  "Acre" = "AC",
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
  "Distrito Federal" = "DF"
)

# ------------------------- Sex, Gender and Sexuality ------------------------ #
# Made by me
sex_by_city <- read.csv(
  "./UFTM-BioStat-DataBase/Background_Data/sex_by_city.csv"
)

# ------------------------------- Age and Birth ------------------------------ #
study_date <- Sys.Date()

# Age
age_sex_by_city <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/age.xlsx"
)

age_range <- 0:99

# Birth
load("./UFTM-BioStat-DataBase/Background_Data/birth_frequency.RData")

# Zodiac Sign
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

# Chinese sign
chinese_sign_list <- c(
  "Macaco",
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
  "Cabra"
)

# ----------------------------------- Name ----------------------------------- #
# First Name
forename_list <- read.csv(
  "./UFTM-BioStat-DataBase/Background_Data/first_name.csv"
) %>%
  select(1:4)

name_filter <- FALSE

# Last Name
surname_list <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/last_name.xlsx"
)

# ------------------------- Race, Hair and Eye Color ------------------------- #
race_list <- read_excel("./UFTM-BioStat-DataBase/Background_Data/race.xlsx")
if (language == "pt") {
  possible_races <- c("Branco", "Preto", "Amarelo", "Pardo",  "Indígena")
} else if (language == "en") {
  possible_races <- c("White", "Black", "Asian", "Multiracial", "Indigenous")
}

hair_color_list <- c(
  "Loiro comum",
  "Castanho claro",
  "Castanho médio",
  "Castanho escuro",
  "Castanho-ruivo",
  "Ruivo",
  "Preto",
  "Grisalho",
  "Branco"
)

eye_color_list <- c("Castanho", "Azul", "Âmbar", "Verde")
eye_color_distribution <- c(45, 27, 18, 9)

# -------------------------------- Height (cm) ------------------------------- #
boys_height_table <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/19_heights.xlsx",
  sheet = 1
)
girls_height_table <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/19_heights.xlsx",
  sheet = 2
)

male_height <- c("mean" = 170.8, "sd" = 6.425)
female_height <- c("mean" = 158, "sd" = 6)

# ---------------------- Sports and Physical Activities ---------------------- #
# Sports
# Every 0, except when age doesn't make sense, was changed to 0.0005
sports_by_sex <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/sports.xlsx",
  sheet = 1
)
sports_by_age <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/sports.xlsx",
  sheet = 2
)
# Weekly sports time (h)
sports_time <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/sports.xlsx",
  sheet = 3
)

age_groups <- list(
  "0_6" = 0:6,
  "7_14" = 7:14,
  "15_19" = 15:19,
  "20_24" = 20:24,
  "25_34" = 25:34,
  "35_44" = 35:44,
  "45_54" = 45:54,
  "55_64" = 55:64,
  "65_74" = 65:74,
  "75_above" = 75:100
)

# ------------------------- Weight, BMI, BF% and MM% ------------------------- #
# Weight (kg)
boys_weight <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/weight_by_age.xlsx",
  sheet = 1
)
girls_weight <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/weight_by_age.xlsx",
  sheet = 2
)

male_weight <- c("mean" = 74.6, "sd" = 24.5)
female_weight <- c("mean" = 65.1, "sd" = 23.1)

# ! BF% and MM% will be implemented after I fix the weight values,

# --------------------------------- Fractures -------------------------------- #
fracture_list <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/fractures.xlsx"
)

# ---------------------------- Residence Situation --------------------------- #
situation_list <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/housing_situation_by_state.xlsx"
)

# ----------------------- Alphabetization and Schooling ---------------------- #
# Alphabetization
reading_list <- read_excel(
  "./UFTM-BioStat-DataBase/Background_Data/reading_by_age_situation_state.xlsx"
)


# ---------------------------------------------------------------------------- #
#                                Data Generation                               #
# ---------------------------------------------------------------------------- #

# --------------------------------- Residence -------------------------------- #
city <- city_generator(n, seed, city_list)
state <- state_generator(n, seed, city_list)

city_state <- paste(city, " (", state_initials[state], ")", sep = "")

# ------------------------- Sex, Gender and Sexuality ------------------------ #
# Sex
sex <- mapply(sex_generator, seed_list, city_state)

# ------------------------------- Age and Birth ------------------------------ #
# Age
# This generator will skip february 29th for now
age <- mapply(age_generator, seed_list, city_state, sex)

birth <- mapply(birthday_generator, seed_list, SIMPLIFY = FALSE)
birth <- reduce(birth, c)

# Zodiac Sign
zodiac_sign <- mapply(zodiac_sign_generator, birth)

# Birth
# Remaking it because it seems I need to
# Maybe I'll find another way later
birth <- mapply(birth_date_generator, birth, age, SIMPLIFY = FALSE)
birth <- reduce(birth, c)

# Chinese Horoscope
# TODO: update it to be more precise (not only year, but day and month)
chinese_sign <- chinese_sign_generator(birth)

# ----------------------------------- Name ----------------------------------- #
# First Name
if (name_filter) {
  first_name <- mapply(filter_name_generator, seed_list, sex)
} else {
  first_name <- mapply(unfilter_name_generator, seed_list, sex)
}
# Last Name
last_name <- mapply(last_name_generator, seed_list)
# Full Name
full_name <- paste(first_name, last_name, sep = " ")

# ------------------------- Race, Hair and Eye Color ------------------------- #
# Race
race <- mapply(race_generator, seed_list, city_state)
# Hair Color
hair_color <- mapply(hair_color_generator, race, age, seed_list)
# Eye Color
eye_color <- eye_color_generator(seed, eye_color_list, eye_color_distribution)

# -------------------------------- Height (cm) ------------------------------- #
height <- mapply(height_generator, seed_list, birth, study_date, sex)

# ---------------------- Sports and Physical Activities ---------------------- #
# Sports
activity <- mapply(activity_generator, age, sex, seed_list)
# Weekly sports time (h)
activity_time <- mapply(activity_time_generator, activity, seed_list)

# ------------------------- Weight, BMI, BF% and MM% ------------------------- #
# ! Still need to tune the values on the funcion. Too many big weights
# Weight (kg)
weight <- mapply(weight_generator, sex, birth, activity_time, seed_list)

# BMI
bmi <- mapply(bmi_calc, weight, height)

# ! BF% and MM% will be implemented after I fix the weight values

# --------------------------------- Fractures -------------------------------- #
fracture <- mapply(fracture_generator, age, sex, seed_list)
fracture <- unlist(fracture)

# ---------------------------- Residence Situation --------------------------- #
situation <- mapply(situation_generator, state, race, seed_list)

# ----------------------- Alphabetization and Schooling ---------------------- #
# Alphabetization
reading <- mapply(reading_generator, state, situation, age, race)
# TODO: add schooling levels, because I FORGOT TO GET THIS DATA

# Income
