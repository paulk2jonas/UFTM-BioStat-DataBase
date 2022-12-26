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
#     Verify if the paths work elsewhere
#     Add some relation between private schools and college prevalence
#     Add relation between location and football team
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
if (!require(svDialogs)) {
  install.packages("svDialogs")
  library(svDialogs)
}


# ---------------------------------- Sources --------------------------------- #
source("./config.R")
source("./personal_info_engine.R")
source("./socio-economics_engine.R")
source("./health_engine.R")
source("./validation_functions.R")
source("./helper_functions.R")
source("./chance_models.R")

# --------------------------------- Variables -------------------------------- #
# n <- n_selector()
n <- 1000
# seed <- seed_selector()
seed <- 42
seed_list <- seed_generator(n, seed)
# language <- language_selector()
language <- "pt"
# minimum_wage <- wage_selector()
minimum_wage <- 121200
# generated_data <- data_selector()  * To be implemented on RStudio

# ---------------------------------------------------------------------------- #
#                           Data Loading and Managing                          #
# ---------------------------------------------------------------------------- #

# --------------------------------- Residence -------------------------------- #
city_list <- read_excel(
  "./Background_Data/cities.xlsx",
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

state_regions <- c(
  "Acre" = "Norte",
  "Alagoas" = "Nordeste",
  "Amapá" = "Norte",
  "Amazonas" = "Norte",
  "Bahia" = "Nordeste",
  "Ceará" = "Nordeste",
  "Espírito Santo" = "Sudeste",
  "Goiás" = "Centro-Oeste",
  "Maranhão" = "Nordeste",
  "Mato Grosso" = "Centro-Oeste",
  "Mato Grosso do Sul" = "Centro-Oeste",
  "Minas Gerais" = "Sudeste",
  "Pará" = "Norte",
  "Paraíba" = "Nordeste",
  "Paraná" = "Sul",
  "Pernambuco" = "Nordeste",
  "Piauí" = "Nordeste",
  "Rio de Janeiro" = "Sudeste",
  "Rio Grande do Norte" = "Nordeste",
  "Rio Grande do Sul" = "Sul",
  "Rondônia" = "Norte",
  "Roraima" = "Norte",
  "Santa Catarina" = "Sul",
  "São Paulo" = "Sudeste",
  "Sergipe" = "Nordeste",
  "Tocantins" = "Norte",
  "Distrito Federal" = "Centro-Oeste"
)

# ------------------------- Sex, Gender and Sexuality ------------------------ #
# Made by me
sex_by_city <- read.csv(
  "./Background_Data/sex_by_city.csv"
)

# ------------------------------- Age and Birth ------------------------------ #
study_date <- Sys.Date()

# Age
age_sex_by_city <- read_excel(
  "./Background_Data/age.xlsx"
)

age_range <- 0:99

# Birth
load("./Background_Data/birth_frequency.RData")

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
  "./Background_Data/first_name.csv"
) %>%
  select(1:4)

name_filter <- FALSE

# Last Name
surname_list <- read_excel(
  "./Background_Data/last_name.xlsx"
)

# ------------------------- Race, Hair and Eye Color ------------------------- #
race_list <- read_excel("./Background_Data/race.xlsx")
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
  "./Background_Data/19_heights.xlsx",
  sheet = 1
)
girls_height_table <- read_excel(
  "./Background_Data/19_heights.xlsx",
  sheet = 2
)

male_height <- c("mean" = 170.8, "sd" = 6.425)
female_height <- c("mean" = 158, "sd" = 6)

# ---------------------- Sports and Physical Activities ---------------------- #
# Sports
# Every 0, except when age doesn't make sense, was changed to 0.0005
sports_by_sex <- read_excel(
  "./Background_Data/sports.xlsx",
  sheet = 1
)
sports_by_age <- read_excel(
  "./Background_Data/sports.xlsx",
  sheet = 2
)
# Weekly sports time (h)
sports_time <- read_excel(
  "./Background_Data/sports.xlsx",
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
  "./Background_Data/weight_by_age.xlsx",
  sheet = 1
)
girls_weight <- read_excel(
  "./Background_Data/weight_by_age.xlsx",
  sheet = 2
)

# Gonna keep these values because it worked good enough
male_weight <- c("mean" = 50, "sd" = 4.5)  # 74.6, 24.5 -> 50, 14.5
female_weight <- c("mean" = 43.6, "sd" = 3.1)  # 65.1, 23.1 -> 43.6, 13.1

# BF%
male_bf <- read_excel(
  "./Background_Data/body_fat_percent.xlsx",
  sheet = 1
)
female_bf <- read_excel(
  "./Background_Data/body_fat_percent.xlsx",
  sheet = 2
)

bf_age_groups <- list(
  "20_29" = 20:29,
  "30_39" = 30:39,
  "40_49" = 40:49,
  "50_59" = 50:59,
  "60_more" = 60:100
)

# ! BF% and MM% will be implemented after I fix the weight values,

# --------------------------------- Fractures -------------------------------- #
fracture_list <- read_excel(
  "./Background_Data/fractures.xlsx"
)

# ---------------------------- Residence Situation --------------------------- #
situation_list <- read_excel(
  "./Background_Data/housing_situation_by_state.xlsx"
)

# ----------------------- Alphabetization and Schooling ---------------------- #
# Alphabetization
reading_list <- read_excel(
  "./Background_Data/reading_by_age_situation_state.xlsx"
)

# Schooling
age_14_24_sex <- read_excel(
  "./Background_Data/instruction_level.xlsx",
  sheet = "14_sex"
)
age_14_24_race <- read_excel(
  "./Background_Data/instruction_level.xlsx",
  sheet = "14_race"
)
age_25_sex <- read_excel(
  "./Background_Data/instruction_level.xlsx",
  sheet = "25_sex"
)
age_25_race <- read_excel(
  "./Background_Data/instruction_level.xlsx",
  sheet = "25_race"
)

instruction_level <- c(
  "Sem instrução",
  "Ensino fundamental incompleto",
  "Ensino fundamental completo",
  "Ensino médio incompleto",
  "Ensino médio completo",
  "Ensino superior incompleto",
  "Ensino superior completo"
)

race_translation <- c(
  "Branco" = "white",
  "Pardo" = "black_multiracial",
  "Amarelo" = "white",
  "Preto" = "black_multiracial",
  "Indígena" = "black_multiracial"
)

# --------------------------- Occupation and Income -------------------------- #
city_codes <- read_excel(
  "./Background_Data/city_codes.xls"
)
load("./Background_Data/RAIS_data/AC2016.Rda")
load("./Background_Data/RAIS_data/AL2016.Rda")
load("./Background_Data/RAIS_data/AM2016.Rda")
load("./Background_Data/RAIS_data/AP2016.Rda")
load("./Background_Data/RAIS_data/BA2016.Rda")
load("./Background_Data/RAIS_data/CE2016.Rda")
load("./Background_Data/RAIS_data/DF2016.Rda")
load("./Background_Data/RAIS_data/ES2016.Rda")
load("./Background_Data/RAIS_data/GO2016.Rda")
load("./Background_Data/RAIS_data/MA2016.Rda")
load("./Background_Data/RAIS_data/MG2016.Rda")
load("./Background_Data/RAIS_data/MS2016.Rda")
load("./Background_Data/RAIS_data/MT2016.Rda")
load("./Background_Data/RAIS_data/PA2016.Rda")
load("./Background_Data/RAIS_data/PB2016.Rda")
load("./Background_Data/RAIS_data/PE2016.Rda")
load("./Background_Data/RAIS_data/PI2016.Rda")
load("./Background_Data/RAIS_data/PR2016.Rda")
load("./Background_Data/RAIS_data/RJ2016.Rda")
load("./Background_Data/RAIS_data/RN2016.Rda")
load("./Background_Data/RAIS_data/RO2016.Rda")
load("./Background_Data/RAIS_data/RR2016.Rda")
load("./Background_Data/RAIS_data/RS2016.Rda")
load("./Background_Data/RAIS_data/SC2016.Rda")
load("./Background_Data/RAIS_data/SE2016.Rda")
load("./Background_Data/RAIS_data/SP2016.Rda")
load("./Background_Data/RAIS_data/TO2016.Rda")

unemployment_rate <- c(
  "Norte" = .089,
  "Nordeste" = .127,
  "Sudeste" = .093,
  "Sul" = .056,
  "Centro-Oeste" = .07
)

occupation_age_groups <- list(
  "1" = 14,
  "2" = 15:17,
  "3" = 18:24,
  "4" = 25:29,
  "5" = 30:39,
  "6" = 40:49,
  "7" = 50:64,
  "8" = 65:99
)

# Maybe this should be a list
occupation_database <- list(
  "Acre" = AC2016,
  "Alagoas" = AL2016,
  "Amapá" = AP2016,
  "Amazonas" = AM2016,
  "Bahia" = BA2016,
  "Ceará" = CE2016,
  "Espírito Santo" = ES2016,
  "Goiás" = GO2016,
  "Maranhão" = MA2016,
  "Mato Grosso" = MT2016,
  "Mato Grosso do Sul" = MS2016,
  "Minas Gerais" = MG2016,
  "Pará" = PA2016,
  "Paraíba" = PB2016,
  "Paraná" = PR2016,
  "Pernambuco" = PE2016,
  "Piauí" = PI2016,
  "Rio de Janeiro" = RJ2016,
  "Rio Grande do Norte" = RN2016,
  "Rio Grande do Sul" = RS2016,
  "Rondônia" = RO2016,
  "Roraima" = RR2016,
  "Santa Catarina" = SC2016,
  "São Paulo" = SP2016,
  "Sergipe" = SE2016,
  "Tocantins" = TO2016,
  "Distrito Federal" = DF2016
)

sex_codes <- c("M" = 1, "F" = 2)

race_codes <- c(
  "Indígena" = 1,
  "Branco" = 2,
  "Preto" = 4,
  "Amarelo" = 6,
  "Pardo" = 8
)

# Using the proportion of 7% (housewives) over the simple mean
# of unemplyment rates (7/((8.9+12.7+9.3+5.6+7)/5)) = .8
house_chance <- .8
nothing_chance <- 1 - house_chance

# ------------------------- Internet Access and Speed ------------------------ #
internet_access_chances <- c("Urbana" = .9, "Rural" = .747)

internet_classes <- list(
  "Até 1 Mbps" = c(.01, 1),
  "1-2" = c(1.01, 2),
  "2-5" = c(2.01, 5),
  "5-10" = c(5.01, 10),
  "10-30" = c(10.01, 30),
  "30-50" = c(30.01, 50),
  "50-100" = c(50.01, 100),
  "100-150" = c(100.01, 150),
  "150-200" = c(150.01, 200),
  "200-300" = c(200.01, 300)
)

# ----------------------------- Health Insurance ----------------------------- #
health_insurance_chances <- c(
  "Norte" = .0937,
  "Nordeste" = .117,
  "Centro-Oeste" = .199,
  "Sudeste" = .3264,
  "Sul" = .2325
)

# ------------------------------ Private School ------------------------------ #
private_public_schools <- read_excel(
  "./Background_Data/private_public_schools.xlsx"
)
load("./Background_Data/school_data.RData")

school_age_groups <- list(
  "6 a 10" = 6:10,
  "11 a 14" = 11:14,
  "15 a 17" = 15:17,
  "18 ou mais" = 18:99
)

column_selectors <- list(
  "F" = "QT_MAT_BAS_FEM",
  "M" = "QT_MAT_BAS_MASC",
  "Branco" = "QT_MAT_BAS_BRANCA",
  "Preto" = "QT_MAT_BAS_PRETA",
  "Pardo" = "QT_MAT_BAS_PARDA",
  "Amarelo" = "QT_MAT_BAS_AMARELA",
  "Indígena" = "QT_MAT_BAS_INDIGENA",
  "6 a 10" = "QT_MAT_BAS_6_10",
  "11 a 14" = "QT_MAT_BAS_11_14",
  "15 a 17" = "QT_MAT_BAS_15_17",
  "18 ou mais" = "QT_MAT_BAS_18_MAIS"
)

# -------------------------------- Blood Type -------------------------------- #
# ABO
abo_distribution <- c(A = .42, B = .1, AB = .03, O = .45)

# Rh
rh_distribution <- c(pos = .805, neg = .195)

# Personality traits
blood_type_personality_traits <- list(
  "A" = c(
    "Organizado",
    "Meticuloso",
    "Tímido",
    "Sensível",
    "Paciente",
    "Ansioso",
    "Intenso",
    "Reservado",
    "Educado",
    "Atencioso",
    "Responsável",
    "Perfeccionista",
    "Bondoso",
    "Consciente"
  ),
  "B" = c(
    "Criativo",
    "Curioso",
    "Forte",
    "Aventureiro",
    "Pragmático",
    "Animado",
    "Disposto",
    "Proativo",
    "Inconvencional",
    "Jovial"
  ),
  "AB" = c(
    "Dual",
    "Reflexivo",
    "Talentoso",
    "Racional",
    "Controlado",
    "Espiritual",
    "Centrado",
    "Artístico",
    "Misterioso",
    "Diplomata",
    "Confiável",
    "Sociável"
  ),
  "O" = c(
    "Otimista",
    "Flexível",
    "Confiante",
    "Determinado",
    "Energético",
    "Calmo",
    "Social",
    "Leal",
    "Líder",
    "Intuitivo",
    "Trabalhador",
    "Independente",
    "Centrado",
    "Devotado"
  )
)

# -------------------------- Prefered Football Club -------------------------- #
# TODO: Add city/state preference later
football_clubs <- read_excel("./Background_Data/football_clubs.xlsx")


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
# Weight (kg)
weight <- mapply(weight_generator, sex, birth, activity_time, seed_list)

# BMI
bmi <- mapply(bmi_calc, weight, height)

# BF%
bf_percent <- mapply(bf_percent_generator, age, bmi, sex, seed_list)

# ! MM% will be implemented when I have time to get data.

# --------------------------------- Fractures -------------------------------- #
fracture <- mapply(fracture_generator, age, sex, seed_list)
fracture <- unlist(fracture)

# ---------------------------- Residence Situation --------------------------- #
situation <- mapply(situation_generator, state, race, seed_list)

# ----------------------- Alphabetization and Schooling ---------------------- #
# Alphabetization
reading <- mapply(reading_generator, state, situation, age, race)

# Schooling
schooling <- mapply(
  generate_schooling_level,
  age,
  reading,
  state,
  sex,
  race,
  seed_list
)

# Work and Income
employment <- mapply(employment_generator, age, state, seed_list)
occupation_data <- mapply(
  occupation_data_generator,
  age,
  state,
  city,
  race,
  employment,
  seed_list
)
occupation <- unlist(occupation_data[1, ])
income_minimum_wage <- unlist(occupation_data[2, ])
income <- unlist(occupation_data[2, ]) * minimum_wage

# ------------------------- Internet Access and Speed ------------------------ #
has_internet <- mapply(has_internet_access, situation, seed_list)
internet_access <- mapply(internet_access_generator, has_internet, seed_list)
# TODO: tie the access chance to income

# ----------------------------- Health Insurance ----------------------------- #
health_insurance <- mapply(health_insurance_generator, state, seed_list)

# ! Have to change the way I deal with income
# ! the way it is now, children can never have healthy insurance

# ------------------------------ Private School ------------------------------ #
school_type <- mapply(
  generate_school_type,
  age,
  reading,
  state,
  city,
  sex,
  race,
  income_minimum_wage,
  seed_list
)

# -------------------------------- Blood Type -------------------------------- #
# ABO
abo_type <- generate_abo_blood_type(abo_distribution, n)

# Rh
rh_type <- generate_rh_blood_type(rh_distribution, n)

# Blood type
blood_type <- paste(abo_type, rh_type, sep = " ")

# Personality traits
personality <- mapply(generate_abo_personality, abo_type)

# -------------------------- Prefered Football Club -------------------------- #
football_team <- mapply(generate_football_team, age, sex)
