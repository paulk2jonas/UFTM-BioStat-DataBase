
# ---------------------------- Seed List Generator --------------------------- #
# ! Move to another package, where it makes sense
seed_generator <- function(n, seed) {
  set.seed(seed)
  seed_list <- sample(
    x = -(n ** 2):(n ** 2),
    size = n
  )

  return(seed_list)
}

# --------------------------------- Residence -------------------------------- #
# City
residence_index <- function(n, seed, city_list) {
  set.seed(seed)
  index <- sample(
    # seq_len is used to avoid edge case and make the lint happy
    x = seq_len(nrow(city_list)),
    size = n,
    replace = TRUE,
    prob = city_list$population
  )
  return(index)
}

city_generator <- function(n, seed, city_list) {
  index <- residence_index(n, seed, city_list)
  city <- city_list$city[index]
  return(city)
}

# State
state_generator <- function(n, seed, city_list) {
  index <- residence_index(n, seed, city_list)
  state <- city_list$state[index]
  return(state)
}

# ------------------------- Sex, Gender and Sexuality ------------------------ #
# Sex
sex_generator <- function(seed_list, city_state) {
  prob <- filter(sex_by_city, city == city_state) %>%
    select(2, 3)
  set.seed(seed_list)
  sex <- sample(
    x = c("M", "F"),
    size = 1,
    prob = prob
  )
  return(sex)
}

# ------------------------------- Age and Birth ------------------------------ #
# Age
age_generator <- function(seed_list, city_state, sex) {
  if (sex == "M") {
    column <- "male"
  } else if (sex == "F") {
    column <- "female"
  }
  prob <- filter(age_sex_by_city, city == city_state) %>%
    pull(all_of(column))
    # select(all_of(column))
  # prob <- prob[[1]]  # single column df needs this
  set.seed(seed_list)
  age <- sample(
    x = age_range,
    size = 1,
    prob = prob
  )
  return(age)
}

# Birth
birthday_generator <- function(seed_list) {
  # print(names(birth_frequency))
  set.seed(seed_list)
  birth <- sample(
    x = names(birth_frequency),
    size = 1,
    prob = birth_frequency
  )
  birth <- as.Date(birth, format = "%d%m")
  # print(birth)
  # print(class(birth))
  return(birth)
}

birth_date_generator <- function(birth, age) {
  if (birth <= study_date) {
    birth <- birth - years(age)
  } else {
    birth <- birth - years(age + 1)
  }
  return(birth)
}

# Zodiac Sign
zodiac_sign_generator <- function(birth) {
  if (birth %within% interval(dmy(21012022), dmy(21122022))) {
    #? Is there a way to not use a for? Is this other way faster?
    for (i in 1:11) {
      if (birth %within% zodiac_sign_list[i]) {
        zodiac_sign <- names(zodiac_sign_list[i])
        return(zodiac_sign)  #? Should this be a return or a break?
      }
    }
  } else {
    zodiac_sign <- "Capricórnio"
    return(zodiac_sign)
  }
}

# Chinese Sign
chinese_sign_generator <- function(birth) {
  index <- (year(birth) %% 12) + 1
  chinese_sign <- chinese_sign_list[index]
  return(chinese_sign)
}

# ----------------------------------- Name ----------------------------------- #
# First Name (filtered by sex classification)
filter_name_generator <- function(seed_list, sex) {
  if (sex == "M") {
    column <- "frequency_male"
  } else if (sex == "F") {
    column <- "frequency_female"
  }
  name <- filter(forename_list, classification == sex) %>%
    pull(name)
    # select(name)
  # name <- name[[1]]  # single column df needs this
  prob <- filter(forename_list, classification == sex) %>%
    pull(all_of(column))
    # select(all_of(column))
  # prob <- prob[[1]]  # single column df needs this
  set.seed(seed_list)
  first_name <- sample(
    x = name,
    size = 1,
    prob = prob
  )
  first_name <- unname(first_name)
  first_name <- str_to_title(first_name)
  return(first_name)
}

# First Name (not filtered by sex classification)
unfilter_name_generator <- function(seed_list, sex) {
  if (sex == "M") {
    column <- "frequency_male"
  } else if (sex == "F") {
    column <- "frequency_female"
  }
  name <- pull(forename_list, name)
  # name <- select(forename_list, name)
  # name <- name[[1]]  # single column df needs this
  prob <- pull(forename_list, all_of(column))
  # prob <- select(forename_list, all_of(column))
  # prob <- prob[[1]]  # single column df needs this
  set.seed(seed_list)
  first_name <- sample(
    x = name,
    size = 1,
    prob = prob
  )
  first_name <- unname(first_name)
  first_name <- str_to_title(first_name)
  return(first_name)
}

# * Didn't work, but still a reasonable idea
# ? Why didn't this work? What was its point?
# first_name_generator <- function(filter, seed_list, sex) {
#   if (filter) {
#     return(filter_name_generator(seed_list, sex))
#   } else {
#     return(unfilter_name_generator(seed_list, sex))
#   }
# }

# Last name
last_name_generator <- function(seed_list) {
  surname <- pull(surname_list, surname)
  # surname <- select(surname_list, surname)
  # surname <- surname[[1]]
  prob <- pull(surname_list, incidence)
  # prob <- select(surname_list, incidence)
  # prob <- prob[[1]]
  set.seed(seed_list)
  last_name <- sample(
    x = surname,
    size = 1,
    prob = prob
  )
  return(last_name)
}

# ------------------------- Race, Hair and Eye Color ------------------------- #
# Race
race_generator <- function(seed_list, city_state) {
  prob <- filter(race_list, city == city_state) %>%
    select(2:6)
  set.seed(seed_list)
  race <- sample(
    x = possible_races,
    size = 1,
    prob = prob
  )
  return(race)
}

# Hair Color
hair_color_generator <- function(race, age, seed_list) {
  if (race == possible_races[1]) {
    race_hair_chance <- c(rep(1, 9))
  } else if (race == possible_races[2]) {
    race_hair_chance <- c(.5, 1, 1, 1, .5, .5, 1, 1, 1)
  } else if (race == possible_races[3]) {
    race_hair_chance <- c(.2, .2, .2, .5, .2, .2, 1, 1, 1)
  } else if (race == possible_races[4]) {
    race_hair_chance <- c(.8, 1, 1, 1, .8, .8, 1, 1, 1)
  } else if (race == possible_races[5]) {
    race_hair_chance <- c(.2, .2, .2, .5, .2, .2, 1, 1, 1)
  }
  if (age < 20) {
    age_hair_chance <- c(rep(1, 7), rep(0, 2))
  } else if (20 <= age && age < 50) {
    color_chance <- (-5 * age + 400) / 3
    white_chance <- (5 * age - 5) / 3
    age_hair_chance <- c(rep(color_chance, 7), rep(white_chance, 2))
  } else if (50 <= age && age < 75) {
    color_chance <- -2 * age + 150
    white_chance <- 2 * age - 50
    age_hair_chance <- c(rep(color_chance, 7), rep(white_chance, 2))
  } else if (75 <= age) {
    age_hair_chance <- c(rep(0, 7), rep(1, 2))
  }
  prob <- race_hair_chance * age_hair_chance
  set.seed(seed_list)
  hair_color <- sample(
    x = hair_color_list,
    size = 1,
    prob = prob
  )
  hair_color <- unname(hair_color)  # It's not working for some reason...
  return(hair_color)
}

# Eye Color
eye_color_generator <- function(seed, eye_color_list, eye_color_distribution) {
  set.seed(seed)
  eye_color <- sample(
    x = eye_color_list,
    size = n,
    replace = TRUE,
    prob = eye_color_distribution
  )
  return(eye_color)
}

# -------------------------------- Height (cm) ------------------------------- #
height_generator <- function(seed_list, birth, study_date, sex) {
  age_in_months <- floor((birth %--% study_date) / months(1))
  # TODO: Change all these to the helper function
  if (sex == "M" && age_in_months <= 228) {
    mean <- filter(boys_height_table, month == age_in_months) %>%
      pull("mean")
    sd <- filter(boys_height_table, month == age_in_months) %>%
      pull("standard_deviation")
  } else if (sex == "F" && age_in_months <= 228) {
    mean <- filter(girls_height_table, month == age_in_months) %>%
      pull("mean")
    sd <- filter(girls_height_table, month == age_in_months) %>%
      pull("standard_deviation")
  } else if (sex == "M" && age_in_months > 228) {
    mean <- male_height["mean"]
    sd <- male_height["sd"]
  } else if (sex == "F" && age_in_months > 228) {
    mean <- female_height["mean"]
    sd <- female_height["sd"]
  }
  set.seed(seed_list)
  height <- rnorm(n = 1, mean = mean, sd = sd)
  height <- round(height, 1)
  return(height)
}

# ---------------------- Sports and Physical Activities ---------------------- #
# Sports
activity_generator <- function(age, sex, seed_list) {
  # print(age_groups)
  # print(names(age_groups))
  for (group in seq_along(age_groups)) {
    # print(age_groups[[group]])
    # print(age %in% age_groups[[group]])
    if (age %in% age_groups[[group]]) {
      # print(names(age_groups[group]))
      age_group <- names(age_groups[group])
    }
    # if (age %in% age_groups[[group]]) {
    #   print(names(age_groups[[group]]))
    #   age_group <- names(age_groups[[group]])
    #   print(age_group)
    # }
  }
  sports <- pull(sports_by_sex, sport)
  sex_prob <- pull(sports_by_sex, sex)
  age_prob <- pull(sports_by_age, age_group)
  prob <- sex_prob * age_prob
  set.seed(seed_list)
  activity <- sample(
    x = sports,
    size = 1,
    prob = prob
  )
  return(activity)
}

# Weekly sports time (h)
activity_time_generator <- function(activity, seed_list) {
  df <- filter(sports_time, sport == activity) %>%
    pull(degrees_of_freedom)
  if (activity == "Sedentário") {
    activity_time <- NA
  } else {
    set.seed(seed_list)
    activity_time <- rchisq(n = 1, df = df)
    activity_time <- round(activity_time, 1)
  }
  activity_time <- unname(activity_time)  # It's not working for some reason...
  return(activity_time)
}

# ------------------------- Weight, BMI, BF% and MM% ------------------------- #
# Weight (kg)
weight_generator <- function(sex, birth, activity_time, seed_list) {
  age_in_months <- floor((birth %--% study_date) / months(1))
  add <- function(mean, sd, weight) {
    add <- mean + ((weight - 3) * sd)  # 1 -> 3
    return(add)
  }
  if (sex == "M" && age_in_months <= 240) {
    mean <- filter(boys_weight, month == age_in_months) %>%
      pull(mean)
    sd <- filter(boys_weight, month == age_in_months) %>%
      pull(standard_deviation)
  } else if (sex == "F" && age_in_months <= 240) {
    mean <- filter(girls_weight, month == age_in_months) %>%
      pull(mean)
    sd <- filter(girls_weight, month == age_in_months) %>%
      pull(standard_deviation)
  } else if (sex == "M" && age_in_months > 240) {
    mean <- male_weight["mean"]
    sd <- male_weight["sd"]
  } else if (sex == "F" && age_in_months > 240) {
    mean <- female_weight["mean"]
    sd <- female_weight["sd"]
  }

  set.seed(seed_list)
  if (age_in_months <= 240) {
    weight <- rnorm(n = 1, mean = mean, sd = sd)
  } else if (age_in_months > 240) {
    weight <- rchisq(n = 1, df = 5)  # 3 -> 5
    weight <- weight + add(mean, sd, weight)
  }

  if (!is.na(activity_time)) {
    weight <- weight - activity_time / 2
  }

  weight <- round(weight, 2)
  weight <- unname(weight)  # It's not working for some reason...
  return(weight)
}

# BMI
bmi_calc <- function(weight, height) {
  bmi <- weight / (height / 100) ** 2
  bmi <- round(bmi, 2)
  return(bmi)
}

# BF%
bf_percent_generator <- function(age, bmi, sex, seed_list) {
  if (age < 20) {
    return(NA)
  }
  if (bmi < 18.5) {
    bmi_class <- "Underweight"
  } else if (bmi < 25) {
    bmi_class <- "Healthy"
  } else if (bmi < 30) {
    bmi_class <- "Overweight"
  } else if (bmi < 35) {
    bmi_class <- "Obesity Class I"
  } else if (bmi < 40) {
    bmi_class <- "Obesity Class II"
  } else {
    bmi_class <- "Obesity Class III"
  }
  if (sex == "M") {
    database <- male_bf %>%
      filter(bmi == bmi_class)
  } else if (sex == "F") {
    database <- female_bf %>%
      filter(bmi == bmi_class)
  }
  for (group in seq_along(bf_age_groups)) {
    if (age %in% bf_age_groups[[group]]) {
      age_group <- names(bf_age_groups[group])
    }
  }
  min_column <- paste(age_group, "min", sep = "_")
  max_column <- paste(age_group, "max", sep = "_")
  min <- pull(database, min_column)
  max <- pull(database, max_column)
  bf_list <- seq(from = min, to = max, by = .05)
  set.seed(seed_list)
  bf <- sample(
    x = bf_list,
    size = 1,
    prob = dnorm(
      x = bf_list,
      mean = mean(bf_list),
      sd = (max - min) / 6
    )
  )
  bf <- round(bf, 1)
  return(bf)
}

# BF% -> I should later update the obesity upper limits
# Probably using ideal IMC as a base and enverythin more is fat
# I think that the values are coming up a little low... but good enough for now

# ! MM% will be implemented when I have time to get data.

# --------------------------------- Fractures -------------------------------- #
fracture_generator <- function(age, sex, seed_list) {
  if (age <= 4) {
    column <- age + 2
  } else if (age <= 12) {
    column <- 7
  } else if (age <= 17 && sex == "M") {
    column <- 8
  } else if (age <= 17 && sex == "F") {
    column <- 9
  } else if (age > 17 && sex == "M") {
    column <- 10
  } else if (age > 17 && sex == "F") {
    column <- 11
  }

  # ! make this into a function
  # prob <- c(
  #   fracture_list[row, column],
  #   1 - fracture_list[row, column]
  # )
  fractures <- c()
  set.seed(seed_list)
  for (row in seq_len(nrow(fracture_list))) {
    fractures[row] <- sample(
      x = c(TRUE, FALSE),
      size = 1,
      prob = c(
        fracture_list[row, column],
        1 - fracture_list[row, column]
      )
    )
  }

  # places <- fracture_list[[1]]
  if (all(!fractures)) {
    fracture <- NA
  } else {
  places <- pull(fracture_list, place)
  fracture <- paste(places[fractures], sep = ", ")
  }
  # print(fracture == [[]])
  # fracture <- unlist(fracture)
  # if (fracture == []) fracture <- NA
  return(fracture)
}

# ---------------------------- Residence Situation --------------------------- #
situation_generator <- function(state, race, seed_list) {
  person_state <- state  # ! because it was messing the filter
  prob <- filter(situation_list, state == person_state) %>%
    pull(str_to_lower(race))
  # situation_chances <- filter(situation_list, state == person_state) %>%
  #   select(str_to_lower(race))
  set.seed(seed_list)
  situation <- sample(
    x = c("Urbana", "Rural"),
    size = 1,
    prob = prob
  )
  # situation_chances[[1]]
  situation <- unname(situation)  # Also not working...
  return(situation)
}

# ----------------------- Alphabetization and Schooling ---------------------- #
# Alphabetization
reading_generator <- function(state, situation, age, race) {
  # utils::globalVariables(reading_list)  # testing
  person_state <- state
  person_situation <- situation
  person_age <- age
  if (person_age < 5) return("N")
  if (race == "Branco") {
    columns <- 4:5
  } else if (race == "Preto") {
    columns  <- 6:7
  } else if (race == "Amarelo") {
    columns <- 8:9
  } else if (race == "Pardo") {
    columns <- 10:11
  } else if (race == "Indígena") {
    columns <- 12:13
  }
  # validation_test <- tibble(read = 0, dont_read = 0)
  # I'll use the first age with non 0 value for now
  while (TRUE) {
    prob <- filter(reading_list, state == person_state) %>%
      filter(situation == person_situation) %>%
        filter(age == person_age) %>%
          select(all_of(columns))
    if (prob[1] == 0 && prob[2] == 0) {
      person_age <- person_age - 1
    } else {
      break
    }
  }
  reading <- sample(
    x = c("S", "N"),
    size = 1,
    prob = prob
  )
  return(reading)
}

generate_schooling_level <- function(
  age,
  reading,
  state,
  sex,
  race,
  seed_list
) {
  # valid_age <- validate_schooling_level(age)
  # if (!valid_age) {
  #   return(ifelse(age >= 6, "Ensino fundamental incompleto", NA))
  #   if (age >= 6) {

  #   }
  # }
  if (!validate_schooling_level(age, reading)) {
    if (age < 6) return(NA)
    if (reading == "S") return("Ensino fundamental incompleto")
    return("Sem instrução")
  }

  if (reading == "N") return("Sem instrução")

  person_state <- state
  race_group <- race_translation[race]

  if (age >= 25) {
    sex_data <- age_25_sex
    race_data <- age_25_race
  } else {
    sex_data <- age_14_24_sex
    race_data <- age_14_24_race
  }

  sex_prob <- filter(sex_data, state == person_state) %>%
    pull(sex)
  race_prob <- filter(race_data, state == person_state)  %>%
    pull(race_group)
  prob <- sex_prob * race_prob

  set.seed(seed_list)
  schooling <- sample(
    x = instruction_level,
    size = 1,
    prob = prob
  )

  return(schooling)
}

# ------------------------------ Marital Status ------------------------------ #
generate_marital_status <- function(age, situation, seed_list) {
  if (!validate_marital_age(age)) return(NA)

  prob <- calculate_marital_chances(situation, age)

  set.seed(seed_list)
  marital_status <- sample(
    x = pull(marital_distribution, marital_status),
    size = 1,
    prob = prob
  )
  return(marital_status)
}
