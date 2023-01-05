calculate_school_type_chances <- function(
  sex,
  race,
  age_group,
  income_minimum_wage,
  city_code
) {
  columns <- c(
    column_selectors[[sex]],
    column_selectors[[race]],
    column_selectors[[age_group]]
  )

  income_factor <- calculate_school_income_factor(income_minimum_wage)

  private_school_chance <- filter(
    private_school_data,
    CO_MUNICIPIO == city_code
  ) %>%
    select(all_of(columns))
  private_school_chance <- rowMeans(private_school_chance) * income_factor[1]

  public_school_chance <- filter(
    public_school_data,
    CO_MUNICIPIO == city_code
  ) %>%
    select(all_of(columns))
  public_school_chance <- rowMeans(public_school_chance) * income_factor[2]

  return(c(private_school_chance, public_school_chance))
}

calculate_school_income_factor <- function(
  income_minimum_wage,
  income_factor = 5
) {
  if (is.na(income_minimum_wage)) {
    income_minimum_wage <- .1
    # I could use the relative minimum, but it might be unnecessarily heavy
  }
  private_factor <- income_minimum_wage * income_factor
  public_factor <- 1 / private_factor
  return(c(private_factor, public_factor))
}

calculate_traits_chances <- function(abo_type, possible_traits) {
  expected_length <- length(blood_type_personality_traits[[abo_type]])
  additional_length <- length(possible_traits) - expected_length
  prob <- c(rep(1, expected_length), rep(.5, additional_length))

  return(prob)
}

# Travel Destiny
calculate_travel_chances <- function(personality) {
  beach_matches <- sum(str_detect(personality, beach_traits))
  country_matches <- sum(str_detect(personality, country_traits))
  city_matches <- sum(str_detect(personality, city_traits))

  beach_chance <- ifelse(beach_matches >= 1, 2 * beach_matches, 1)
  country_chance <- ifelse(country_matches >= 1, 2 * country_matches, 1)
  city_chance <- ifelse(city_matches >= 1, 2 * city_matches, 1)

  return(c(beach_chance, country_chance, city_chance))
}

# Period of the day
calculate_day_period_chances <- function(personality) {
  morning_matches <- sum(str_detect(personality, morning_traits))
  afternoon_matches <- sum(str_detect(personality, afternoon_traits))
  evening_matches <- sum(str_detect(personality, evening_traits))

  morning_chance <- ifelse(morning_matches >= 1, 2 * morning_matches, 1)
  afternoon_chance <- ifelse(afternoon_matches >= 1, 4 * afternoon_matches, 1)
  evening_chance <- ifelse(evening_matches >= 1, 5 * evening_matches, 1)

  return(c(morning_chance, afternoon_chance, evening_chance))
}

# Prefered meal
calculate_meal_chances <- function(prefered_day_period, seed_list) {
  set.seed(seed_list)
  error_term <- runif(1, min = -1, max = 1)

  if (prefered_day_period == "Manhã") {
    breakfast_chance <- 10
    lunch_chance <- 1 + error_term
    dinner_chance <- 1 + error_term
  }
  if (prefered_day_period == "Tarde") {
    breakfast_chance <- 1 + error_term
    lunch_chance <- 10
    dinner_chance <- 1 + error_term
  }
  if (prefered_day_period == "Noite") {
    breakfast_chance <- 1 + error_term
    lunch_chance <- 1 + error_term
    dinner_chance <- 10
  }

  return(c(breakfast_chance, lunch_chance, dinner_chance))
}

# Marital Status
calculate_marital_chances <- function(situation, age) {
  situation_chance <- pull(marital_distribution, situation)

  single_chance <- ((-3 / 20) * age) + 15.7
  married_chance <- ((11.7 / 100) * age) + 0.3
  divorced_chance <- ((12.9 / 100) * age) + 0.2
  widowed_chance <- ((15.5 / 100) * age) + 0.1
  age_chance <- c(
    single_chance,
    married_chance,
    divorced_chance,
    widowed_chance
  )

  return(situation_chance * age_chance)
}

calculate_world_view_chances <- function(
  age,
  situation,
  marital_status,
  employment,
  reading,
  race,
  sex,
  income_minimum_wage,
  seed_list
) {
  factors <- c(
    situation_view[situation],
    marital_view[as.character(marital_status)],
    employment_view[as.character(employment)],
    reading_view[reading],
    race_view[race],
    sex_view[sex]
  )
  age_factor <- (age - 7) / 86  # adds to negative, takes from positive
  # rn (dez/27/22), minimum wage should be about 6x bigger according to DIEESE
  # So I'll use 6x as the cut ->
  if (is.na(income_minimum_wage)) {
    income_factor <- -.5
  } else {
    income_factor <- (income_minimum_wage - 6) / 4
    # adds to positive, takes from negative
  }

  world_view_points <- tabulate_world_view_points(factors)
  world_view_points <- add_world_view_points(
    world_view_points,
    age_factor,
    income_factor
  )

  return(world_view_points$Freq)
}

# Political Position
# * To be implemented

# ----------------------------------- Drugs ---------------------------------- #
# Alcohol
calculate_drug_chances <- function(
  drug,
  sex,
  age_group,
  schooling_group,
  seed_list
) {
  drug_1 <- drug
  sex_1 <- sex
  # Both because it was causing confusion. Refactor later

  sex_mean <- filter(drug_data_sex, drug == drug_1, sex == sex_1) %>%
    pull(mean)
  sex_sd <- filter(drug_data_sex, drug == drug_1, sex == sex_1) %>%
    pull(sd)
  set.seed(seed_list)
  sex_prevalence <- rnorm(n = 1, mean = sex_mean, sd = sex_sd)

  age_mean <- filter(drug_data_age, drug == drug_1, age == age_group) %>%
    pull(mean)
  age_sd <- filter(drug_data_age, drug == drug_1, age == age_group) %>%
    pull(sd)
  set.seed(seed_list)
  age_prevalence <- rnorm(n = 1, mean = age_mean, sd = age_sd)

  schooling_mean <- filter(
    drug_data_schooling,
    drug == drug_1,
    schooling == schooling_group
  ) %>%
    pull(mean)
  schooling_sd <- filter(
    drug_data_schooling,
    drug == drug_1,
    schooling == schooling_group
  ) %>%
    pull(sd)
  set.seed(seed_list)
  schooling_prevalence <- rnorm(n = 1, mean = schooling_mean, sd = schooling_sd)

  use_chance <- mean(c(sex_prevalence, age_prevalence, schooling_prevalence))
  no_use_chance <- 100 - use_chance
  return(c(use_chance, no_use_chance))
}

# Hypertension
calculate_hypertension_chances <- function(sex, age, schooling, bmi) {
  # The income effect should come here. Refactor later.
  if (sex == "M") {
    age_modifier <- (age - 7) / 100
  } else if (sex == "F") {
    age_modifier <- (age - 8) / 100
  }
  # This "model" is too low rn. Gotta move the starting age.
  if (age_modifier < 0) age_modifier <- 0

  schooling_prevalence <- select_schooling_htension_prev(schooling)
  nutritional_prevalence <- select_nutrition_htension_prev(bmi)
  expected_prevalence <- mean(c(schooling_prevalence, nutritional_prevalence))
  prevalence <- 42 * expected_prevalence * age_modifier
  # Note: 42 was an "experimental" value... to get the prevalence higher
  # Which means I need to make this model better

  if (prevalence < 0) prevalence <- 0
  if (prevalence > 100) prevalence <- 100

  return(prevalence)
}

calculate_dx_htension_chances <- function() {
  subdiagnosis <- .132
  diagnosis <- 1 - subdiagnosis

  return(c(diagnosis, subdiagnosis))
}

calculate_dm_chances <- function(
  sex,
  age,
  schooling,
  marital_status,
  state,
  bmi,
  activity,
  alcohol,
  hypertension
) {
  for (group in seq_along(dm_age_groups)) {
    if (age %in% dm_age_groups[[group]]) {
      age_group <- names(dm_age_groups[group])
    }
  }
  if (is.na(marital_status)) marital_status <- "NA"
  if (marital_status != "Casado(a)") marital_status <- "Outro estado conjugal"
  dm_bmi <- ifelse(bmi >= 30, "Obeso", "Não obeso")
  if (is.na(activity)) activity <- "NA"
  if (activity != "Sedentário") activity <- "Ativo"
  dm_alcohol <- ifelse(alcohol, "Bebe", "Não bebe")
  dm_hypertension <- ifelse(hypertension, "Hipertenso", "Não hipertenso")

  factors <- c(
    sex,
    age_group,
    dm_schooling_groups[schooling],
    state_regions[state],
    dm_bmi,
    activity,
    dm_alcohol,
    dm_hypertension
  )

  prevalences <- filter(dm_data, variable %in% factors) %>%
    pull(prevalence)

  return(mean(prevalences))
}

calculate_stroke_chances <- function(age, sex, schooling) {
  for (group in seq_along(stroke_age_groups)) {
    if (age %in% stroke_age_groups[[group]]) {
      age_group <- names(stroke_age_groups[group])
    }
  }

  factors <- c(sex, age_group, stroke_schooling_groups[schooling])

  prevalences <- filter(stroke_data, variable %in% factors) %>%
    pull(had_stroke)

  return(mean(prevalences))
}

calculate_heart_attack_chances <- function(age, sex, schooling, state) {
  for (group in seq_along(heart_attack_age_groups)) {
    if (age %in% heart_attack_age_groups[[group]]) {
      age_group <- names(heart_attack_age_groups[group])
    }
  }
  # if (is.na(tobacco)) tobacco <- FALSE
  # if (tobacco) {
  #   smokes <- "Tabagismo"
  # } else {
  #   smokes <- "NA"
  # }
  # hypertense <- if (hypertension) "Hipertensão Arterial" else "NA"
  # diabetic <- if (!is.na(diabetes)) "Diabetes" else "NA"

  factors <- c(
    sex,
    age_group,
    heart_attack_schooling_groups[schooling],
    state_regions[state]
  )

  prevalences <- filter(heart_attack_data, variable %in% factors) %>%
    pull(mean)

  return(mean(prevalences))
}
