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