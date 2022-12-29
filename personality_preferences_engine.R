# -------------------------------- Blood Type -------------------------------- #
# Personality traits
generate_abo_personality <- function(abo_type, seed_list) {
  number_of_traits <- 3

  possible_traits <- generate_possible_traits(abo_type, seed_list)

  prob <- calculate_traits_chances(abo_type, possible_traits)

  set.seed(seed_list)
  traits <- sample(
    x = possible_traits,
    size = number_of_traits,
    prob = prob
  )
  traits <- unname(traits)
  traits <- paste(traits, collapse = ", ")

  return(traits)
}

# -------------------------------- Preferences ------------------------------- #
# Football Team
generate_football_team <- function(age, sex, seed_list) {
  if (!validate_football_fan(age, sex, seed_list)) return("Não torce")

  set.seed(seed_list)
  team <- sample(
    x = football_clubs$club,
    size = 1,
    prob = football_clubs$fanbase
  )

  return(team)
}

# Travel Destiny
generate_travel_preference <- function(age, personality, seed_list) {
  if (!validate_travel_age(age)) {
    return(NA)
  }

  prob <- calculate_travel_chances(personality)

  set.seed(seed_list)
  travel <- sample(
    x = destinies,
    size = 1,
    prob = prob
  )
  return(travel)
}

# Period of the day
generate_day_period_preference <- function(age, personality, seed_list) {
  if (!validate_day_period_age(age)) {
    return(NA)
  }

  prob <- calculate_day_period_chances(personality)

  set.seed(seed_list)
  day_period <- sample(
    x = day_periods,
    size = 1,
    prob = prob
  )
  return(day_period)
}

# Prefered meal
generate_meal_preference <- function(age, prefered_day_period, seed_list) {
  if (!validate_meal_age(age)) return(NA)

  prob <- calculate_meal_chances(prefered_day_period, seed_list)

  set.seed(seed_list)
  meal <- sample(
    x = meals,
    size = 1,
    prob = prob
  )

  return(meal)
}

# Favorite food
generate_favorite_food <- function(age, state, seed_list) {
  if (!validate_food_age(age)) return(NA)

  possible_foods <- generate_possible_foods(state)

  set.seed(seed_list)
  favorite_food <- sample(x = possible_foods, size = 1)

  return(favorite_food)
}

# World View # ! Não esquecer de tirar o seed_list da calc
generate_world_view <- function(
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
  if (!(validate_world_view_age(age))) return(NA)

  prob <- calculate_world_view_chances(
    age,
    situation,
    marital_status,
    employment,
    reading,
    race,
    sex,
    income_minimum_wage,
    seed_list
  )

  set.seed(seed_list)
  world_view <- sample(
    x = world_views,
    size = 1,
    prob = prob
  )

  return(world_view)
}
