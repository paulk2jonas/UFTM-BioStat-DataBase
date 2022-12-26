# -------------------------------- Blood Type -------------------------------- #
# Personality traits
generate_abo_personality <- function(abo_type) {
  number_of_traits <- 3

  possible_traits <- generate_possible_traits(abo_type)

  prob <- calculate_traits_chances(abo_type, possible_traits)

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
generate_football_team <- function(age, sex) {
  if (!validate_football_fan(age, sex)) return("Não torce")

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
