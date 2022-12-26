find_city_code <- function(state_name, city_name) {
  city_code <- filter(
    city_codes,
    state == state_name,
    city == city_name
  ) %>%
    pull(code)
  return(city_code)
}

# * Create function to "unify" age groups

generate_possible_traits <- function(abo_type) {
  expected_traits <- blood_type_personality_traits[[abo_type]]
  additional_traits <- sample(
    x = unlist(blood_type_personality_traits),
    size = 5
  )
  possible_traits <- c(expected_traits, additional_traits)
  possible_traits <- possible_traits[!duplicated(possible_traits)]

  return(possible_traits)
}

generate_possible_foods <- function(state) {
  additional_food <- state_foods[[state]]

  possible_foods <- c(foods, additional_food)

  return(possible_foods)
}
