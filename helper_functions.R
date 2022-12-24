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