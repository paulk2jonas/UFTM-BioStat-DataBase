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

generate_possible_traits <- function(abo_type, seed_list) {
  expected_traits <- blood_type_personality_traits[[abo_type]]
  set.seed(seed_list)
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

tabulate_world_view_points <- function(factors) {
  world_view_points <- table(factors, useNA = "no")
  world_view_points <- as.data.frame(world_view_points)

  if (!any(world_view_points$factors == "Negativa")) {
    temp_df <- data.frame(factors = "Negativa", Freq = 0)
    world_view_points <- rbind(world_view_points, temp_df)
  }
  if (!any(world_view_points$factors == "Realista")) {
    temp_df <- data.frame(factors = "Realista", Freq = 0)
    world_view_points <- rbind(world_view_points, temp_df)
  }
  if (!any(world_view_points$factors == "Positiva")) {
    temp_df <- data.frame(factors = "Positiva", Freq = 0)
    world_view_points <- rbind(world_view_points, temp_df)
  }

  world_view_points <- sort_world_view_df(world_view_points)

  return(world_view_points)
}

add_world_view_points <- function(
  world_view_points,
  age_factor,
  income_factor
) {
  world_view_points[1, 2] <- world_view_points[1, 2] + age_factor
  world_view_points[3, 2] <- world_view_points[3, 2] - age_factor
  world_view_points[1, 2] <- world_view_points[1, 2] - income_factor
  world_view_points[3, 2] <- world_view_points[3, 2] + income_factor

  world_view_points <- force_negative_to_zero(world_view_points)

  return(world_view_points)
}

force_negative_to_zero <- function(world_view_points) {
  # ! Should definetelly make this better later
  for (line in c(1, 3)) {
    if (world_view_points[line, 2] < 0) {
      world_view_points[line, 2] <- 0
    }
  }

  return(world_view_points)
}

sort_world_view_df <- function(world_view_points) {
  negativa <- world_view_points$Freq[world_view_points$factors == "Negativa"]
  realista <- world_view_points$Freq[world_view_points$factors == "Realista"]
  positiva <- world_view_points$Freq[world_view_points$factors == "Positiva"]

  world_view_points <- data.frame(
    factors = world_views,
    Freq = c(negativa, realista, positiva)
  )

  return(world_view_points)
}
