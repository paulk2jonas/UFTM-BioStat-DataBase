validate_school_type <- function(age, reading) {
  if (age >= 6 && age <= 18) {
    # For now, gonna consider only 6-18 yo
    right_age <- TRUE
  } else {
    right_age <- FALSE
  }

  if (reading == "S") {
    reads <- TRUE
  } else if (reading == "N") {
    reads <- FALSE
  }

  return(right_age & reads)
}

validate_schooling_level <- function(age, reading) {
  if (age < 14) {
    right_age <- FALSE
  } else {
    right_age <- TRUE
  }

  if (reading == "S") {
    reads <- TRUE
  } else if (reading == "N") {
    reads <- FALSE
  }

  return(right_age & reads)
}

# Football team
validate_football_fan <- function(age, sex, seed_list) {
  if (age < 4) return(FALSE)

  if (sex == "F") {
    set.seed(seed_list)
    fan <- sample(
      x = c(TRUE, FALSE),
      size = 1
    )
    return(fan)
  }

  return(TRUE)
}

# Travel Destiny
validate_travel_age <- function(age) {
  if (age < 7) return(FALSE) else return(TRUE)
}

# Period of the day
validate_day_period_age <- function(age) {
  if (age < 5) return(FALSE) else return(TRUE)
}

# Prefered meal
validate_meal_age <- function(age) {
  if (age < 6) return(FALSE) else return(TRUE)
  # can never be less than the day period cut
}

# Favorite food
validate_food_age <- function(age) {
  if (age < 7) return(FALSE) else return(TRUE)
}

# Marital Status
validate_marital_age <- function(age) {
  if (age < 16) return(FALSE) else return(TRUE)
}

# World View
validate_world_view_age <- function(age) {
  if (age < 14) return(FALSE) else return(TRUE)
}

# Political Position
# * To be implemented
# validate_political_age <- function(age) {
#   if (age < 18) return(FALSE) else return(TRUE)
# }

# Favorite Color
validate_color_age <- function(age) {
  if (age < 4) return(FALSE) else return(TRUE)
}

# Drugs
validate_drug_age <- function(age) {
  if (age < 12) return(FALSE) else return(TRUE)
}
