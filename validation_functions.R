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

validate_football_fan <- function(age, sex) {
  if (age < 4) return(FALSE)

  if (sex == "F") {
    fan <- sample(
      x = c(TRUE, FALSE),
      size = 1
    )
    return(fan)
  }

  return(TRUE)
}

validate_travel_age <- function(age) {
  if (age < 7) return(FALSE) else return(TRUE)
}
