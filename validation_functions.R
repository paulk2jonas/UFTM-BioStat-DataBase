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

