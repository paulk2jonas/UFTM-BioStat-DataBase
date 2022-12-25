# -------------------------------- Blood Type -------------------------------- #
# ABO
generate_abo_blood_type <- function(abo_distribution, n) {
  abo_type <- sample(
    x = names(abo_distribution),
    size = n,
    replace = TRUE,
    prob = abo_distribution
  )
  return(abo_type)
}

# Rh
generate_rh_blood_type <- function(rh_distribution, n) {
  rh_type <- sample(
    x = names(rh_distribution),
    size = n,
    replace = TRUE,
    prob = rh_distribution
  )
  return(rh_type)
}
