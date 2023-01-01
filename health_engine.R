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

# ----------------------------------- Drugs ---------------------------------- #
# Alcohol
generate_alcohol_use <- function(age, schooling, sex, seed_list) {
  if (!validate_drug_age(age)) return(NA)

  age_group <- select_drug_age(age)
  schooling_group <- select_drug_schooling(schooling)

  prob <- calculate_drug_chances(
    "Alcohol",
    sex,
    age_group,
    schooling_group,
    seed_list
  )

  set.seed(seed_list)
  alcohol <- sample(
    x = c(TRUE, FALSE),
    size = 1,
    prob = prob
  )

  return(alcohol)
}

# Tobacco
generate_tobacco_use <- function(age, schooling, sex, seed_list) {
  if (!validate_drug_age(age)) return(NA)

  age_group <- select_drug_age(age)
  schooling_group <- select_drug_schooling(schooling)

  prob <- calculate_drug_chances(
    "Tobacco",
    sex,
    age_group,
    schooling_group,
    seed_list
  )

  set.seed(seed_list)
  tobacco <- sample(
    x = c(TRUE, FALSE),
    size = 1,
    prob = prob
  )

  return(tobacco)
}

# Non Prescribed Drugs
generate_npd_use <- function(age, schooling, sex, seed_list) {
  if (!validate_drug_age(age)) return(NA)

  age_group <- select_drug_age(age)
  schooling_group <- select_drug_schooling(schooling)

  prob <- calculate_drug_chances(
    "NPD",
    sex,
    age_group,
    schooling_group,
    seed_list
  )

  set.seed(seed_list)
  npd <- sample(
    x = c(TRUE, FALSE),
    size = 1,
    prob = prob
  )

  return(npd)
}

# Use of illegal Drug
generate_illegal_use <- function(age, schooling, sex, seed_list) {
  if (!validate_drug_age(age)) return(NA)

  age_group <- select_drug_age(age)
  schooling_group <- select_drug_schooling(schooling)

  prob <- calculate_drug_chances(
    "Illegal",
    sex,
    age_group,
    schooling_group,
    seed_list
  )

  set.seed(seed_list)
  illegal <- sample(
    x = c(TRUE, FALSE),
    size = 1,
    prob = prob
  )

  return(illegal)
}

# # Which illegal drugs
# generate_illegal_drugs <- function(illegal_drug, seed_list) {
#   if (!illegal_drug) return(NA)

#   uses <- c(rep(NULL, length(possible_illicity_drugs)))
#   for (drug in seq_along(possible_illicity_drugs)) {
#     prob <- c(
#       drug_data_illegal$prevalence[drug],
#       1 - drug_data_illegal$prevalence[drug]
#   )
#     set.seed(seed_list)
#     uses[drug] <- sample(
#       x = c(TRUE, FALSE),
#       size = 1,
#       prob = prob
#     )
#   }
#   print(uses)

#   if (any(uses)) {
#     drugs <- paste(possible_illicity_drugs[uses], sep = ", ")
#     return(drugs)
#   } else {
#     return
#   }
# }
# generate_illegal_drugs(TRUE, 43)
# ! TO BE IMPLEMENTED