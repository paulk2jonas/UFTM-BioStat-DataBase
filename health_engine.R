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

# Use of Illegal Drug
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

# ------------------------------- Comorbidities ------------------------------ #
# Hypertension
generate_hypertension <- function(sex, age, schooling, bmi, seed_list) {
  if (!validate_hypertension_age(age)) return(FALSE)

  prevalence <- calculate_hypertension_chances(sex, age, schooling, bmi)
  prob <- c(prevalence, 100 - prevalence)

  set.seed(seed_list)
  hypertension <- sample(
    x = c(TRUE, FALSE),
    size = 1,
    prob = prob
  )

  return(hypertension)
}

# Diagnosed Hypertension
generate_hypertension_dx <- function(hypertension, seed_list) {
  if (hypertension) {
    prob <- calculate_dx_htension_chances()

    set.seed(seed_list)
    hypertension_dx <- sample(
      x = c(TRUE, FALSE),
      size = 1,
      prob = prob
    )

    return(hypertension_dx)
  } else {
    return(FALSE)
  }
}

generate_systolic_tension <- function(
  hypertension,
  activity_time,
  alcohol,
  seed_list
) {
  if (hypertension) {
    systolic_mean <- htension_systolic_data["mean"]
    systolic_sd <- htension_systolic_data["sd"]
  } else {
    systolic_mean <- normal_systolic_data["mean"]
    systolic_sd <- normal_systolic_data["sd"]
  }

  set.seed(seed_list)
  systolic_tension <- rnorm(
    n = 1,
    mean = systolic_mean,
    sd = systolic_sd
  )

  modifier <- calculate_btension_modifiers(activity_time, alcohol, seed_list)

  systolic_tension <- systolic_tension + modifier

  if (hypertension) {
    if (systolic_tension < 130) systolic_tension <- 130
  } else {
    if (systolic_tension > 130) systolic_tension <- 130
  }

  return(round(systolic_tension))
}

generate_diastolic_tension <- function(
  hypertension,
  activity_time,
  alcohol,
  systolic_tension,
  seed_list
) {
  if (hypertension) {
    diastolic_mean <- htension_diastolic_data["mean"]
    diastolic_sd <- htension_diastolic_data["sd"]
  } else {
    diastolic_mean <- normal_diastolic_data["mean"]
    diastolic_sd <- normal_diastolic_data["sd"]
  }

  set.seed(seed_list)
  diastolic_tension <- rnorm(
    n = 1,
    mean = diastolic_mean,
    sd = diastolic_sd
  )

  modifier <- calculate_btension_modifiers(activity_time, alcohol, seed_list)

  diastolic_tension <- diastolic_tension + modifier

  if (hypertension) {
    if (diastolic_tension < 130) diastolic_tension <- 130
  } else {
    if (diastolic_tension > 130) diastolic_tension <- 130
  }
  if (diastolic_tension > systolic_tension - 20) {
    diastolic_tension <- systolic_tension - 20
  }

  return(round(diastolic_tension))
}

generate_reduced_systolic <- function(
  hypertension_dx,
  systolic_tension,
  seed_list
) {
  if (!validate_htension_treatment(hypertension_dx)) return(NA)

  reduction <- calculate_systolic_reduction(seed_list)

  return(systolic_tension - reduction)
}

generate_reduced_diastolic <- function(
  hypertension_dx,
  diastolic_tension,
  seed_list
) {
  if (!validate_htension_treatment(hypertension_dx)) return(NA)

  reduction <- calculate_diastolic_reduction(seed_list)

  return(diastolic_tension - reduction)
}

# ----------------------------- Diabetes Mellitus ---------------------------- #
generate_diabetes_melitus <- function(
  sex,
  age,
  schooling,
  marital_status,
  state,
  bmi,
  activity,
  alcohol,
  hypertension,
  dm1_prevalence,
  dm2_prevalence,
  seed_list
) {
  if (!validate_dm_age(age)) return(NA)

  prevalence <- calculate_dm_chances(
    sex,
    age,
    schooling,
    marital_status,
    state,
    bmi,
    activity,
    alcohol,
    hypertension
  )
  prob <- c(prevalence, 100 - prevalence)

  set.seed(seed_list)
  diabetes <- sample(
    x = c(TRUE, FALSE),
    size = 1,
    prob = prob
  )

  set.seed(seed_list + 42)  # using the same seed was causing it to be always 1
  # or worse: "FALSE" was going instead of DM Tipo 2
  if (diabetes) {
    type <- sample(
      x = c("DM Tipo 1", "DM Tipo 2"),
      size = 1,
      prob = c(dm1_prevalence, dm2_prevalence)
    )
  } else {
    type <- NA
  }

  return(type)
}

generate_stroke <- function(age, sex, schooling, seed_list) {
  if (!validate_stroke_age(age)) return(FALSE)

  prevalence <- calculate_stroke_chances(age, sex, schooling)
  prob <- c(prevalence, 100 - prevalence)

  set.seed(seed_list)
  stroke <- sample(
    x = c(TRUE, FALSE),
    size = 1,
    prob = prob
  )

  return(stroke)
}

generate_heart_attack <- function(
  age,
  sex,
  schooling,
  state,
  tobacco,
  hypertension,
  diabetes,
  seed_list
) {
  if (!validate_heart_attack_age(age)) return(FALSE)

  prevalence <- calculate_heart_attack_chances(age, sex, schooling, state)
  prob <- c(prevalence, 100 - prevalence)
  if (!is.na(tobacco) && tobacco) {
    odds_ratio <- filter(heart_attack_data, variable == "Tabagismo") %>%
      pull(mean)
    prob[1] <- prob[1] * odds_ratio
  }
  if (hypertension) {
    odds_ratio <- filter(
      heart_attack_data,
      variable == "Hipertensão Arterial"
    ) %>%
      pull(mean)
    prob[1] <- prob[1] * odds_ratio
  }
  if (!is.na(diabetes)) {
    odds_ratio <- filter(heart_attack_data, variable == "Diabetes") %>%
      pull(mean)
    prob[1] <- prob[1] * odds_ratio
  }

  set.seed(seed_list)
  heart_attack <- sample(
    x = c(TRUE, FALSE),
    size = 1,
    prob = prob
  )

  return(heart_attack)
}

# Resting Heart Rate
generate_heart_rate <- function(age, birth, activity_time, seed_list) {
  heart_rate_mean <- get_heart_rate_data(age, birth, activity_time)[1]
  heart_rate_sd <- get_heart_rate_data(age, birth, activity_time)[2]

  set.seed(seed_list)
  heart_rate <- rnorm(n = 1, mean = heart_rate_mean, sd = heart_rate_sd)

  heart_rate <- round(heart_rate)

  if (heart_rate < 35) heart_rate <- 35

  return(heart_rate)
}
