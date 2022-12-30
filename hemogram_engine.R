# --------------------------------- Red Cells -------------------------------- #
# Erythrocyte Count
generate_erythrocyte_count <- function(age, study_date, birth, sex, seed_list) {
  age_filtered_table <- select_hemogram_age(age, study_date, birth, sex)
  ery_mean <- pull(age_filtered_table, erythrocytes_mean)
  ery_sd <- pull(age_filtered_table, erythrocytes_sd)

  set.seed(seed_list)
  erythrocytes <- rnorm(n = 1, mean = ery_mean, sd = ery_sd)
  if (erythrocytes < 2) erythrocytes <- 2
  # I didn't find an absolute minimum, so this should be quite extreme

  return(erythrocytes)
}

# Hemoglobin
generate_hemoglobin <- function(age, study_date, birth, sex, seed_list) {
  age_filtered_table <- select_hemogram_age(age, study_date, birth, sex)
  hemo_mean <- pull(age_filtered_table, hemoglobin_mean)
  hemo_sd <- pull(age_filtered_table, hemoglobin_sd)

  set.seed(seed_list)
  hemoglobin <- rnorm(n = 1, mean = hemo_mean, sd = hemo_sd)
  if (hemoglobin < .5) hemoglobin <- .5
  # This time I found:
  #    doi: 10.1080/08998280.2016.11929482
  #    doi: 10.1213/XAA.0000000000000132  THIS ONE

  return(hemoglobin)
}

# Hematocrit
generate_hematocrit <- function(age, study_date, birth, sex, seed_list) {
  age_filtered_table <- select_hemogram_age(age, study_date, birth, sex)
  ht_mean <- pull(age_filtered_table, hematocrit_mean)
  ht_sd <- pull(age_filtered_table, hematocrit_sd)

  set.seed(seed_list)
  hematocrit <- rnorm(n = 1, mean = ht_mean, sd = ht_sd)
  if (hematocrit < 19) hematocrit <- 19
  # Bol Asoc Med P R. 2011 Apr-Jun;103(2):25-9. PMID: 22111467  (PUBMED)

  return(hematocrit)
}

# MCV
generate_mcv <- function(hematocrit, erythrocytes) {
  mcv  <- (hematocrit * 10) / erythrocytes

  # if (mcv < 60) mcv <- 60  # ! have no vectorization rn
  # I didn't find an absolute minimum, so this should be quite extreme

  return(mcv)
}

# -------------------------------- White Cells ------------------------------- #