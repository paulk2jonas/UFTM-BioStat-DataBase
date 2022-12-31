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
# Neutrophils
generate_neutrophils <- function(age, study_date, birth, sex, seed_list) {
  age_filtered_table <- select_hemogram_age(age, study_date, birth, sex)
  neutrophils_mean <- pull(age_filtered_table, neutrophils_mean)
  neutrophils_sd <- pull(age_filtered_table, neutrophils_sd)

  set.seed(seed_list)
  neutrophils <- rnorm(n = 1, mean = neutrophils_mean, sd = neutrophils_sd)
  if (neutrophils < 0) neutrophils <- 0
  # All white cells will be minimum 0
  neutrophils <- round(neutrophils)

  return(neutrophils)
}

# Lymphocytes
generate_lymphocytes <- function(age, study_date, birth, sex, seed_list) {
  age_filtered_table <- select_hemogram_age(age, study_date, birth, sex)
  lymphocytes_mean <- pull(age_filtered_table, lymphocytes_mean)
  lymphocytes_sd <- pull(age_filtered_table, lymphocytes_sd)

  set.seed(seed_list)
  lymphocytes <- rnorm(n = 1, mean = lymphocytes_mean, sd = lymphocytes_sd)
  if (lymphocytes < 0) lymphocytes <- 0
  lymphocytes <- round(lymphocytes)

  return(lymphocytes)
}

# Monocytes
generate_monocytes <- function(age, study_date, birth, sex, seed_list) {
  age_filtered_table <- select_hemogram_age(age, study_date, birth, sex)
  monocytes_mean <- pull(age_filtered_table, monocytes_mean)
  monocytes_sd <- pull(age_filtered_table, monocytes_sd)

  set.seed(seed_list)
  monocytes <- rnorm(n = 1, mean = monocytes_mean, sd = monocytes_sd)
  if (monocytes < 0) monocytes <- 0
  monocytes <- round(monocytes)

  return(monocytes)
}

# Eosinophils
generate_eosinophils <- function(age, study_date, birth, sex, seed_list) {
  age_filtered_table <- select_hemogram_age(age, study_date, birth, sex)
  eosinophils_mean <- pull(age_filtered_table, eosinophils_mean)
  eosinophils_sd <- pull(age_filtered_table, eosinophils_sd)

  set.seed(seed_list)
  eosinophils <- rnorm(n = 1, mean = eosinophils_mean, sd = eosinophils_sd)
  if (eosinophils < 0) eosinophils <- 0
  eosinophils <- round(eosinophils)

  return(eosinophils)
}

# Basophils
generate_basophils <- function(age, study_date, birth, sex, seed_list) {
  age_filtered_table <- select_hemogram_age(age, study_date, birth, sex)
  basophils_mean <- pull(age_filtered_table, basophils_mean)
  basophils_sd <- pull(age_filtered_table, basophils_sd)

  set.seed(seed_list)
  basophils <- rnorm(n = 1, mean = basophils_mean, sd = basophils_sd)
  if (basophils < 0) basophils <- 0
  basophils <- round(basophils)

  return(basophils)
}

# White Cell Count
generate_leukocyte_count <- function(
  neutrophils,
  lymphocytes,
  monocytes,
  eosinophils,
  basophils
) {
  leukocytes <- neutrophils + lymphocytes + monocytes + eosinophils + basophils

  return(leukocytes)
}

# Neutrophils Percentage
generate_neutrophils_percent <- function(neutrophils, leukocytes) {
  neutrophils_perc <- (neutrophils / leukocytes) * 100
  neutrophils_perc <- round(neutrophils_perc, 2)

  return(neutrophils_perc)
}

# Lymphocytes Percentage
generate_lymphocytes_percent <- function(lymphocytes, leukocytes) {
  lymphocytes_perc <- (lymphocytes / leukocytes) * 100
  lymphocytes_perc <- round(lymphocytes_perc, 2)

  return(lymphocytes_perc)
}

# Monocytes Percentage
generate_monocytes_percent <- function(monocytes, leukocytes) {
  monocytes_perc <- (monocytes / leukocytes) * 100
  monocytes_perc <- round(monocytes_perc, 2)

  return(monocytes_perc)
}

# Eosinophils Percentage
generate_eosinophils_percent <- function(eosinophils, leukocytes) {
  eosinophils_perc <- (eosinophils / leukocytes) * 100
  eosinophils_perc <- round(eosinophils_perc, 2)

  return(eosinophils_perc)
}

# Basophils Percentage
generate_basophils_percent <- function(basophils, leukocytes) {
  basophils_perc <- (basophils / leukocytes) * 100
  basophils_perc <- round(basophils_perc, 2)

  return(basophils_perc)
}