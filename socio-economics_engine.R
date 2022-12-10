# --------------------------- Occupation and Income -------------------------- #
employment_generator <- function(age, state, seed_list) {
  if (age < 14) {
    return(FALSE)
  }
  region <- state_regions[state]
  unemployment_chance <- unemployment_rate[region]
  employment_chance <- 1 - unemployment_chance
  set.seed(seed_list)
  employment <- sample(
    c(TRUE, FALSE),
    size = 1,
    prob = c(employment_chance, unemployment_chance)
  )
  employment <- unname(employment)
  return(employment)
}

# Data generator because I'll create both the occupation and the income
occupation_data_generator <- function(age, state, city, race, employment, seed_list) {
  for (group in seq_along(occupation_age_groups)) {
    if (age %in% occupation_age_groups[[group]]) {
      age_group <- names(occupation_age_groups[group])  # I may need to use as.numeric()
      break  # if this works, add break to activity_generator()
    }
  }
  # print(age_group)
  city_code <- filter(
    city_codes,
    .data[["state"]] == state,
    .data[["city"]] == city
  ) %>%
    pull(code)
  city_code <- substring(as.character(city_code), 1, 6)
  if (age >= 14) {
    database <- occupation_database[[state]] %>%
      filter(
        .data[["Faixa Etária"]] == age_group,
        .data[["Município"]] == city_code,
        .data[["Raça Cor"]] == race_codes[race],
        .data[["Sexo Trabalhador"]] == sex_codes[sex]
      )
    if (nrow(database) == 0) employment <- FALSE  # some criteria simply don't have data
    # print(nrow(database))
  }
  set.seed(seed_list)
  if (employment) {
    index <- sample(
      x = seq_len(nrow(database)),
      size = 1
    )
    occupation <- database[index, ] %>%
      pull("CBO Ocupação 2002")
    income <- database[index, ] %>%
      pull("Vl Remun Média (SM)")
  if (income == 0) income <- 1  # because there are some 0 velues for income
  } else {
    if (age < 14) {
      occupation <- NA
      income <- NA
    }
    occupation <- sample(
      x = list("Do lar", NA),
      size = 1,
      prob = c(house_chance, nothing_chance)
    )
    income <- NA
  }
  return(list(occupation, income))
}

# ------------------------- Internet Access and Speed ------------------------ #
has_internet_access <- function(situation, seed_list) {
  set.seed(seed_list)
  has_internet <- sample(
    x = c(TRUE, FALSE),
    size = 1,
    prob = c(
      internet_access_chances[situation],
      1 - internet_access_chances[situation]
    )
  )
  has_internet <- unname(has_internet)
  return(has_internet)
}


internet_access_generator <- function(has_internet, seed_list) {
  if (has_internet) {
    set.seed(seed_list)
    internet_class <- sample(
      x = names(internet_classes),
      size = 1,
      prob = dnorm(
        x = seq_len(length(internet_classes)),
        mean = ceiling(length(internet_classes) / 2),
        sd = length(internet_classes) / 6
      )
    )
    set.seed(seed_list)
    internet_access <- runif(
      n = 1,
      min = internet_classes[[internet_class]][1],
      max = internet_classes[[internet_class]][2]
    )
  } else {
    internet_access <- NA
  }
  internet_access <- unname(internet_access)
  return(internet_access)
}
