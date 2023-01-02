find_city_code <- function(state_name, city_name) {
  city_code <- filter(
    city_codes,
    state == state_name,
    city == city_name
  ) %>%
    pull(code)
  return(city_code)
}

calculate_age_in_months <- function(study_date = study_date, birth = birth) {
  # ! For some reason, both arguments must be given
  # * Maybe the "standard" isn't working like that (variable)
  age_in_months <- floor((birth %--% study_date) / months(1))

  return(age_in_months)
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

# Hemogram 1
select_hemogram_age <- function(age, study_date, birth, sex) {
  if (age < 1) {
    age_in_months <- calculate_age_in_months(study_date, birth)

    for (i in seq_along(hemogram_baby_groups)) {
      if (age_in_months %in% hemogram_baby_groups[[i]]) {
        group <- names(hemogram_baby_groups[i])
      } else {
        group <- "1-2 years"
      }
    }
  } else if (age <= 10) {
    for (i in seq_along(hemogram_age_groups)) {
      if (age %in% hemogram_age_groups[[i]]) {
        group <- names(hemogram_age_groups[i])
      }
    }
  } else if (age >= 70) {
    group <- "70 years or more"
  } else if (sex == "M") {
    group <- "male adult"
  } else if (sex == "F") {
    group <- "female adult"
  }

  filtered_table <- filter(hemogram_data_1, age == group)
  return(filtered_table)
}

# Drugs - age groups
select_drug_age <- function(age) {
  for (group in seq_along(drug_age_groups)) {
    if (age %in% drug_age_groups[[group]]) {
      age_group <- names(drug_age_groups[group])
    }
  }

  return(age_group)
}

# Drugs <- schooling groups
select_drug_schooling <- function(schooling) {
  return(drug_schooling_groups[schooling])
}

# Hypertension
select_schooling_htension_prev <- function(schooling) {
  schooling_prevalence <- filter(
    htension_schooling,
    schooling_level == htension_schooling_groups[schooling]
  ) %>%
    pull(mean)  # rn, not gonna consider the confidence interval

  return(schooling_prevalence)
}

select_nutrition_htension_prev <- function(bmi) {
  if (bmi < 18.5) {
    nutrition <- "Baixo peso"
  } else if (bmi < 30) {
    nutrition <- "Eutrofia"
  } else if (bmi < 35) {
    nutrition <- "Sobrepeso"
  } else {
    nutrition <- "Obesidade"
  }

  nutritional_prevalence <- filter(
    htension_nutrition,
    nutritional_state == nutrition
  ) %>%
    pull(mean)  # rn, not gonna consider the confidence interval

  return(nutritional_prevalence)
}

calculate_btension_modifiers <- function(activity_time, alcohol, seed_list) {
  if (is.na(activity_time)) activity_time <- 0
  if (is.na(alcohol)) alcohol <- FALSE

  if (activity_time > 2.5) {
    set.seed(seed_list)
    modifier_1 <- runif(n = 1, min = -7, max = -4)
  } else {
    modifier_1 <- 0
  }

  if (!alcohol) {
    set.seed(seed_list)
    modifier_2 <- runif(n = 1, min = -5, max = -4)
  } else {
    modifier_2 <- 0
  }

  return(mean(c(modifier_1, modifier_2)))
}

calculate_systolic_reduction <- function(seed_list) {
  set.seed(seed_list)
  reduction <- rnorm(n = 1, mean = 10, sd = 6.67)

  return(reduction)
}

calculate_diastolic_reduction <- function(seed_list) {
  set.seed(seed_list)
  reduction <- rnorm(n = 1, mean = 10, sd = 3.5)

  return(reduction)
}
