calculate_school_type_chances <- function(
  sex,
  race,
  age_group,
  income_minimum_wage,
  city_code
) {
  columns <- c(
    column_selectors[[sex]],
    column_selectors[[race]],
    column_selectors[[age_group]]
  )

  income_factor <- calculate_school_income_factor(income_minimum_wage)

  private_school_chance <- filter(
    private_school_data,
    CO_MUNICIPIO == city_code
  ) %>%
    select(all_of(columns))
  private_school_chance <- rowMeans(private_school_chance) * income_factor[1]

  public_school_chance <- filter(
    public_school_data,
    CO_MUNICIPIO == city_code
  ) %>%
    select(all_of(columns))
  public_school_chance <- rowMeans(public_school_chance) * income_factor[2]

  return(c(private_school_chance, public_school_chance))
}

calculate_school_income_factor <- function(
  income_minimum_wage,
  income_factor = 5
) {
  if (is.na(income_minimum_wage)) {
    income_minimum_wage <- .1
    # I could use the relative minimum, but it might be unnecessarily heavy
  }
  private_factor <- income_minimum_wage * income_factor
  public_factor <- 1 / private_factor
  return(c(private_factor, public_factor))
}

calculate_traits_chances <- function(abo_type, possible_traits) {
  expected_length <- length(blood_type_personality_traits[[abo_type]])
  additional_length <- length(possible_traits) - expected_length
  prob <- c(rep(1, expected_length), rep(.5, additional_length))

  return(prob)
}
