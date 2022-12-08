n_selector <- function() {
  n <- dlg_input(
    message = "Choose a number of observations (integer):",
    default = 1000,
  )$res
  n <- as.integer(n)
  if (is.na(n)) {
    message("Sorry, I didn't understand that. Please try again.")
    n <- n_selector()
    return(n)
  }
  if (n <= 0) {
    message("Please choose a number greater than 0.")
    n <- n_selector()
    return(n)
  }
  return(n)
}

seed_selector <- function() {
  seed <- dlg_input(
    message = "Choose a number of observations (integer):",
    default = 42,
  )$res
  seed <- as.integer(seed)
  if (is.na(seed)) {
    message("Sorry, I didn't understand that. Please try again.")
    seed <- n_selector()
    return(seed)
  }
  if (seed <= 0) {
    message("Please choose a number greater than 0.")
    seed <- n_selector()
    return(seed)
  }
  return(seed)
}

language_selector <- function() {
  language <- dlg_list(
    choices = c("pt", "en"),
    preselect = "pt",
    title = "Select a language to generate the data:"
  )$res
  return(language)
}

wage_selector <- function() {
  wage_format <- "RRRR,cc (R = Reais; c = centavos)"
  dlg_title <- paste(
    "Please input a value for the minimum wage in the format ",
    wage_format,
    ": ", sep = "")
  wage <- dlg_input(
    message = dlg_title,
    default = "1212,00"
  )$res
  validation <- grepl(
    pattern = "\\d+\\,\\d{2}",
    x = wage
  )
  if (validation) {
    wage <- str_extract_all(wage, "\\d+")
    wage <- unlist(wage)
    wage <- paste(wage[1], wage[2], sep = "")
  } else {
    wage <- wage_selector()
  }
  wage <- as.numeric(wage)
  return(wage)
}
