for (i in seq_along(unique(AC2016$`Vl Remun Média`))) {
  print(unique(AC2016$`Vl Remun Média`)[i])
}

head(AC2016)
for (cbo in seq_along(unique(AC2016$`CBO Ocupação 2002`))) {
  for (schooling_level in seq_along(unique(AC2016$`Escolaridade após 2005`))) {
    for (age_group in seq_along(unique(AC2016$Idade))) {
      for (town in seq_along(unique(AC2016$Município))) {
        for (color in seq_along(unique(AC2016$`Raça Cor`))) {
          for (gender in seq_along(unique(AC2016$`Sexo Trabalhador`))){
            pay_mean <- AC2016 %>%
              filter(
                `CBO Ocupação 2002` == cbo,
                `Escolaridade após 2005` == schooling_level,
                Idade == age_group,
                Município == town,
                `Raça Cor` == color,
                `Sexo Trabalhador` == gender
              ) %>%
              pull(`Vl Remun Média (SM)`) %>%
              mean()

            print(pay_mean)
          }
        }
      }
    }
  }
}
ac_pay[2, ] <- c("Sombra", "Fer Tiemi")
ac_pay <- data.frame(
  "CBO Ocupação 2002",
  "Escolaridade após 2005",
  "Idade",
  "Município",
  "Raça Cor",
  "Sexo Trabalhador",
  "Mean",
  "SD"
)

for (cbo in seq_along(unique(AC2016$`CBO Ocupação 2002`))) {
  for (schooling_level in seq_along(unique(AC2016$`Escolaridade após 2005`))) {
    for (age_group in seq_along(unique(AC2016$Idade))) {
      for (town in seq_along(unique(AC2016$Município))) {
        for (color in seq_along(unique(AC2016$`Raça Cor`))) {
          for (gender in seq_along(unique(AC2016$`Sexo Trabalhador`))) {
            pay_mean <- AC2016 %>%
              filter(
                `CBO Ocupação 2002` == unique(AC2016$`CBO Ocupação 2002`)[cbo],
                `Escolaridade após 2005` == unique(AC2016$`Escolaridade após 2005`)[schooling_level],
                Idade == unique(AC2016$Idade)[age_group],
                Município == unique(AC2016$Município)[town],
                `Raça Cor` == unique(AC2016$`Raça Cor`)[color],
                `Sexo Trabalhador` == unique(AC2016$`Sexo Trabalhador`)[gender]
              ) %>%
              pull(`Vl Remun Média (SM)`) %>%
              mean(na.rm = TRUE)
            pay_sd <- AC2016 %>%
              filter(
                `CBO Ocupação 2002` == unique(AC2016$`CBO Ocupação 2002`)[cbo],
                `Escolaridade após 2005` == unique(AC2016$`Escolaridade após 2005`)[schooling_level],
                Idade == unique(AC2016$Idade)[age_group],
                Município == unique(AC2016$Município)[town],
                `Raça Cor` == unique(AC2016$`Raça Cor`)[color],
                `Sexo Trabalhador` == unique(AC2016$`Sexo Trabalhador`)[gender]
              ) %>%
              pull(`Vl Remun Média (SM)`) %>%
              sd(na.rm = TRUE)

            ac_pay[cbo * schooling_level * age_group* town * color * gender, ] <- c(
              unique(AC2016$`Raça Cor`)[color],
              unique(AC2016$`Sexo Trabalhador`)[gender],
              pay_mean,
              pay_sd
            )
          }
        }
      }
    }
  }
}

head(AC2016)

please_work_cbo <- function(data, cbo) {
  new_data <- data[data[1] == cbo, ]

  return(new_data)
}
please_work_cbo(AC2016, 517420)
please_work_age <- function(data, age) {
  new_data <- data[data[4] == age, ]

  return(new_data)
}
please_work_age(AC2016, 42)
please_work_schooling <- function(data, schooling) {
  new_data <- data[data[3] == schooling, ]

  return(new_data)
}
please_work_schooling(AC2016, 1)
please_work_city <- function(data, city) {
  new_data <- data[data[5] == city, ]

  return(new_data)
}
please_work_city(AC2016, 120040)
please_work_race <- function(data, race) {
  new_data <- data[data[6] == race, ]

  return(new_data)
}
please_work_race(AC2016, 9)
please_work_sex <- function(data, sex) {
  new_data <- data[data[8] == sex, ]

  return(new_data)
}
please_work_sex(AC2016, 2)

please_table_gen <- function(data) {
  available_cbo <- unique(data[[1]])
  available_age <- unique(data[[4]])
  available_schooling <- unique(data[[3]])
  available_city <- unique(data[[5]])
  available_race <- unique(data[[6]])
  available_sex <- unique(data[[8]])

  get_pay <- function(
    data,
    available_cbo,
    available_age,
    available_schooling,
    available_city,
    available_race,
    available_sex
  ) {
    my_data <- head(
      filter(
        data,
        `CBO Ocupação 2002` == available_cbo[1],
        Idade == available_age[1],
        `Escolaridade após 2005` == available_schooling[1],
        Município == available_city[1],
        `Raça Cor` == available_race[1],
        `Sexo Trabalhador` == available_sex[1],
      )
    ) #%>%
      #pull(`Vl Remun Média (SM)`)

    return(my_data)
  }

  my_data <- get_pay(
    data,
    # available_cbo,
    "Fer Tiemi",
    available_age,
    available_schooling,
    available_city,
    available_race,
    available_sex
  )

  if (nrow(my_data) == 0) {
    return("Essa tabela está vazia")
  } else {
    return("Ou essa tabela tem algo, ou algo deu errado...")
  }

  return(my_data)
  return(c(mean(my_data), sd(my_data)))

  # return(c(
  #   head(available_cbo),
  #   head(available_age),
  #   head(available_schooling),
  #   head(available_city),
  #   head(available_race),
  #   head(available_sex))
  # )
}
please_table_gen(AC2016)
head(unique(AC2016$`CBO Ocupação 2002`)[1])

AC2016 %>%
  filter(`CBO Ocupação 2002` == 517420, `Faixa Etária` == 7) %>%
  pull(`Vl Remun Média (SM)`) %>%
  mean()
head(filter(AC2016, c()))