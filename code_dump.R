# * Commented code for possible later use
# load("./UFTM-BioStat-DataBase/Background_Data/births.RData")

# sex_by_city_male <- c()
# sex_by_city_female <- c()
# for (i in 1:length(city_state_list)) {
#   sex_by_city_male[i] <- sum(filter(age_sex_by_city,
#                                     city == city_state_list[i])$male)
#   sex_by_city_female[i] <- sum(filter(age_sex_by_city,
#                                     city == city_state_list[i])$female)
# }
# tail(data.frame("city" = city_state_list,
#                 "male" = sex_by_city_male,
#                 "female" = sex_by_city_female))
# nrow(data.frame("city" = city_state_list,
#                 "male" = sex_by_city_male,
#                 "female" = sex_by_city_female))
# sex_by_city <- data.frame("city" = city_state_list,
#                 "male" = sex_by_city_male,
#                 "female" = sex_by_city_female)
# write_csv(sex_by_city, file = "./UFTM-BioStat-DataBase/Background_Data/sex_by_city.csv")

# ! Verify this
# * Not gonna delete this code right now because it was hard
city_state_list <- paste(city_list$city, " (", state_initials[city_list$state], ")", sep = "")

# possible_births <- substr(nascimento, 1, 4)
# possible_births <- possible_births[possible_births != "0000"]
# possible_births <- possible_births[possible_births != "2902"]

# birth_frequency <- table(possible_births)

View(filter(
  private_public_schools,
  !is.na(.data[["QT_MAT_BAS"]])
) %>%
  head(, n = 25))
View(head(drop_na(private_public_schools, c(9:48)), n = 25))
private_public_schools <- drop_na(private_public_schools, c(9:48))
head(filter(private_public_schools,
.data[["TP_CATEGORIA_ESCOLA_PRIVADA"]],
.data[["NU_CNPJ_ESCOLA_PRIVADA"]]))
head(drop_na(private_public_schools, c("TP_CATEGORIA_ESCOLA_PRIVADA", "NU_CNPJ_ESCOLA_PRIVADA")))
private_schools <- drop_na(
  private_public_schools,
  c("TP_CATEGORIA_ESCOLA_PRIVADA", "NU_CNPJ_ESCOLA_PRIVADA")
)
public_schools <- filter(
  private_public_schools,
  is.na(.data[["TP_CATEGORIA_ESCOLA_PRIVADA"]]),
  is.na(.data[["NU_CNPJ_ESCOLA_PRIVADA"]])
)
private_codes <- unique(private_schools$CO_MUNICIPIO)

for (code in private_codes) {
  filter(
    private_schools,
    CO_MUNICIPIO == code
  )
}
public_codes <- unique(public_schools$CO_MUNICIPIO)
filter(private_schools, CO_MUNICIPIO == private_codes[2]) %>%
  summarise(across(.cols = starts_with("QT"), .fns = sum))
  select(-c(1:8)) %>%
    summarise_each(funs(sum))
private_school_data <- group_by(private_schools, CO_MUNICIPIO) %>%
  summarise(across(.cols = starts_with("QT"), .fns = sum)) %>%
  bind_rows(
    summarise(., across(.cols = starts_with("QT"), .fns = sum),
    across(.cols = starts_with("CO"), ~9999999))
  ) %>%
  select(
    .,
    "CO_MUNICIPIO",
    "QT_MAT_BAS_FEM",
    "QT_MAT_BAS_MASC",
    "QT_MAT_BAS_BRANCA",
    "QT_MAT_BAS_PRETA",
    "QT_MAT_BAS_PARDA",
    "QT_MAT_BAS_AMARELA",
    "QT_MAT_BAS_INDIGENA",
    "QT_MAT_BAS_0_3",
    "QT_MAT_BAS_4_5",
    "QT_MAT_BAS_6_10",
    "QT_MAT_BAS_11_14",
    "QT_MAT_BAS_15_17",
    "QT_MAT_BAS_18_MAIS"
  )
public_school_data <- group_by(public_schools, CO_MUNICIPIO) %>%
  summarise(across(.cols = starts_with("QT"), .fns = sum)) %>%
  bind_rows(
    summarise(., across(.cols = starts_with("QT"), .fns = sum),
    across(.cols = starts_with("CO"), ~9999999))
  ) %>%
  select(
    .,
    "CO_MUNICIPIO",
    "QT_MAT_BAS_FEM",
    "QT_MAT_BAS_MASC",
    "QT_MAT_BAS_BRANCA",
    "QT_MAT_BAS_PRETA",
    "QT_MAT_BAS_PARDA",
    "QT_MAT_BAS_AMARELA",
    "QT_MAT_BAS_INDIGENA",
    "QT_MAT_BAS_0_3",
    "QT_MAT_BAS_4_5",
    "QT_MAT_BAS_6_10",
    "QT_MAT_BAS_11_14",
    "QT_MAT_BAS_15_17",
    "QT_MAT_BAS_18_MAIS"
  )
save(private_school_data, public_school_data, file = "school_data.RData")