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
