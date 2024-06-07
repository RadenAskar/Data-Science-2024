##Energy Data 
github_energy <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/energy_2022.tsv")
github_energy_consumption <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/energy_consumption.tsv")
github_energy_mixdata <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/per_capita_energy_mix.tsv")


#######Average data for renewable energy in 5 countries######## PROBLEM 1
#Sort those 5 countries: 
asean_countries <- c("Indonesia", "Malaysia", "Thailand", "Philippines", "Vietnam")

#Filter Data
asean_energy_data <- github_energy %>% filter(country %in% asean_countries)

#Sorting the Renewable Energy columns
renewable_energy <- c("biofuel_electricity", "gas_electricity", "hydro_electricity", "nuclear_electricity", "renewables_electricity", "solar_electricity", "wind_electricity")

#Calculate the average of renewable energy columns
average_renewable_energy <- asean_energy_data %>%
  select(country, all_of(renewable_energy)) %>%
  group_by(country) %>%
  summarise(across(everything(),mean, na.rm = TRUE))

#Melt the data
average_renewable_energy_melted <- melt(average_renewable_energy, id.vars = "country")

#Change Labels
energy_type <- c(
  "biofuel_electricity"= "Biofuel",
  "gas_electricity"= "Gas",
  "hydro_electricity"= "Hydro",
  "nuclear_electricity"= "Nuclear",
  "renewables_electricity"= "Average Renewable Electricity",
  "solar_electricity"= "Solar",
  "wind_electricity"= "Wind")


data_for_bar <- average_renewable_energy_melted %>% 
  mutate(renamed_labels = recode(variable, 
                                 "biofuel_electricity"= "Biofuel",
                                 "gas_electricity"= "Gas",
                                 "hydro_electricity"= "Hydro",
                                 "nuclear_electricity"= "Nuclear",
                                 "renewables_electricity"= "Average Renewable Electricity",
                                 "solar_electricity"= "Solar",
                                 "wind_electricity"= "Wind"))

#Visualize in Geom Bar
ggplot(data = data_for_bar %>% filter(renamed_labels != "Nuclear"), 
       aes(x = country, y = value, fill = renamed_labels)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Renewable Energy Data in ASEAN Countries",
       x = "Country",
       y = "Kilo Watt Hour",
       fill = "Energy Type") + 
    scale_x_discrete(labels = energy_type)

#Visualize in Tree Map
ggplot(data = average_renewable_energy_melted, aes(area = value, fill = variable, label = Country)) +
  geom_treemap() +
  labs(title = "Average Renewable Energy Data in ASEAN Countries",
       fill = "Energy Type") +
  scale_fill_discrete(labels = energy_type) +
  theme_minimal()



