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
       aes(x = country, y = value, 
           fill = renamed_labels, label = round(value))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Renewable Energy Data in ASEAN Countries",
       x = "Country",
       y = "Kilo Watt Hour",
       fill = "Energy Type") + 
    scale_x_discrete(labels = energy_type) +
  geom_text(position = position_dodge(width = 0.9),
            size = 3,
            vjust = -0.5, colour = "black")


#Visualize in Sunburst Chart#### FAIL
data <- data.frame(
  country = c("Indonesia", "Malaysia", "Philippines", "Thailand", "Vietnam"),
  energy_type = c("Biofuel", "Gas", "Hydro", "Solar", "Wind"),
  Value = c("data_for_bar")
)

#Calculate angles
data_for_bar <- data_for_bar %>%
  group_by(country) %>%
  mutate(country_total = sum(value)) %>%
  ungroup() %>%
  arrange(country, renamed_labels) %>%
  mutate(
    fraction = value / sum(value),
    ymax = cumsum(fraction),
    ymin = lag(ymax, default = 0),
    country_ymax = cumsum(country_total / sum(country_total)),
    country_ymin = lag(country_ymax,default = 0)
  )

###Visualize in Sunburst chart###
ggplot(data_for_bar, aes(fill = renamed_labels)) +
  geom_rect(
    aes(xmin = country_ymin, xmax = country_ymax, ymin = ymin, ymax = ymax),
    colour="white"
  ) +
  geom_rect(
    aes(xmin = country_ymin, xmax = country_ymax, ymin = 0, ymax = 1),
    fill = NA, colour = "white", size = 0.5
  ) +
  scale_fill_brewer(palette= "Set3") +
  coord_polar(theta="y") +
  labs(title = "Average Renewable Energy by Country and Type in ASEAN") +
  theme_void() +
  theme(legend.position="right")


  
