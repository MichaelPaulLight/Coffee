coffee <- psd_coffee %>% 
  select(Country_Name, Country_Code, Market_Year, Attribute_Description, Value)

count_cont <- datasets_14947_19943_countryContinent %>% 
  mutate(Country_Name = country)

coffee <- left_join(coffee, count_cont, by = "Country_Name")

coffee <- coffee %>% 
  mutate(Country_Name=recode(Country_Name,"Congo (Brazzaville)" = "Republic of the Congo", "Yemen (Sanaa)" = "Yemen", "Congo (Kinshasa)" = "Democratic Republic of the Congo")) %>%
  mutate(continent=recode(Country_Name, "Bolivia" = "Americas", "Cote d'Ivoire" = "Africa", "Democratic Republic of the Congo" = "Africa", "European Union" = "Europe", "Iran" = "Asia", "Korea, South
" = "Asia", "Laos" = "Asia", "Republic of the Congo" = "Africa", "Russia" = "Asia", "Taiwan" = "Asia", "Tanzania" = "Africa", "United States" = "Americas", "Venezuela" = "Americas", "Vietnam" = "Asia", "Yemen" = "Asia", .default = continent))
  
wide_coffee <- coffee %>% 
  pivot_wider(names_from = Attribute_Description, values_from = Value)

wide_coffee$Market_Year %<>% as.integer()
wide_coffee$`Arabica Production` %<>% as.integer()
wide_coffee$`Domestic Consumption` %<>% as.integer()
wide_coffee <- wide_coffee %>% select(Country_Name, continent, sub_region, Market_Year, `Arabica Production`, `Domestic Consumption`, Production)

wide_coffee <- wide_coffee %>% 
  filter(Market_Year > 1960, Production > 0)

ggplot(data = wide_coffee) + 
  geom_jitter(mapping = aes(x = Market_Year, y = Production, color = continent))

Africa_coffee <- wide_coffee %>% 
  filter(continent == "Africa")
Asia_coffee<- wide_coffee %>% 
  filter(continent == "Asia")
Americas_coffee<- wide_coffee %>% 
  filter(continent == "Americas")
Oceania_coffee<- wide_coffee %>% 
  filter(continent == "Oceania")

big_plot <- ggplot(data = wide_coffee) +
  geom_smooth(mapping = aes(x = Market_Year, y = Production, color = 'Total Production')) +
  geom_smooth(mapping = aes(x = Market_Year, y = `Domestic Consumption`, color = "Domestic Consumption")) +
  facet_wrap(~ Country_Name, scales = 'free', nrow = 5) +
  labs(title = "Trends in Coffee Production and Consumption in Producing Countries",
       subtitle = "Global Producers",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year",
       y = "Production / Consumption (1,000 60 KG bags)",
       color = "Legend")

Africa_Plot<- ggplot(data = Africa_coffee) +
  geom_smooth(mapping = aes(x = Market_Year, y = Production, color = 'Total Production')) +
  geom_smooth(mapping = aes(x = Market_Year, y = `Domestic Consumption`, color = "Domestic Consumption")) +
  facet_wrap(~ Country_Name, scales = 'free', nrow = 5) +
  labs(title = "Trends in Coffee Production and Consumption in Producing Countries",
       subtitle = "Africa",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year",
       y = "Production / Consumption (1,000 60 KG bags)",
       color = "Legend")

Asia_Plot<- ggplot(data = Asia_coffee) +
  geom_smooth(mapping = aes(x = Market_Year, y = Production, color = 'Total Production')) +
  geom_smooth(mapping = aes(x = Market_Year, y = `Domestic Consumption`, color = "Domestic Consumption")) +
  facet_wrap(~ Country_Name, scales = 'free', nrow = 5) +
  labs(title = "Trends in Coffee Production and Consumption in Producing Countries",
       subtitle = "Asia",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year",
       y = "Production / Consumption (1,000 60 KG bags)",
       color = "Legend")

Americas_Plot<- ggplot(data = Americas_coffee) +
  geom_smooth(mapping = aes(x = Market_Year, y = Production, color = 'Total Production')) +
  geom_smooth(mapping = aes(x = Market_Year, y = `Domestic Consumption`, color = "Domestic Consumption")) +
  facet_wrap(~ Country_Name, scales = 'free', nrow = 5) +
  labs(title = "Trends in Coffee Production and Consumption in Producing Countries",
       subtitle = "North and South America",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year",
       y = "Production / Consumption (1,000 60 KG bags)",
       color = "Legend")

Oceania_Plot<- ggplot(data = Oceania_coffee) +
  geom_smooth(mapping = aes(x = Market_Year, y = Production, color = 'Total Production')) +
  geom_smooth(mapping = aes(x = Market_Year, y = `Domestic Consumption`, color = "Domestic Consumption")) +
  facet_wrap(~ Country_Name, scales = 'free', nrow = 5) +
  labs(title = "Trends in Coffee Production and Consumption in Producing Countries",
       subtitle = "Oceania",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year",
       y = "Production / Consumption (1,000 60 KG bags)",
       color = "Legend")

Africa_Plot
Asia_Plot
Americas_Plot
Oceania_Plot
big_plot
