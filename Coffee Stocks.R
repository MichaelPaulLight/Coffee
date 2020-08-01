library(tidyverse)

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
wide_coffee <- wide_coffee %>% select(Country_Name, continent, sub_region, Market_Year, `Arabica Production`, `Domestic Consumption`, Production, `Beginning Stocks` , `Ending Stocks`) %>% 
  group_by(Country_Name) %>% 
  mutate(rel_beginning_stock = `Beginning Stocks` / mean(`Beginning Stocks`)) %>% 
  ungroup()

wide_coffee$rel_beginning_stock[is.nan(wide_coffee$rel_beginning_stock)]<-0

Africa_coffee <- wide_coffee %>% 
  filter(continent == "Africa")
Asia_coffee<- wide_coffee %>% 
  filter(continent == "Asia")
Americas_coffee<- wide_coffee %>% 
  filter(continent == "Americas")
Oceania_coffee<- wide_coffee %>% 
  filter(continent == "Oceania")
Brazil <- wide_coffee %>% 
  filter(Country_Name == "Brazil")

ggplot(data = Americas_coffee) +
  geom_point(mapping = aes(x = Market_Year, size = rel_beginning_stock, y = `Domestic Consumption`, alpha = .5), color = 'blue') +
  facet_wrap(~ Country_Name, scales = 'free') +
  guides(alpha = FALSE) +
  labs(title = "Domestic Coffee Consumption Relative to Stock at Beginning of Year",
       subtitle = "North and South American Coffee Producing Countries",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year", 
       y = "Domestic Consumption (1,000 60 KG bags)",
       size = "Stock Increases with Point Size")

ggplot(data = Africa_coffee) +
  geom_point(mapping = aes(x = Market_Year, size = rel_beginning_stock, y = `Domestic Consumption`, alpha = .5), color = 'blue') +
  facet_wrap(~ Country_Name, scales = 'free') +
  guides(alpha = FALSE) +
  labs(title = "Domestic Coffee Consumption Relative to Stock at Beginning of Year",
       subtitle = "African Coffee Producing Countries",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year", 
       y = "Domestic Consumption (1,000 60 KG bags)",
       size = "Stock Increases with Point Size")

ggplot(data = Asia_coffee) +
  geom_point(mapping = aes(x = Market_Year, size = rel_beginning_stock, y = `Domestic Consumption`, alpha = .5), color = 'blue') +
  facet_wrap(~ Country_Name, scales = 'free') +
  guides(alpha = FALSE) +
  labs(title = "Domestic Coffee Consumption Relative to Stock at Beginning of Year",
       subtitle = "Asian Coffee Producing Countries",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year", 
       y = "Domestic Consumption (1,000 60 KG bags)",
       size = "Stock Increases with Point Size")

ggplot(data = Oceania_coffee) +
  geom_point(mapping = aes(x = Market_Year, size = rel_beginning_stock, y = `Domestic Consumption`, alpha = .5), color = 'blue') +
  facet_wrap(~ Country_Name, scales = 'free') +
  guides(alpha = FALSE) +
  labs(title = "Domestic Coffee Consumption Relative to Stock at Beginning of Year",
       subtitle = "Oceanic Coffee Producing Countries",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year", 
       y = "Domestic Consumption (1,000 60 KG bags)",
       size = "Stock Increases with Point Size")

ggplot(data = Brazil) +
  geom_point(mapping = aes(x = Market_Year, size = rel_beginning_stock, y = `Domestic Consumption`, alpha = .5), color = 'blue') +
  facet_wrap(~ Country_Name, scales = 'free') +
  guides(alpha = FALSE) +
  labs(title = "Domestic Coffee Consumption Relative to Stock at Beginning of Year",
       subtitle = "Brazil",
       caption = "Source: USDA Foreign Agricultural Service. (2018). USDA Foreign Agricultural Service Production, Supply, and Distribution Database. Foreign Agricultural Service, Department of Agriculture. https://data.nal.usda.gov/dataset/usda-foreign-agricultural-service-production-supply-and-distribution-database. Accessed 2020-07-30.",
       x = "Year", 
       y = "Domestic Consumption (1,000 60 KG bags)",
       size = "Stock Increases with Point Size")
