library("tidyverse")
library("ggplot2")
library("dplyr")
library("devtools")
#devtools::install_github("UrbanInstitute/urbnmapr")
library("urbnmapr")


incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration)

#Summary Variables:

#Summary List
summary <- list()

#1. What is the most recent average value of the total jail population across all counties?
max_year <- incarceration %>% summarize(maxyr = max(year, na.rm = TRUE))
avg_total_jail_capacity <- incarceration %>%
  select(year, jail_rated_capacity) %>%
  filter(is.na(jail_rated_capacity) != TRUE) %>%
  filter(year == max(year)) %>%
  summarize(avg_cap = mean(jail_rated_capacity, na.rm = TRUE)) %>%
  pull(avg_cap)

#2. In which county was the highest jail capacity in which year?
summary$max_jail_capacity_county <- incarceration %>%
  select(county_name, year, jail_rated_capacity) %>%
  filter(is.na(jail_rated_capacity) != TRUE) %>%
  filter(jail_rated_capacity == max(jail_rated_capacity, na.rm =TRUE))%>%
  pull(county_name)
summary$max_jail_capacity_year <- incarceration %>%
  select(county_name, year, jail_rated_capacity) %>%
  filter(is.na(jail_rated_capacity) != TRUE) %>%
  filter(jail_rated_capacity == max(jail_rated_capacity, na.rm =TRUE)) %>%
  pull(year)
summary$max_jail_capacity <- incarceration %>%
  select(county_name, year, jail_rated_capacity) %>%
  filter(is.na(jail_rated_capacity) != TRUE) %>%
  filter(jail_rated_capacity == max(jail_rated_capacity, na.rm =TRUE)) %>%
  pull(jail_rated_capacity)

#3. By how much has the average rated jail capacity changed in the last 15 years?
prev_15_yr <- incarceration %>%
  select(year, jail_rated_capacity) %>%
  filter(is.na(jail_rated_capacity) != TRUE) %>%
  filter(year == max(year) - 15) %>%
  summarize(avg_cap = mean(jail_rated_capacity, na.rm = TRUE)) %>%
  pull(avg_cap)

summary$cap_change_15yr <- avg_total_jail_capacity - prev_15_yr


#4. What was the average ratio of black people in jail compared to black population?
summary$avg_black_jail_pop <- 100 * (incarceration %>%
                               select(black_jail_pop, black_pop_15to64) %>%
                               filter(is.na(black_jail_pop) != TRUE) %>%
                               filter(is.na(black_pop_15to64) != TRUE) %>%
                               mutate(ratio_black_jail_to_pop = black_jail_pop/black_pop_15to64) %>%
                               summarize(avg_ratio = median(ratio_black_jail_to_pop, na.rm =TRUE)) %>%
                               pull(avg_ratio))

#5. What was the average ratio of white people in jail compared to white population?
summary$avg_white_jail_pop <- 100 * (incarceration %>%
                               select(white_jail_pop, white_pop_15to64) %>%
                               filter(is.na(white_jail_pop) != TRUE) %>%
                               filter(is.na(white_pop_15to64) != TRUE) %>%
                               mutate(ratio_white_jail_to_pop = white_jail_pop/white_pop_15to64) %>%
                               summarize(avg_ratio = median(ratio_white_jail_to_pop, na.rm =TRUE)) %>%
                               pull(avg_ratio))

#Trends over time chart - avg ratio of black people in jail 
#to population and avg white people in jail to population over time
trends_table <- incarceration %>%
  select(year, white_jail_pop, white_pop_15to64, black_jail_pop, black_pop_15to64) %>%
  na.omit() %>%
  mutate(ratio_white_jail_to_pop = white_jail_pop/white_pop_15to64) %>%
  mutate(ratio_black_jail_to_pop = black_jail_pop/black_pop_15to64) %>%
  select(year, ratio_white_jail_to_pop, ratio_black_jail_to_pop) %>%
  group_by(year) %>%
  summarise(ratio_white_jail_to_pop = median(ratio_white_jail_to_pop, na.rm = TRUE)*100, 
            ratio_black_jail_to_pop = median(ratio_black_jail_to_pop, na.rm= TRUE)*100)
colors <- c("Black Jail Pop to Total Pop" = "blue", "White Jail Pop to Total Pop" = "green")

trends <- ggplot(data = trends_table) +
  geom_point(
    mapping = aes(x = year, y = ratio_black_jail_to_pop, color = "Black Jail Pop to Total Pop")
  ) +
  geom_point(
    mapping = aes(x = year, y = ratio_white_jail_to_pop, color = "White Jail Pop to Total Pop")
  )  +
  labs(
    title = "Average Percent In Jail to Racial Population by Year", # plot title
    x = "Year", # x-axis label
    y = "Average Percent of Racial Population in Jail", # y-axis label
    color = "Legend"
  ) +
  scale_color_manual(values = colors)


#Variable comparison chart:
jail_rated_cap_to_pop <- incarceration %>%
  select(county_name, jail_rated_capacity, total_jail_pop) %>%
  filter(is.na(county_name) != TRUE, 
         is.na(jail_rated_capacity) != TRUE, 
         is.na(total_jail_pop) != TRUE) %>%
  group_by(county_name) %>%
  summarize(avg_jail_pop = median(total_jail_pop, na.rm=TRUE),
            avg_cap = median(jail_rated_capacity, na.rm = TRUE))

comparison <- ggplot(data = jail_rated_cap_to_pop) +
  geom_point(
    mapping = aes(x = avg_jail_pop, y = avg_cap),
    color = "red",
  ) +
  xlim(0, 4000) + 
  ylim(0, 4500) +
  geom_abline(slope=1, intercept=-15) +
  labs(
    title = "Avg Jail Rated Capacity to Avg Jail Population", # plot title
    x = "Average Jail Population", # x-axis label
    y = "Average Jail Capacity", # y-axis label
  )

#map showing counties with highest ice arrests
#1. filter data set to counties with values for total jail and ice arrests in the most recent year
#and make new column of proportion

prop_ice_arrests_to_tot_jail <- incarceration %>%
                                filter(year == max(year, na.rm = TRUE)) %>%
                                select(fips, total_jail_pop, total_jail_from_ice) %>%
                                na.omit() %>%
                                filter(is.na(total_jail_pop) != TRUE & is.na(total_jail_from_ice) != TRUE) %>%
                                mutate(prop_ice_to_jail = (total_jail_from_ice/total_jail_pop)*100 ) %>%
                                mutate(fips = as.character(fips)) %>%
                                select(fips, prop_ice_to_jail) %>%
                                rename(county_fips = fips) %>%
                                left_join(counties, by = "county_fips") %>%
                                na.omit() %>%
                                filter(prop_ice_to_jail != 0)

#View(prop_ice_arrests_to_tot_jail)

map <- prop_ice_arrests_to_tot_jail %>%
  ggplot(aes(long, lat, group = group, fill = prop_ice_to_jail)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Percent of jail from ICE Arrests")


