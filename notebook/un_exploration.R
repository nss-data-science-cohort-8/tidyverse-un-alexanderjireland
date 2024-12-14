library(tidyverse)
#question 1
gdp_df <- read_csv("../data/gdp_per_capita.csv")
gdp_df |> slice(1:10)
gdp_df |> slice_tail(n=10)
#question 2
gdp_df <- gdp_df |> select(!"Value Footnotes", "Country" = "Country or Area", "Year" = Year, "GDP_Per_Capita"= Value)
#question 3
gdp_grouped_year <- gdp_df |> group_by(Year)
gdp_grouped_year_count <- gdp_grouped_year |> summarize(count = n())
ggplot(gdp_grouped_year_count, aes(x=Year, y=count)) + geom_line() + ggtitle("Number of Countries per Year in gdp_df")
#question 4
gdp_grouped_country <- gdp_df |> 
  group_by(Country) |> 
  summarize(count = n())
total_countries <- nrow(gdp_grouped_country)
total_countries
fewest_obs_countries <- gdp_grouped_country |> 
  arrange(count) |> 
  filter(count == min(count))
fewest_obs_countries
#question 5
gdp_2021 <- gdp_df |> filter(Year == 2021)
gdp_2021
#question 6
summary(gdp_2021 |> select(GDP_Per_Capita))
#question 7
ggplot(gdp_2021, aes(x=GDP_Per_Capita)) + geom_histogram(binwidth=5000)
#question 8
sorted_gdp_2021 <- gdp_2021 |> arrange(desc(GDP_Per_Capita))
sorted_gdp_2021 |> slice(1:5)
sorted_gdp_2021 |> slice_tail(n=5) |> arrange(GDP_Per_Capita) 
#question 9
gdp_pivoted <- gdp_df |> 
  pivot_wider(names_from=Year, values_from=GDP_Per_Capita) |> 
  filter(!is.na(1990) & !is.na(2021))
gdp_pivoted
#question 10
gdp_pivoted <- gdp_pivoted |> 
  select("Country", "1991":"2020", "GDP_2021" = "2021", "GDP_1990" = "1990") |> 
  mutate("Percent_Change" = 100*(GDP_2021 - GDP_1990)/GDP_1990) |> 
  rename("2021" = "GDP_2021", "1990" = "GDP_1990") |> 
  relocate("1990", .before = "1991")
gdp_pivoted
#question 11
pct_change_df <- gdp_pivoted |> 
  select("Country", "Percent_Change")
neg_pct_change <- pct_change_df |> 
  filter(Percent_Change < 0) |> 
  arrange(Percent_Change)
num_neg_pct_change_countries <- neg_pct_change |> 
  summarize(count = n())
num_neg_pct_change_countries
#question 12
highest_pct_change <- pct_change_df |> 
  arrange(desc(Percent_Change))
highest_pct_change
top_two_pct_change_countries <- highest_pct_change |> 
  slice(1:2) |> 
  pull(Country)
top2 <- gdp_df |> 
  group_by(Country) |> 
  filter(Country %in% top_two_pct_change_countries)
ggplot(top2, aes(x=Year, y=GDP_Per_Capita, color=Country)) + geom_line() + ggtitle("Top 2 Change in GDP Per Capita Countries from 1990 to 2021")
#question 13
continents <- read_csv("../data/continents.csv")
continents
#question 14
gdp_df <- gdp_df |> full_join(continents)
gdp_df
#question 15
num_countries_continent <- gdp_df |> 
  group_by(Continent) |> 
  summarize(count = n_distinct(Country)) |> 
  drop_na()
num_countries_continent
ggplot(num_countries_continent, aes(Continent, y=count)) + geom_col()
#question 16
gdp_df_2021 <- gdp_df |> filter(Year == 2021)
ggplot(gdp_df_2021, aes(x=Continent, y=GDP_Per_Capita)) + geom_boxplot()
#question 17 & 18
life_expectancy <- read_csv("../data/life_expectancy.csv", skip=3)
life_expectancy <- life_expectancy |> 
  select(-`Country Code`, -`Indicator Name`, -`Indicator Code`, "Country" = `Country Name`) |> 
  pivot_longer(!`Country`, names_to="Year", values_to="Life_Expectancy") |> 
  mutate_at("Year", as.double)
life_expectancy
#question 19
first_country_to_80 <- life_expectancy |> 
  group_by(Country) |> 
  filter(Life_Expectancy >= 80) |> 
  summarize(year_to_80 = min(Year)) |> 
  ungroup() |> 
  arrange(year_to_80) |> 
  slice(1)
first_country_to_80
#question 20
gdp_le <- gdp_df |> full_join(life_expectancy)
gdp_le |> slice(1:5)
gdp_le |> slice_tail(n=5)
#question 21
gdp_le_2021 <- gdp_le |> filter(Year == 2021)
gdp_le_2021 |> filter(Life_Expectancy >= 80) |> tally()
#question 22
top3_gdp <- gdp_le_2021 |> 
  arrange(desc(GDP_Per_Capita)) |> 
  slice(1:3)
top3_gdp_countries <- top3_gdp |> pull(Country)
top3 <- gdp_le |> 
  group_by(Country) |> 
  filter(Country %in% top3_gdp_countries)
p <- ggplot(top3, aes(x=Year, y=Life_Expectancy)) + geom_line()
p + facet_grid(cols=vars(Country))
#question 23
ggplot(gdp_le_2021, aes(x=GDP_Per_Capita, y=Life_Expectancy)) + geom_point()
#I notice that there is a positive correlation!

#question 24
library(corrr)
gdp_le_2021 |> correlate()
#this means there is a fairly high correlation between LE and GDP per Capita. As one increases, so does the other!

#question 25
gdp_le_2021 <- gdp_le_2021 |> 
  mutate(log_GDP_Per_Capita = log(GDP_Per_Capita))
gdp_le_2021 |> correlate()
ggplot(gdp_le_2021, aes(x=log_GDP_Per_Capita, y=Life_Expectancy)) + geom_point()
#There is an even higher correlation between the log of GDP Per Capita and Life Expectancy... 
#the plot demonstrates this linear relationship!