#DSC102 Lesson 9 Hands On
#Melissa Gonzalez
#11th May 2021

library(gapminder)
library(ggplot2)
library(dplyr)

levels(gapminder$country)

#Choose 5 countries
#Cuba, Haiti, Dominican Republic, Jamaica, Puerto Rico

#Which country has lowest per capita GDP in 1952? in 2007?
#Which had highest GDP in 1952 and 2007?
#Create plot with year on x and lifeExp on y for the countries.
#Compute the median of lifeExp as function for year for all countries in gapminder.
#     What years is life expectancy in my countries above median for all countries?
#Create presentation for findings and graphs.

# ------------------------------------------------------------------------------

# Create new Data Frame with only 5 chosen countries: Cuba, Haiti, Dominican Rep.,
#     Puerto Rico, and Jamaica.
gm_5countries <- gapminder %>% filter(country %in% c("Cuba", "Haiti", 
                                                     "Dominican Republic", 
                                                     "Puerto Rico",
                                                     "Jamaica")) %>% 
  select(country, year, lifeExp, gdpPercap)
gm_5countries

# Create a Line Plot to show Life Expectancy for chosen countries 1952-2007
cariblifeExp <- ggplot(gm_5countries) + geom_line(aes(x=year, y=lifeExp, 
                                                      color=country)) + 
  ylab("Life Expectancy") + ggtitle("Life Expectancy in Caribbean island nations")
cariblifeExp

# Create a Line Plot to show GDP for chosen countries 1952-2007
caribGDP <- ggplot(gm_5countries) + geom_line(aes(x=year, y=gdpPercap, 
                                                  color=country)) + 
  ylab("GDP per capita") + ggtitle("GDP per capita in Caribbean island nations")
caribGDP

# Calculate median life expectancy across all gapminder dataset
lifeExpmedians <- gapminder %>%
  group_by(year) %>%
  summarise(life_med_all_countries = median(lifeExp))
View(lifeExpmedians)

# Create a new data frame with World Average life expectancy per year
addedData <-data.frame(c("World Average", "World Average", "World Average", "World Average", "World Average", "World Average", "World Average", "World Average", "World Average", "World Average", "World Average", "World Average"),
                               c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007),
                               c(45.1355, 48.3605, 50.8810, 53.8250, 56.5300, 59.6720, 62.4415, 65.8340, 67.7030, 69.3940, 70.8255, 71.9355))
View(addedData)

# Create column names to clean up "addedData"
names(addedData) <- c("country", "year", "lifeExp")
View(addedData)

# Create dataframe with life expectancy for the 5 countries, but no GDP
gm_5countriesLE <- gm_5countries %>% select("country", "year", "lifeExp")
View(gm_5countriesLE)

# Combine addedData (world average) to gm_5countriesLE
ComparisonlifeExp <- rbind(addedData, gm_5countriesLE)
View(ComparisonlifeExp)

# Plot world average with the life expectancy from 5 chosen countries
ggplot(ComparisonlifeExp) + geom_line(aes(x=year, y=lifeExp, color=country)) + 
  ylab("Life Expectancy") + ggtitle("Comparison of Life Expectancy in Caribbean 
                                    Nations to World Average")


# GDP and Life Expectancy plots arranged on top of each other

library("gridExtra")

FivecountrieslifeExp <- ggplot(gm_5countries) + geom_line(aes(x=year, y=lifeExp,
                                                              color=country)) +
  ylab("Life Expectancy") + ggtitle("Life Expectancy and GDP per capita in 
                                    Carribbean island nations")
Fivecountriesgdp <- ggplot(gm_5countries) + geom_line(aes(x=year, y=gdpPercap, 
                                                          color=country)) + 
  ylab("Per Capita GDP")

grid.arrange(FivecountrieslifeExp, Fivecountriesgdp, ncol=1 )
