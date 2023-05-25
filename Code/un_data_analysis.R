library(tidyverse)
#Read in dataset
gapminder_data  <- read_csv("Data/gapminder_data.csv")
view(gapminder_data)

#What is the mean life expectancy? Using summarize()
summarize(gapminder_data, averageLifeExp = mean(lifeExp))
#output was
# A tibble: 1 × 1
#averageLifeExp
#          <dbl>
#1       59.47444

#Using the pipe operator, we can replicate the previous line of code with the same
#output, and with more functions we can use it without having to nest it
gapminder_data %>%
  summarize(averageLifeExp = mean(lifeExp))
#We can also create a variable in which to store the output, as a dataset
gapminder_data_summarized <- gapminder_data %>%
  summarize(averageLifeExp = mean(lifeExp))

#What is the mean population in the gapminder_data dataset?
gapminder_data_pop <- gapminder_data %>%
  summarize(averagePop = mean(pop))

#How would we summarize 2 things to the same output dataframe?
gapminder_data %>%
  summarize(averageLifeExp = mean(lifeExp),
            averagePop = mean(pop))

#What is the mean life expectancy for the most recent year?
gapminder_data %>%
  summarize(recentYr = max(year)) #Output shows most recent year is 2007
gapminder_data %>%
  filter(year == 2007) #shows all the rows with years 2007

gapminder_data %>%
  filter(year == 2007) %>% 
  summarize(averageLifeExp2007 = mean(lifeExp)) #puts it together to show output 27

gapminder_data %>%
  filter(year == max(year)) %>% 
  summarize(averageLifeExp2007 = mean(lifeExp)) #shows same output without hardcoding
                                  #older output, this way code works by variables only

#What is the mean GDP per capita for the first/earliest year?
gapminder_data %>%
  filter(year == min(year)) %>% 
  summarize(meanGDPearly = mean(gdpPercap)) # output is a tibble with 3725

#Other operators like == are <,>, !=

#What is the mean life expectancy for each year?
gapminder_data %>%
  group_by(year) %>%
  summarize(yrlyLifeExp = mean(lifeExp))

#What is the mean life expectancy for each continent?
gapminder_data %>%
  group_by(continent) %>%
  summarize(lifeExpContinent = mean(lifeExp))

#What is the mean life expectancy and mean GDP per capita for each continent?
gapminder_data %>%
  group_by(continent) %>%
  summarize(lifeExpContinent = mean(lifeExp),
            meanGDPearly = mean(gdpPercap))

#What is the total GDP for each country (not per capita)?
gapminder_data %>%
  mutate(gdp = gdpPercap*pop) #output tibble has a new column with multiplied values

#Make a new column for population in millions and the total GDP for each
#country (not per capita) in the same tibble?
gapminder_data %>%
  mutate(popInMill = pop/1000000, 
          gdp = gdpPercap*pop)  #This will send the output tibble to the console

gapminder_data <- gapminder_data %>%  
  mutate(popInMill = pop/1000000, 
         gdp = gdpPercap*pop)   #This will not show output in the console but instead
                    #will modify the gapminder_data dataset object and save it as such

gapminder_data %>%
  select(year, pop) #will only show these 2 columns in the console output

gapminder_data %>%
  select(-continent) #will show every column except continent in console output

#Create a tibble with only country, continent, year and lifeExp
gapminder_data %>%
  select(country, continent, year, lifeExp) #or

gapminder_data %>%
  select(-gdpPercap, -pop) #same output


#select() helper functions can be used inside: starts_with() and ends_with()
gapminder_data %>%
  select(year, starts_with("C")) #this will output the column for year as well
#as the columns that start with letter C, continent and country

#Vectors are a set of values all of the same type, such as all numbers or all
#characters. A vector cannot be a mix of numbers and characters. Each column in
#the gapminder dataset is a vector of whatever type. The way you specify a new
#vector you want to make is using c()

vec <- c() #empty vector containing NULL is made in the environment
vec <- c("dog", "cat", "horse") #vector containing 3 character values
numVec <- c(1,2,3,4) #vector containing 3 number values

proof <- gapminder_data %>% 
  pull(year) #because each column in gapminder is a vector, when a single
#column is pulled out, it becomes a vector in the Values area of the
#environment

your_data %>% 
  filter (id %in% c("A", "B", "C")) #this would allow you to filter your
#dataset by a vector of IDs you send it

gapminder_data %>%
  select(contains("e")) #this would output all columns containing e

#pivot_wider can take datasets from longform (closer to one row for every observation)
#to a wide form (where rows now contain more than one observation); and
#pivot_longer can make a longform dataset out of rows that have many observations

#Use pivot_wider to have columns of the life expectancy for each year
gapminder_data %>%
  select(country, continent, year, lifeExp) %>% 
    pivot_wider(
    names_from = year,
    values_from = lifeExp
  )
#Use pivot_wider and populate values with gdpPercap
gapminder_data %>%
  select(country, continent, year, gdpPercap) %>% 
  pivot_wider(
    names_from = year,
    values_from = gdpPercap
  )

#Use pivot_longer to make just one column for each observation
gapminder_data %>%
  pivot_longer(cols=c(pop, lifeExp, gdpPercap),
              names_to = "measure",
              values_to = "measurement")

#Is there a relationship between GDP and CO2 emissions?
#Steps:
#assign result to gapminder_data_2007
#filter for year 2007 and Americas as continent
#remove the year and continent columns
gapminder_data2007 <- gapminder_data %>%
  filter(year ==2007, continent == "Americas") %>% 
  select(-year,-continent)


#What percent of total CO2 emissions is accounted for by North America?
#skipping 2 error-ridden rows and renaming the columns
read_csv("Data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year","series", "value",
                       "footnotes", "source")) -> co2_emissions_dirty
#since some of these rows have long strings in the series column, we can recode
#them so we dont need to type long things out

co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))

co2_emissions <- co2_emissions_dirty %>% pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

#inner_join will look at the intersection of the venn diagram of 2 datasets
#outer_join will only take items in the outer sets of the venn diagram of 
#2 datasets

inner_join(gapminder_data2007, co2_emissions, by = "country")







