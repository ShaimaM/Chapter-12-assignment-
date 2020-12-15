#12.2.1-------------------------------------------

#Using prose, describe how the variables and observations are organised in each of the sample tables.
#In table1, each row represents a (country, year) combination. The columns cases and population contain the values for those variables.
#In table2, each row represents a (country, year, variable) combination. The column count contains the values of variables cases and population .
#In table3, each row represents a (country, year) combination. The column rate provides the values of both cases and population in formatted as cases / population.
#Table 4 is split into two tables. The table table4a contains the values of cases and table4b contains the values of population. Within each table, each row represents a country, each column represents a year.

#Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
#1-Extract the number of TB cases per country per year.
#2-Extract the matching population per country per year.
library("tidyverse")
t2_cases <- filter(table2, type == "cases") %>%
  rename(cases = count) %>%
  arrange(country, year)
t2_population <- filter(table2, type == "population") %>%
  rename(population = count) %>%
  arrange(country, year) 
#3-Divide cases by population, and multiply by 10000.
t2_cases_per_cap <- tibble(
  year = t2_cases$year,
  country = t2_cases$country,
  cases = t2_cases$cases,
  population = t2_population$population
) %>%
  mutate(cases_per_cap = (cases / population) * 10000) %>%
  select(country, year, cases_per_cap)
#4-Store back in the appropriate place.
t2_cases_per_cap <- t2_cases_per_cap %>%
  mutate(type = "cases_per_cap") %>%
  rename(count = cases_per_cap)
bind_rows(table2, t2_cases_per_cap) %>%
  arrange(country, year, type, count)
#Which representation is easiest to work with? Which is hardest? Why?
#The easiest format of a data frame  is one with columns country, year, cases, and population. 

#Recreate the plot showing change in cases over time using table2 instead of table1. What do you need to do first?
table2 %>%
  filter(type == "cases") %>%
  ggplot(aes(year, count)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country)) +
  scale_x_continuous(breaks = unique(table2$year)) +
  ylab("cases")
#Before creating the plot with change in cases over time, we need to filter table to only include rows representing cases of TB.

#12.3.3-----------------------------------------------------------

#Why are pivot_longer() and pivot_wider() not perfectly symmetrical?
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")
#The pivot_longer() expression unpivots the table, returning it to a tidy data frame with columns for half, year, and return.
#The pivot_wider() expression pivots the table to create a data frame with years as column names, and the values in return as the column values.

#Why does this code fail?
table4a %>% 
  pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")
#> Error: Can't subset columns that don't exist.
#> ??? Locations 1999 and 2000 don't exist.
#> ??? There are only 3 columns.
#Answer:
#Because the column names 1999 and 2000 are not non-syntactic variable names. To select the columns 1999 and 2000, the names must be surrounded in backticks (  `) or provided as strings.

#What would happen if you widen this table? Why?
people <- tribble(
  ~name, ~key, ~value,
  #-----------------|--------|------
  "Phillip Woods",  "age", 45,
  "Phillip Woods", "height", 186,
  "Phillip Woods", "age", 50,
  "Jessica Cordero", "age", 37,
  "Jessica Cordero", "height", 156
)
#Widening this data frame using pivot_wider() produces columns that are lists of numeric vectors because the name and key columns do not uniquely identify rows.
#How could you add a new column to uniquely identify each value?
people %>%
  distinct(name, key, .keep_all = TRUE) %>%
  pivot_wider(names_from="name", values_from = "value")

#Tidy the simple tibble below. Do you need to make it wider or longer? 
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
#To tidy the preg table use pivot_longer() to create a long table. 
#What are the variables?
#The variables in this data are:sex,pregnant and count

#12.4.3------------------------------------------------------

#What do the extra and fill arguments do in separate()? 

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"))
#separate() drops extra values with a warning.
#Experiment with the various options for the following two toy datasets.
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "drop")
tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "right")

#Both unite() and separate() have a remove argument. What does it do? Why would you set it to FALSE?
#The remove argument discards input columns in the result data frame. You would set it to FALSE if you want to create a new variable, but keep the old one.

#Compare and contrast separate() and extract(). Why are there three variations of separation (by position, by separator, and with groups), but only one unite?
#The function separate(), splits a column into multiple columns by separator.
#The function extract() uses a regular expression to specify groups in character vector and split that single character vector into multiple columns.
#Both convert a single column to many columns. However, unite() converts many columns to one, with a choice of a separator to include between column values.

#12.5.1----------------------------------------------------------

#Compare and contrast the fill arguments to pivot_wider() and complete().
#The values_fill argument in pivot_wider() and the fill argument to complete() both set vales to replace NA. Both arguments accept named lists to set values for each column. Additionally, the values_fill argument of pivot_wider() accepts a single value. 
#In complete(), the fill argument also sets a value to replace NAs but it is named list, allowing for different values for different variables. Also, both cases replace both implicit and explicit missing values.

#What does the direction argument to fill() do?
# Determine the directionwhether NA values should be replaced by the previous non-missing value ("down") or the next non-missing value ("up")
































