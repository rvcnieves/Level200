library(tidyverse)

##12.2.1 #2-3
# Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
#   
#   Extract the number of TB cases per country per year.
t2_cases <- filter(table2, type == "cases") %>%
  rename(cases = count) %>%
  arrange(country, year) %>%
  select(country,year,cases)
t2_cases

 ### table 4a
cases_1999 <-table4a %>%
  mutate(year = 1999,cases = table4a$`1999` )

cases_2000 <- table4a %>%
  
  mutate(year =2000, cases = table4a$`2000`)
table4_cases <- bind_rows(cases_1999,cases_2000) %>%
  select(country,year,cases)
table4_cases




# TBCasesbyCountryByyear <- table2 %>%
#   filter(type=='cases')
# 
# TBCasesbyCountryByyear$count

# Extract the matching population per country per year.
t2_population <- filter(table2, type == "population") %>%
  rename(population = count) %>%
  arrange(country, year) %>%
  select(country,year,population)
t2_population

 ### table 4b

pop_1999 <-table4b %>%
  mutate(year = 1999,population = table4b$`1999` )

pop_2000 <- table4b %>%
  
  mutate(year =2000, population = table4b$`2000`)
table4_pop <- bind_rows(pop_1999,pop_2000) %>%
  select(country,year,population)
table4_pop


# Divide cases by population, and multiply by 10000.
# Store back in the appropriate place.
rate <- t2_cases %>%
  mutate(population=t2_population$population,
         rate = t2_cases$cases / population *10000) %>%
  arrange(country,year,rate)
rate 

### table 4a/b

rate4ab <- table4_cases %>%
  mutate(population=table4_pop$population,
         rate = table4_cases$cases / population *10000) %>%
  arrange(country,year,rate)
rate4ab


# Which representation is easiest to work with? Which is hardest? Why?
#   ANSWER: table 2 was easier since all data was within that table.  Table 4 needed to integrate table4a and 4b in the same format as table2.  Then, I followed the same processed as in table 2.


#   Recreate the plot showing change in cases over time using table2 instead of table1. What do you need to do first?
library(ggplot2)
table2 %>%
  filter(type == "cases") %>%
  
  ggplot( aes(year, count)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

##12.3.3 #1-4

## 1

###Why are gather() and spread() not perfectly symmetrical?
##  Carefully consider the following example:
  
  stocks <- tibble(
    year   = c(2015, 2015, 2016, 2016),
    half  = c(   1,    2,     1,    2),
    return = c(1.88, 0.59, 0.92, 0.17)
  )
  stocks
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)
###(Hint: look at the variable types and think about column names.)

  ### ANSWER: They are not perfectly symmetrical since column type is not transferred between spread and gather and it is assumed to be a character type.

### Both spread() and gather() have a convert argument. What does it do?
## ANSWER: It will convert a character vector in the appropriate type if set to TRUE.

## 2

## Why does this code fail?
  
  table4a %>% 
#  gather(1999, 2000, key = "year", value = "cases")
  gather('1999', '2000', key = "year", value = "cases")
#  ANSWER: It fails, because 1999 and 2000 need quotes in order to work
  
## 3
###  Why does spreading this tibble fail? How could you add a new column to fix the problem?
    
  ##  ANSWER: There is no column that helps differentiate multiple observations for the same values in the key column.  So adding this column that counts number of observations per same key value will help fix it.
  
    people <- tribble(
      ~name,             ~key,    ~value, ~observations,
      #-----------------|--------|-------|---------
      "Phillip Woods",   "age",       45, 1,
      "Phillip Woods",   "height",   186, 1,
      "Phillip Woods",   "age",       50, 2,
      "Jessica Cordero", "age",       37, 1,
      "Jessica Cordero", "height",   156, 1
    )
    spread(people,key,value)
  
## 4
    
    # Tidy the simple tibble below. Do you need to spread or gather it? What are the variables?
      
      preg <- tribble(
        ~pregnant, ~male, ~female,
        "yes",     NA,    10,
        "no",      20,    12
      )
    
      ### ANSWER  the variables are sex(m/f), is_pregnant? (binary), Count of pregnancy occurrences (numeric)
      pregnant_tidy <- preg %>%
        gather(male, female, key = "sex", value = "count")
      pregnant_tidy
      
      
  
##12.5.1 #1-2
      
      ### 1   
      ####  Compare and contrast the fill arguments to spread() and complete().
      # ANSWER: 
      ## `spread()` sets the value to `NA`s if missing.
     ### complete() takes a set of columns, and finds all unique combinations. It then ensures the original dataset contains all those values, filling in explicit NAs where necessary
     
      
      
      ####2
      ####   What does the direction argument to fill() do?
      ###  ANSWER: Direction in which to fill missing values. Currently either "down" (the default) or "up
        
        