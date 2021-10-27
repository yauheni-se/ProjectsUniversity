# install.packages("dplyr")
# install.packages("gapminder")

install.packages("gdtools")

install.packages("kableExtra", dependencies = TRUE)

devtools::install_github("haozhu223/kableExtra")

if (require("sexr") != TRUE) {
  install.packages("gdtools")
}

library(dplyr)
# library(gapminder)
# gap <- gapminder
# Ctrl+Shift+M - pipe
# Alt+'-' - assignment

library(rmarkdown)

paged_table(options = options)
starwars %>% 
  lm(mass~.)

gap <- read.csv('https://csiro-data-school.github.io/r/data/gapminder.csv')


str(gap)

#### view str of data
glimpse(gap)
Vi

glimpse(starwars)
############################################################################################################


gap_selected <- gap %>%
  select(country, year)
class(gap_selected)


#### SELECT
#1 -- usual way
gap_selected <- gap %>% 
  select(country, year)
gap_selected

#2 -- select not
gap_selected <- gap %>% 
  select(-country, -year)
gap_selected

#3 -- select from vector
col_names <-  c("year", "lifeExp")
gap_selected <- gap %>% 
  select(col_names)
gap_selected

#4 -- numeric range
gap_selected <- gap %>% 
  select(1:3)
gap_selected

#5 -- select last column
gap_selected <- gap %>% 
  select(last_col())
gap_selected

#6 -- select columns that starts with
gap_selected <- gap %>% 
  select(starts_with("co", ignore.case = TRUE))
gap_selected

#7 -- select columns that ends with
gap_selected <- gap %>% 
  select(ends_with("cap"))
gap_selected

#8 -- select columns that contains
gap_selected <- gap %>% 
  select(contains("e"))
gap_selected

#9 -- select columns that matches regex
gap_selected <- gap %>% 
  select(matches("+Per+"))
gap_selected



#### RENAME -- zmienic nazwe
gap_renamed <- gap %>% 
  rename(population = pop,
         life_expectancy = lifeExp,
         gdp_per_cap = gdpPercap)
gap_renamed



#### RELOCATE -- zmienic miejsce; mozna zmienic i nazwe
gap_relocated <- gap_renamed %>%
  relocate(year, country, continent, population)
gap_relocated


#! select tez pozwala na zmiane nazwy oraz na relokacje:
gap_relocated <- gap %>%
  select(-pop,
         -lifeExp,
         -gdpPercap,
         population = pop,
         life_expectancy = lifeExp,
         gdp_per_cap = gdpPercap)
gap_relocated



#### MUTATE -- add definitely new columns wia calculations

#1
gap_mutated <- gap_relocated %>%
  mutate(gdp = gdp_per_cap * population)
gap_mutated

#2
gap_mutated <- gap_relocated %>%
  mutate(gdp = gdp_per_cap * population, .keep = "used")    # all/used/unused/none
gap_mutated %>% head()

#3
gap_mutated <- gap_relocated %>%
  mutate(gdp = gdp_per_cap * population,
         worldwide_pop_cumulative = sum(population),
         avg_worldvide_gdp_cumulative = mean(gdp))
gap_mutated



#### TRANSMUTE -- add definitely new columns wia calculations but only they are kept
gap_transmuted <- gap_relocated %>%
  transmute(gdp = gdp_per_cap * population,
         worldwide_pop_cumulative = sum(population),
         avg_worldvide_gdp_cumulative = mean(gdp))
gap_transmuted




############################################################################################################
gap_new <- gap %>% 
  rename(gdp_pc = gdpPercap,
         life_exp = lifeExp,
         population = pop) %>%
  relocate(year, country, continent, population) %>% 
  mutate(gdp = gdp_pc * population)
gap_new


#### ARRANGE

#1
gap_new_arranged <- gap_new %>%
  arrange(population)
gap_new_arranged

#2
gap_new_arranged <- gap_new %>%
  arrange(population, life_exp)
gap_new_arranged

#3
gap_new_arranged <- gap_new %>%
  arrange(desc(population, life_exp))
gap_new_arranged



#### SLICE
gap_new_sliced <- gap_new %>%
  slice(1000:nrow(gap_new))
gap_new_sliced 



#### TOP_N, TOP_FRAC
gap_new_top <- gap_new %>%
  top_n(3, gdp_pc)
gap_new_top


betwe
#### FILTER

#1
gap_new_filtered <- gap_new %>%
  filter(year == 2007)
gap_new_filtered


#2
gap_new_filtered <- gap_new %>%
  filter(year == 2007 & continent == "Europe")
gap_new_filtered


#3
gap_new_filtered <- gap_new %>%
  filter(year %in% c(2007, 1972))
gap_new_filtered


#4
gap_new_filtered <- gap_new %>%
  filter(between(year, 1972, 2007))
gap_new_filtered



#### GROUP_BY

gap_new_grouped <- gap_new %>%
  group_by(year)
gap_new_grouped


gap_new_grouped <- gap_new %>%
  group_by(year, country)
gap_new_grouped


#### SUMMARIZE -- awersome group statistics

#1
gap_new_summarized <- gap_new_grouped %>%
  group_by(year) %>%
  summarize(earth_popul = sum(population))
gap_new_summarized


#2
gap_new_summarized <- gap_new %>%
  group_by(continent, year) %>% 
  summarize(max_lifexp = max(life_exp))
gap_new_summarized


#3
gap_new_summarized <- gap_new %>%
  filter(gdp_pc <= 5000) %>% 
  group_by(continent, country) %>% 
  summarize(mean_lifexp = mean(life_exp),
            mean_gdp_pc = mean(gdp_pc)) %>% 
  arrange(mean_gdp_pc) %>%
  tail(5)
gap_new_summarized


#### COUNT
gap_new_counted <- gap_new %>%
  filter(gdp_pc >= 75000) %>% 
  count()
gap_new_counted


#! == equals
gap_new_counted <- gap_new %>%
  filter(gdp_pc >= 75000) %>% 
  summarize(n = n())
gap_new_counted



#### VERB_IF, _AT, _ALL

# works for -- mutate, select, rename, transmute, filter, arrange, summarize, group_by


#1
gap
gap_new_2 <- gap %>%
  mutate_if(is.numeric, log10)
gap_new_2


#! more funcs
gap_new_2 <- gap %>%
  mutate_if(is.numeric, list(~log10(.),
                             ~`*`(., 100)))
gap_new_2


gap_new_2 <- gap %>%
  mutate_if(is.numeric, c(log10, log2))
gap_new_2


#2
gap_new_2 <- gap %>%
  mutate_at(c("lifeExp", "pop", "gdpPercap"), `/`, 100)
gap_new_2


#3
gap_new_2 <- gap %>%
  mutate_all(as.character)
gap_new_2

library(dplyr)
x <- c(1, 2, 3)
mean(sqrt(log(max(x))))

x %>% 
  max() %>% 
  log() %>% 
  sqrt() %>% 
  mean()

############################################################################################################


df1 <- gap %>%
  filter(country %in% c("Poland", "Germany", "Norway") & year == 2007) %>% 
  select(country, gdp = gdpPercap)
df1


df2 <- gap %>%
  filter(country %in% c("Poland", "Germany", "Spain") & year == 2007) %>% 
  select(country, life_exp = lifeExp)
df2

df1 %>% 
  bind_cols(df2)


inner_join(df1, df2, by = c("country"))
full_join(df1, df2, by = c("country"))


left_join(df1, df2, by = c("country"))
right_join(df1, df2, by = c("country"))



semi_join(df1, df2, by = c("country"))
anti_join(df1, df2, by = c("country"))


df3 <- gap %>%
  filter(country %in% c("Czech Republic", "Italy", "Sweden") & year == 2007) %>% 
  select(country, gdp = gdpPercap)
df3


df1 %>% bind_cols(df2 %>% select(-country)) 
df1 %>% bind_rows(df3)