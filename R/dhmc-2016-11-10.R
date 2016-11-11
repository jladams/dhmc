
new_fun <- function(x) {
  new_int <- x
  other_int <- x * 2
  cat("The first argument was", new_int, "and the new number is", other_int)
}

library(tidyverse)
library(gapminder)

gapminder <- gapminder

write_csv(gapminder, "./data/gapminder.csv")

select()
filter()
group_by()
summarize()
mutate()

test <- select(gapminder, year, country, gdpPercap)
test <- gapminder %>% select(year, country, gdpPercap)

foo_foo <- little_bunny()

bop(
  scoop_up(
    hop_through(foo_foo, forest), 
    field_mouse), 
  head)

foo_foo %>%
  hop_through(forest) %>%
  scoop_up(field_mouse) %>%
  bop(head)

year_country_gdp_euro <- gapminder %>%
  filter(continent != "Europe") %>%
  select(year, country, gdpPercap)


gdp_bycontinents <- gapminder %>%
  group_by(continent, year) %>%
  summarize(meanGdpPercap = median(gdpPercap))


gapminder2 <- gapminder

gapminder2$gdp_billions <- gapminder2$gdpPercap * gapminder2$pop/10^9

gdp_bycontinent_billions <- gapminder %>%
  mutate(gdp_billion = gdpPercap * pop/10^9) %>%
  group_by(continent) %>%
  summarize(mean_gdp)

library(haven)

gapminder1 <- gapminder %>%
  select(country, year, pop)

gapminder2 <- gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(Countries = country)


gapminder_new <- merge(gapminder1, gapminder2, by.x = c("country", "year"), by.y = c("Countries", "year"))


gapminder3 <- read.csv("./data/gapminder.csv", stringsAsFactors = FALSE)
gapminder3 <- read_csv("./data/gapminder.csv")


gapminder3$year <- as.character(gapminder3$year)

gapminder_tidy <- gapminder %>%
  gather(variable, value, lifeExp:gdpPercap)

