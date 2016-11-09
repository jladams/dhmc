
# Thank you for coming to today's presentation!
# I've included some of the code we'll be using today on this Etherpad.

# ------------------------------------------------
# Downloads
# Here's where you can download R: http://cran.cnr.berkeley.edu/
# Here's where you can download RStudio: https://www.rstudio.com/products/rstudio/download/

# ------------------------------------------------
# Getting Started With R
# Variables
new_int <- 4 # 'new_int' is a name we made up - variables can be called whatever is easy for you to remember
new_int

cos(new_int) # Finds the cosine of the value stored in new_int
cos(4)

# Functions
new_fun <- function(x) { # Creates a function called new_fun that takes one argument, "x"
  my_int <- x # Stores "x" as my_int
  your_int <- my_int * 2 # Stores x * 2 as your_int
  cat("My integer is ",my_int," and your integer is ",your_int) # Prints out a result telling us each our numbers
}

new_fun(4) # Try it out with any number



# Install Packages We'll Use
install.packages("tidyverse")
install.packages("gapminder")

# Load packages into work environment
library(tidyverse)
library(gapminder)

# Read in gapminder data
df <- gapminder

# Write data to local file to avoid re-downloading every time
write_csv(df, "PATH/TO/YOUR/DATA/FOLDER/filename.csv")


# Manipulating Data with dplyr, using the pipe
select()
filter()
group_by()
summarize()
mutate()

# select() and pipe
year_country_gdp <- select(gapminder,year,country,gdpPercap)
year_country_gdp <- gapminder %>% select(year,country,gdpPercap)

# How the pipe works
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

# filter()
year_country_gdp_euro <- gapminder %>%
  filter(continent=="Europe") %>%
  select(year,country,gdpPercap)

# group_by() and summarize()
gdp_bycontinents <- gapminder %>%
  group_by(continent) %>%
  summarize(mean_gdpPercap=mean(gdpPercap))

# group_by() with multiple variables
gdp_bycontinents_byyear <- gapminder %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap))

gdp_pop_bycontinents_byyear <- gapminder %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap),
            sd_gdpPercap=sd(gdpPercap),
            mean_pop=mean(pop),
            sd_pop=sd(pop))


# mutate()
gdp_pop_bycontinents_byyear <- gapminder %>%
  mutate(gdp_billion=gdpPercap*pop/10^9) %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap),
            sd_gdpPercap=sd(gdpPercap),
            mean_pop=mean(pop),
            sd_pop=sd(pop),
            mean_gdp_billion=mean(gdp_billion),
            sd_gdp_billion=sd(gdp_billion))


# ------------------------------------------------

# Tidy Data
# Link to paper: http://vita.had.co.nz/papers/tidy-data.pdf

# ------------------------------------------------

# Read in wide dataset
gap_wide <- read_csv("http://bit.ly/dartmouth-2016-11-09-wide")

# Wide to long
gap_long <- gap_wide %>%
  gather(key = obstype_year, value = obs_values, 
         starts_with('pop'),
         starts_with('lifeExp'), 
         starts_with('gdpPercap'))

# We can also do it this way
gap_long <- gap_wide %>% 
  gather(obstype_year,obs_values,-continent,-country)

# We have two types of data in one column, which is no good - let's separate them
gap_long <- gap_long %>% 
  separate(obstype_year, into=c('obs_type','year'), sep="_")

# If we wanted it to be truly tidy, we'd probably want to move obs_types to different tables, but we won't today

# Plotting with ggplot2
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

# Base plot
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp))

# CHALLENGE: Modify the example so that it shows how life expectancy has changed over time
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()

# Let's bring in a little color
ggplot(data = gapminder, aes(x = year, y = lifeExp, color=continent)) +
  geom_point()

# Lines instead of points
ggplot(data = gapminder, aes(x=year, y=lifeExp, by=country, color=continent)) +
  geom_line()

# Demonstrate how we build things up by layers
ggplot(data = gapminder, aes(x=year, y=lifeExp, by=country)) +
  geom_line(aes(color=continent)) + 
  geom_point()

# Changing the order matters
ggplot(data = gapminder, aes(x=year, y=lifeExp, by=country)) +
  geom_point() +
  geom_line(aes(color=continent))


# We can do some statistical modelling as well - let's go back to our first example
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color=continent)) +
  geom_point()

# Hard to see relationship among points because of outliers, so let's adjust the scale
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.5) + 
  scale_x_log10()

# Now we can demonstrate a relationship with a linear model
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method="lm")

# We can use locally weighted regression, as well
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method="loess")

# We can mess around with the aesthetics of the line, too - talk about why this doesn't have to go in aes()
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method="lm", size=1.5)

# CHALLENGE - using your knowledge of how to assign aesthetics, how would you turn all of the points orange and set their size to 3?
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() +
  geom_smooth(method="lm", size=1.5)

# Solution
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(size=3, color="orange") + scale_x_log10() +
  geom_smooth(method="lm", size=1.5)

# CHALLENGE - How would you assign colors based on continent, and also give each continent a geom_smooth() with the appropriate color?
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  geom_smooth(method="lm", size=1.5)

# Faceting / Multi-panel figures
# This is a way to demonstrate more variables than you might able to put on just one chart
asia <- gapminder %>%
  filter(continent == "Asia")

ggplot(asia, aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  facet_wrap( ~ country)

# Remove the legend
ggplot(asia, aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  facet_wrap( ~ country) +
  scale_color_discrete(guide = FALSE)

# We can define how we want to facet in certain directions
# Facets along Y axis
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  geom_smooth(method="lm", size=1.5) +
  facet_grid(continent~.)

# Facets along X axis
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  geom_smooth(method="lm", size=1.5) +
  facet_grid(.~continent)

# Both
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  geom_smooth(method="lm", size=1.5) +
  facet_grid(year~continent)

# Other Resources
# ggplot2 cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
# RStudio cheat sheet: https://www.rstudio.com/wp-content/uploads/2016/01/rstudio-IDE-cheatsheet.pdf
# Data wrangling (tidyr and dplyr) cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf



