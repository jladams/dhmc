
# Data from: https://www.healthdata.gov/dataset/deaths-pnuemonia-and-influneza-and-all-deaths-state-and-region-national-center-health
# Etherpad at http://bit.ly/dhmc-11-01-16


library(tidyverse)
library(stringr)

# Read in CSV file from URL
df <- read_csv("http://bit.ly/dhmc-11-01-16-data")

# Save local copy of your data to avoid re-downloading
write_csv(df, "./data/deaths.csv")

df <- read_csv("./data/deaths.csv")

df <- df %>%
  separate(season, into = c("season_start", "season_end"), convert = TRUE) %>%
  mutate(week = str_sub(`MMWR Year/Week`, start = 5), year = str_sub(`MMWR Year/Week`, end = 4)) %>%
  select(geoid, region = Region, age, week, year, season_start, flu = `Deaths from influenza`, pneumonia = `Deaths from pneumonia`, all_deaths = `All Deaths`)

write_csv(df, "./data/deaths_cleaned.csv")

# Times when it's fine for data not to be perfectly tidy
regional <- df %>%
  filter(geoid == "Region" & age == "All")

ggplot(regional, aes(x = flu, y = pneumonia)) +
  geom_point()

ggplot(regional, aes(x = flu, y = pneumonia))

ggplot(regional, aes(x = flu, y = pneumonia)) +
  geom_point(color = "red")

ggplot(regional, aes(x = flu, y = pneumonia)) +
  geom_point(aes(color = region))

ggplot(regional, aes(x = flu, y = pneumonia)) +
  geom_point(aes(color = region)) +
  scale_x_log10()

ggplot(regional, aes(x = flu, y = pneumonia)) +
  geom_point(aes(color = region)) +
  scale_x_log10() +
  geom_smooth(method = "lm")

# Other times when it's easier for it to be so
national <- df %>%
  filter(geoid == "National" & age == "All")

national_seasons <- national %>%
  group_by(season_start) %>%
  summarize(flu = sum(flu), pneumonia = sum(pneumonia), all_deaths = sum(all_deaths))

p <- ggplot(national_seasons) +
  geom_line(aes(x = season_start, y = flu)) +
  geom_text(aes(x = season_start, y = flu, label = flu)) +
  geom_line(aes(x = season_start, y = pneumonia)) +
  geom_text(aes(x = season_start, y = pneumonia, label = pneumonia)) +
  geom_line(aes(x = season_start, y = all_deaths)) +
  geom_text(aes(x = season_start, y = all_deaths, label = all_deaths))

print(p)


national_tidy <- national_seasons %>%
  gather(variable, value, flu:all_deaths)

p <- ggplot(national_tidy) +
  geom_line(aes(x = season_start, y = value, group = variable, color = variable)) +
  geom_text(aes(x = season_start, y = value, label = value))

print(p)

national_age <- df %>%
  filter(geoid == "National" & age != "All") %>%
  group_by(season_start, age) %>%
  summarize(flu = sum(flu), pneumonia = sum(pneumonia)) %>%
  gather(illness, deaths, flu:pneumonia)


p_age <- ggplot(national_age, aes(x = illness, y = deaths)) +
  geom_bar(aes(fill = illness), stat = "identity") +
  facet_grid(age~season_start)


print(p_age)
