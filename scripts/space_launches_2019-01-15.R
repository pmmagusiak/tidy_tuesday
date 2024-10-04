library(tidyverse)
library(countrycode)

theme_set(theme_light())


agencies <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/agencies.csv"
)
launches <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv"
)

write_csv(agencies, "data/2019/2019-01-15/agencies.csv")
write_csv(launches, "data/2019/2019-01-15/launches.csv")

launches |>
  View()

agencies |>
  count(state_code, wt = count, sort = TRUE)

launches_processed <- launches |>
  filter(launch_date < Sys.Date()) |>
  mutate(state_code_cleaned = fct_collapse(
    state_code,
    "RU" = c("SU", "RU"),
    "IT" = "I",
    "FR" = "F",
    "JP" = "J"
  )) |>
  mutate(
    state_name = countrycode(state_code_cleaned, "iso2c", "country.name"),
    state_name = fct_lump(state_name, 6)
  ) |>
  replace_na(list(state_name = "Other"))

launches_processed |>
  count(launch_year, state_name) |>
  mutate(state_name = fct_reorder(state_name, -n, sum)) |>
  ggplot(aes(launch_year, n, color = state_name)) +
  geom_line() +
  labs(
    x = "Time",
    y = "Launches per year",
    color = "Responsible state",
    title = "Launches per year per country",
    subtitles = "Combines Soviet Union (pre-1990) with Russia"
  )

private_and_startup_launches <- launches_processed |>
  filter(agency_type %in% c("private", "startup")) |>
  inner_join(agencies |>
               select(agency, agency_name = name, short_name, parent),
             by = "agency") |>
  mutate(
    agency_name_lumped = fct_lump(agency_name, 6),
    agency_name_lumped = case_when(
      agency_name_lumped == "Other" &
        state_name == "United States" ~ "Other US",
      .default = agency_name_lumped
    )
  )

private_and_startup_launches |>
  count(agency_name_lumped, state_name, sort = TRUE) |>
  mutate(agency_name_lumped = fct_reorder(agency_name_lumped, n, sum)) |>
  ggplot(aes(agency_name_lumped, n, fill = state_name)) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = "no of launches",
       fill = "Country",
       title = "Most active private and startup agencies")

private_and_startup_launches |>
  count(agency_name_lumped,
        decade = 5 * (launch_year %/% 5),
        sort = TRUE) |>
  mutate(agency_name_lumped = fct_reorder(agency_name_lumped, -n, sum)) |>
  complete(decade, agency_name_lumped, fill = list(n = 0)) |>
  ggplot(aes(decade, n, color = agency_name_lumped)) +
  geom_line() +
  facet_wrap( ~ agency_name_lumped) +
  theme(legend.position = "none") +
  labs(x = "time", y = "# launches in 5 years period", title = "Agencies activity per 5-year interval")

vehicles <- launches_processed |>
  group_by(type, state_name) |>
  summarise(
    first_launch = min(launch_year),
    last_launch = max(launch_year),
    launches = n()
  ) |>
  arrange(desc(launches))



Russian_vehicles <- vehicles |>
  filter(state_name == "Russia", launches > 50)

launches_processed |>
  semi_join(Russian_vehicles, by = "type") |>
  mutate(type = fct_reorder(type, launch_date, min)) |>
  ggplot(aes(launch_date, type)) +
  geom_jitter(alpha = .25,
              width = 0,
              height = 0.2) +
  theme(legend.position = "none") +
  labs(
    x = "Launch date",
    y = "Vehicles",
    title = "Timeline of russian space vehicles",
    subtitle = "Only vehicles of > 50 launches"
  )

launches_processed |>
  filter(state_code_cleaned == "US") |>
  add_count(type) |>
  filter(n > 30) |>
  mutate(type = fct_reorder(type, launch_date, min),
         agency_type = str_to_title(agency_type)) |>
  ggplot(aes(launch_date, type, color = agency_type)) +
  geom_jitter(alpha = .25,
              width = 0,
              height = 0.2) +
  labs(
    x = "Launch date",
    y = "Vehicles",
    color = "Agency type",
    title = "Timeline of US space vehicles",
    subtitle = "Only vehicles of > 30 launches"
  )
