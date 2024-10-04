library(tidyverse)

cocktails <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv"
)

write_csv(cocktails, "data/2020/2020-05-26/cocktails.csv")

cocktails <- cocktails |>
  mutate(ingredient = tolower(ingredient))


# Are name and row_id variables equivalent? -------------------------------


cocktails |>
  group_by(name) |>
  summarise(ids = n_distinct(row_id)) |>
  filter(ids > 1)


# One-ingredient cocktails

cocktails |>
  count(name) |>
  count(n)

cocktails |>
  group_by(name) |>
  filter(n() == 1) |>
  arrange(name)


# How big is each cocktail? -----------------------------------------------

cocktails |>
  count(measure, sort = TRUE)


# Filtering out the bitters -----------------------------------------------



sizes <- cocktails |>
  mutate(ingredient = tolower(ingredient)) |>
  filter(str_detect(measure, "oz")) |>
  filter(!str_detect(ingredient, "bitters")) |>
  mutate(
    oz = str_replace(measure, " oz", ""),
    oz = str_replace(oz, " ?1/2", ".5"),
    oz = str_replace(oz, " ?1/3", ".33"),
    oz = str_replace(oz, " ?1/4", ".25"),
    oz = str_replace(oz, " ? ?3/4", ".75"),
    oz = str_replace(oz, " ?2/3", ".66")
  ) |>
  mutate(oz = as.numeric(oz))


# Filtering for potentially incorrect data (ingredient amount too high) -------------



filter(sizes, oz > 10)

total_sizes_per_cocktail <- sizes |>
  group_by(name) |>
  summarise(n = n(), total_oz = sum(oz)) |>
  filter(total_oz < 20)

total_sizes_per_cocktail |>
  ggplot(aes(total_oz)) +
  geom_histogram(binwidth = .5)


# Shocking differences in semi_join results while using different pipes

total_sizes_per_cocktail |>
  filter(total_oz > 6) |>
  semi_join(cocktails, ., by = "name")


total_sizes_per_cocktail %>%
  filter(total_oz > 6) %>%
  semi_join(cocktails, ., by = "name")


# Semi_join using "%" pipe with "." as a function argument is almost equivalent
# to inner_join but with only joined column from x table

total_sizes_per_cocktail %>%
  filter(total_oz > 6) %>%
  semi_join(cocktails, ., by = "name") %>%
  View()


total_sizes_per_cocktail |>
  filter(total_oz > 6) |>
  inner_join(cocktails, by = "name") |>
  select(-c(n, total_oz)) |>
  View()

sizes |>
  group_by(ingredient) |>
  summarise(n = n(), mean_ingredient_oz = mean(oz)) |>
  filter(n > 5) |>
  arrange(desc(mean_ingredient_oz))


# What are most popular ingredients? --------------------------------------



cocktails |>
  count(ingredient = tolower(ingredient), sort = TRUE) |>
  head(20)

new_ingredient <- tibble(
  ingredient = c(
    "fresh lemon juice",
    "juice of a lemon",
    "fresh lime juice",
    "juice of a lime"
  ),
  standard_name = c("lemon juice", "lemon juice", "lime juice", "lime juice")
)

ingredient_changes <- cocktails |>
  select(name, ingredient, ingredient_number) |>
  right_join(new_ingredient) |>
  select(name, ingredient_number, ingredient = standard_name)

cocktails |>
  rows_update(ingredient_changes, by = c("name", "ingredient_number")) |>
  count(ingredient, sort = TRUE) |>
  head(20)


# Trying a bunch of tidying tips from yt comments -------------------------


cocktails |>
  mutate(oz = case_when(str_detect(measure, "oz") ~ str_replace(measure, " ?oz", ""))) |>
  mutate(oz = str_replace(oz, " ", "+")) |>
  rowwise() |>
  mutate(oz = eval(parse(text = oz))) |>
  count(oz, sort = TRUE)

cocktails |>
  mutate(ingredient = str_replace(ingredient, fixed("fresh", ignore_case = TRUE), "")) |>
  mutate(ingredient = str_trim(ingredient)) |>
  mutate(ingredient = str_to_sentence(ingredient)) |> 
  count(ingredient, sort = TRUE) |> 
  head(20)
