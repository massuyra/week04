pacman::p_load(       
  tidyverse,
  janitor,
  skimr,
  glue, #combining strings and objects
  gapminder, # dataset
  ggplot2, #plotting
  gridExtra, #arranging plots
  kableExtra,
  flextable
)
emdat <- read.csv("EMDAT.csv")
affected <- clean_names(emdat) %>%
  select("entity", "year", "deaths_all_disasters", "injured_all_disasters", "homeless_all_disasters")
view(affected)
avg_dih <- affected %>%
  filter(!entity %in% c("World", "Soviet Union")) %>%
  group_by(entity) %>%
  summarize(
    avg_deaths = mean(deaths_all_disasters, na.rm = TRUE) %>% round(2),
    avg_injured = mean(injured_all_disasters, na.rm = TRUE) %>% round(2),
    avg_homeless = mean(homeless_all_disasters, na.rm = TRUE) %>% round(2)
  )
rank <- function(data, x, y){
  data %>%
    select({{x}}, {{y}}) %>%
    arrange(desc({{y}})) %>%
    head(10)
}
deaths_10 <- rank(avg_dih, entity, avg_deaths)
injured_10 <- rank(avg_dih, entity, avg_injured)
homeless_10 <- rank(avg_dih, entity, avg_homeless)

deaths_wider <- affected %>%
  select(entity, year, deaths_all_disasters) %>%
  pivot_wider(
    names_from = year,
    values_from = deaths_all_disasters
  )
per_year <- function(x){
  affected %>%
    select(entity, year, x) %>%
    pivot_wider(
      names_from = year,
      values_from = x
    )
}
deahts_per_year <- per_year("deaths_all_disasters")
injured_per_year <- per_year("injured_all_disasters")
homeless_per_year <- per_year("homeless_all_disasters")

columns_to_process <- c("deaths_all_disasters", "injured_all_disasters", "homeless_all_disasters")
result_list <- map(columns_to_process, ~per_year(.x))
kable(result_list) %>%
  kable_styling()

filtered_affected <- affected %>% 
  filter(entity %in% c("Afghanistan", "Belarus","Jamaica", "Armenia", "Romania"),
         deaths_all_disasters < 2500,
         injured_all_disasters < 2500,
         homeless_all_disasters < 30000
         )

create_point_plot <- function(i) {
  filtered_affected %>%
    ggplot(aes(x = names(filtered_affected)[2], y = names(filtered_affected)[i])) +
    geom_point() +
    labs(
      title = glue("Relationship between year and {names(affected)[i]}"),
      y = glue("{names(affected)[i]}")
    )
}
plots_list <- map(3:ncol(filtered_affected), create_point_plot)
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2) # Adjust ncol as needed


create_point_plot <- function(i) {
  col_name_x <- names(filtered_affected)[2]
  col_name_y <- names(filtered_affected)[i]
  
  ggplot(filtered_affected, aes(x = .data[[col_name_x]], y = .data[[col_name_y]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(
      title = glue::glue("Relationship between {col_name_x} and {col_name_y}"),
      y = col_name_y
    )
}

plots_list <- map(3:ncol(filtered_affected), create_point_plot)

plots_grid <- grid.arrange(grobs = plots_list, ncol = 2)

