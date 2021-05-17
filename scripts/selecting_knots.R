source(here::here("scripts", "library.R"))

start_date = "2020-04-26"
end_date = "2021-02-28"

case_data <- read_csv(here("data", "cases_2021-05-17.csv")) %>%
  filter(date >= start_date & date <= end_date) %>%
  mutate(date = as.Date(date),
         new_case = newCasesBySpecimenDate,
         rate_group = factor(case_when(date <= "2020-04-29" ~ "Rate 1",
                                date > "2020-04-29" & date <= "2020-07-19" ~ "Rate 2",
                                date > "2020-07-19" & date <= "2020-09-15" ~ "Rate 3",
                                date > "2020-09-15" & date <= "2020-11-02" ~ "Rate 4",
                                date > "2020-11-02" & date <= "2020-11-29" ~ "Rate 5",
                                date > "2020-11-29" & date <= "2020-12-30" ~ "Rate 6",
                                date > "2020-12-30" & date <= "2021-02-05" ~ "Rate 7",
                                date > "2021-02-05" & date <= "2021-02-28" ~ "Rate 8",
                                TRUE ~ "Other"))) %>%
  select(date, new_case, rate_group)

ggplot(case_data, aes(x = date, y = new_case, fill = rate_group)) +
  geom_col() +
  geom_smooth(method = "lm", colour = "black") +
  theme_minimal() +
  labs(x = element_blank(),
       y = "Number of cases",
       fill = "Rate changes")

plotly::plot_ly(
  x = case_data$date,
  y = case_data$new_case,
  color = case_data$rate_group,
  type = "bar"
)



# Using a smooth spline function ------------------------------------------

auto_knots <- smooth.spline(x = as.numeric(case_data$date), y = case_data$new_case,
                            keep.data = T, nknots = 12)
pred <- predict(auto_knots, as.numeric(case_data$date))

knots <- tibble(knot = unique(auto_knots$fit$knot)) %>%
  mutate(xintercept = ((max(pred$x) - min(pred$x)) * knot) + min(pred$x))

ggplot() +
  geom_point(aes(x = as.numeric(case_data$date), y = case_data$new_case)) +
  geom_line(aes(x = pred$x, y = pred$y)) +
  geom_vline(aes(xintercept = knots$xintercept)) +
  theme_minimal() +
  labs(x = "Date",
       y = "Number of cases") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())



