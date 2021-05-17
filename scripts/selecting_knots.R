source(here::here("scripts", "library.R"))

# Used to visually inspect changes in rate of cases

plotly::plot_ly(
  x = case_data$date,
  y = case_data$new_case,
  type = "bar"
)

# Start and end date of the planned analysis.
start_date = "2020-04-26"
end_date = "2021-02-28"


# Importing gov.uk case data and categorising the segments by date

case_data <- read_csv(here("data", "cases_2021-05-17.csv")) %>%
  filter(date >= start_date & date <= end_date) %>%
  mutate(date = as.Date(date),
         new_case = newCasesBySpecimenDate,
         Segment_group = factor(case_when(date <= "2020-04-29" ~ "Segment 1",
                                date > "2020-04-29" & date <= "2020-07-19" ~ "Segment 2",
                                date > "2020-07-19" & date <= "2020-09-15" ~ "Segment 3",
                                date > "2020-09-15" & date <= "2020-11-02" ~ "Segment 4",
                                date > "2020-11-02" & date <= "2020-11-29" ~ "Segment 5",
                                date > "2020-11-29" & date <= "2020-12-30" ~ "Segment 6",
                                date > "2020-12-30" & date <= "2021-02-05" ~ "Segment 7",
                                date > "2021-02-05" & date <= "2021-02-28" ~ "Segment 8",
                                TRUE ~ "Other"))) %>%
  select(date, new_case, Segment_group)

# Plotting the segments to visually inspect trend

ggplot(case_data, aes(x = date, y = new_case, fill = Segment_group)) +
  geom_col() +
  geom_smooth(method = "lm", colour = "black") +
  theme_minimal() +
  labs(x = element_blank(),
       y = "Number of cases",
       fill = "Segment changes")



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



