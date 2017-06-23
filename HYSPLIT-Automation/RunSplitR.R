library(SplitR)
library(magrittr)


# Create the `dispersion_model` object, add
# a grid of starting locations, add run
# parameters, and then execute the model run
dispersion_model <-
  create_disp_model() %>%
  add_emissions(
    rate = 1,
    duration = 72,
    start_day = "2012-02-01",
    start_hour = 0) %>%
  add_species(
    pdiam = 0.00065,
    density = 0.001977,
    shape_factor = 1) %>%
  add_grid(
    range = c(10, 10),
    division = c(0.05, 0.05)) %>%
  add_params(
    lat = 39.2846,
    lon = -96.1150,
    height = 200,
    duration = 72,
    start_day = "2012-02-01",
    start_hour = 0,
    direction = "forward",
    met_type = "reanalysis") %>%
  run_model

dispersion_df <-
  dispersion_model %>% get_output_df

dispersion_model %>% dispersion_plot