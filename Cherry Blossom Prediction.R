### STAT 490 Cherry Blossom Assignment
# Areeb Ehtisham

#--Setup--
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(rnoaa)
library(lubridate)

#--Data--
cherry <- read.csv("C:/Users/thegr/Downloads/peak-bloom-prediction-main/data/washingtondc.csv") %>% 
  bind_rows(read.csv("C:/Users/thegr/Downloads/peak-bloom-prediction-main/data/liestal.csv")) %>% 
  bind_rows(read.csv("C:/Users/thegr/Downloads/peak-bloom-prediction-main/data/kyoto.csv"))

stations <- ghcnd_stations()

#--Temperature--

get_temperature <- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmax"), 
               date_min = "1950-01-01", date_max = "2022-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(strftime(date, '%m')) %% 12,
           season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include.lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Fall")),
           year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, season) %>%
    summarize(tmax_avg = mean(tmax, na.rm = TRUE))
}

historic_temperatures <-
  tibble(location = "washingtondc", get_temperature("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_temperature("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_temperature("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_temperature("CA001108395")))

ls_fit_temperature <- lm(tmax_avg ~ year * season + location, 
                         data = historic_temperatures)
summary(ls_fit_temperature)

temperature_predictions <-
  expand_grid(location = c("washingtondc", "liestal", "kyoto", "vancouver" ),
              season = c("Winter", "Spring", "Summer", "Fall"),
              year = 1950:2032) %>%
  bind_cols(predicted_temperature = 
              predict(ls_fit_temperature, newdata = .)) %>%
  filter(season %in% c("Winter", "Spring")) %>%
  pivot_wider(names_from = season, values_from = predicted_temperature)

#--Precipitation--

get_prcp <- function(stationid) {
  ghcnd_search(stationid = stationid, var = c("prcp"), 
               date_min = "1950-01-01", date_max = "2022-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(strftime(date, '%m')) %% 12,
           season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include.lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Fall")),
           year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, season) %>%
    summarize(prcp_avg = mean(prcp, na.rm = TRUE))
}

historic_prcp <-
  tibble(location = "washingtondc", get_prcp("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_prcp("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_prcp("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_prcp("CA001108395")))

ls_fit_prcp <- lm(prcp_avg ~ year * season + location, 
                         data = historic_prcp)
summary(ls_fit_prcp)

prcp_predictions <-
  expand_grid(location = c("washingtondc", "liestal", "kyoto", "vancouver" ),
              season = c("Winter", "Spring", "Summer", "Fall"),
              year = 1950:2032) %>%
  bind_cols(predicted_precip = 
              predict(ls_fit_prcp, newdata = .)) %>%
  filter(season %in% c("Winter", "Spring")) %>%
  pivot_wider(names_from = season, values_from = predicted_precip)
colnames(prcp_predictions) <- c("location","year","WinterP","SpringP")


#--Predictions--

pred_full <- prcp_predictions %>% 
  left_join(temperature_predictions,
            by = c('location','year'))

predictions <- pred_full %>%
  right_join(cherry,
            by = c("location", "year")) %>%
  lm(bloom_doy ~ Spring * Winter + I(SpringP * WinterP), data = .) %>%
  predict(newdata = pred_full) %>%
  round() %>%
  bind_cols(predicted_doy = ., pred_full)
predictions


submission_predictions <- predictions %>% 
  filter(year > 2022) %>%
  mutate(predicted_doy = round(predicted_doy)) %>%
  select(predicted_doy, year, location)

submission_predictions
View(submission_predictions)

write.csv(submission_predictions, file = "cherry-predictions.csv",
          row.names = F)
