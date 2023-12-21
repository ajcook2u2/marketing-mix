library(Robyn)
library(readr)
library(dplyr)
library(lubridate)
library(jsonlite)
library(ggplot2)
library(caTools)

setwd("D:/downloads/kaggle/marketing mix")
data <- read_csv("marketing_mix.csv")
data <- data %>%
  rename('Google_Ads' = 'Google Ads')
data$Date <- mdy(data$Date)

holidays <- dt_prophet_holidays

head(data)

InputCollect <- robyn_inputs(
  date_var = 'Date',
  dep_var = 'Sales',
  dt_input = data,
  dt_holidays = holidays,
  paid_media_spends = c('TikTok', 'Facebook', 'Google_Ads'),
  paid_media_vars = c('TikTok', 'Facebook', 'Google_Ads'),
  prophet_country = 'US',
  window_start = '2018-1-7',
  window_end = '2021-10-31',
  adstock = 'geometric',
  dep_var_type = 'conversion')

OutputModels <- robyn_run(InputCollect = InputCollect, 
                          cores = NULL, iterations = 2000, 
                          trials = 5, ts_validation = TRUE,
                          add_penalty_factor = FALSE)

saveRDS(OutputModels, file = "my_data.rds")
loaded_data <- readRDS("my_data.rds")

OutputCollect <- robyn_outputs(
  InputCollect, loaded_data,
  pareto_fronts = "auto", # automatically pick how many pareto-fronts to fill min_candidates (100)
  min_candidates = 100, # top pareto models for clustering. Default to 100
  calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  export = create_files, # this will create files locally
  plot_folder = robyn_directory, # path for plots exports and files creation
  plot_pareto = create_files # Set to FALSE to deactivate plotting and saving model one-pagers
)

#load in previous outputcollect
saveRDS(OutputCollect, file = "outputcollect.rds")
OutputCollect <- readRDS("outputcollect.rds")

select_model <- '5_86_3'

AllocatorCollect3 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = NULL, # Default last month as initial period
  scenario = "max_response",
  # target_value = 2, # Customize target ROAS or CPA value
  #'target_efficiency' for CPA or 'max_response' 
  export = create_files
)

tiktok_curve <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = 'TikTok',
  metric_value = 2000,
  date_range = "last_5"
)
tiktok_curve_data <- tiktok_curve$plot$data

google_curve <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = 'Google_Ads',
  metric_value = 20000,
  date_range = "last_5"
)
google_curve_data <- google_curve$plot$data


facebook_curve <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = 'Facebook',
  metric_value = 2000,
  date_range = "last_5"
)
facebook_curve_data <- facebook_curve$plot$data


alpha <- subset(OutputCollect[["resultHypParam"]]$TikTok_alphas, OutputCollect[["resultHypParam"]]$solID == '5_86_3')
gamma <- subset(OutputCollect[["resultHypParam"]]$TikTok_gammas, OutputCollect[["resultHypParam"]]$solID == '5_86_3')
theta <- subset(OutputCollect[["resultHypParam"]]$TikTok_thetas, OutputCollect[["resultHypParam"]]$solID == '5_86_3')


x <- seq(0, 1, length.out = 500)
y <- numeric(length(x))

#generates a saturation curve
for (i in 1:length(x)) {
  y[i] <- (x1^alpha) / (x1^alpha + gamma^alpha)
}

#scales the curve
tactic_spend <- tiktok_curve$plot$data$metric
tactic_response <- tiktok_curve$plot$data$response
expected_spend <- x * (max(tactic_spend) - min(tactic_spend)) + min(tactic_spend)
response <- y * (max(tactic_response) - min(tactic_response)) + min(tactic_response)
response <- response * (max(tactic_response) / max(response))


plot_data <- data.frame(x = expected_spend, y = response, response = response)
library(ggplot2)

# Create a line plot
ggplot(plot_data, aes(x = expected_spend, y = response)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Saturation Curve",
       x = "Spend Levels",
       y = "Saturation Curve Values")

google_model <- lm(response ~ poly(metric, degree = 18), data = google_curve_data)
google_curve_data$y_pred <- predict(google_model, newdata = google_curve_data)
summary(google_model)

ggplot() +
  geom_point(data = google_curve_data, aes(metric, response), color = "blue", size = 3) +
  geom_line(data = google_curve_data, aes(metric, y_pred), color = "red", size = 1.5) +
  labs(title = "Polynomial Regression",
       x = "Independent Variable (x)",
       y = "Dependent Variable (y)") +
  theme_minimal()

facebook_model <- lm(response ~ poly(metric, degree = 18), data = facebook_curve_data)
facebook_curve_data$y_pred <- predict(facebook_model, newdata = facebook_curve_data)
summary(facebook_model)

ggplot() +
  geom_point(data = facebook_curve_data, aes(metric, response), color = "blue", size = 3) +
  geom_line(data = facebook_curve_data, aes(metric, y_pred), color = "red", size = 1.5) +
  labs(title = "Polynomial Regression",
       x = "Independent Variable (x)",
       y = "Dependent Variable (y)") +
  theme_minimal()

tiktok_model <- lm(response ~ poly(y, degree = 18), data = plot_data)
plot_data$y_pred <- predict(tiktok_model, newdata = plot_data)
summary(tiktok_model)

ggplot() +
  geom_point(data = tiktok_curve_data, aes(metric, response), color = "blue", size = 3) +
  geom_line(data = plot_data, aes(x, y_pred), color = "red", size = 1.5) +
  labs(title = "Polynomial Regression",
       x = "Independent Variable (x)",
       y = "Dependent Variable (y)") +
  theme_minimal()

