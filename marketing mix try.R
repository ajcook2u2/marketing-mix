library(Robyn)
library(readr)
library(dplyr)
library(lubridate)
library(jsonlite)

Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

#option to create files locally
create_files <- TRUE
robyn_directory <- "D:/downloads/kaggle/marketing mix"

#load in the dataset
setwd("D:/downloads/kaggle/marketing mix")
data <- read_csv("marketing_mix.csv")
data <- data %>%
  rename('Google_Ads' = 'Google Ads')
data$Date <- mdy(data$Date)

holidays <- dt_prophet_holidays

head(data)

InputCollect <- robyn_inputs(
  dt_input = data,
  dt_holidays = holidays,
  date_var = 'Date',
  dep_var = 'Sales',
  paid_media_spends = c('TikTok', 'Facebook', 'Google_Ads'),
  paid_media_vars = c('TikTok', 'Facebook', 'Google_Ads'),
  prophet_country = 'US',
  window_start = '2018-1-7',
  window_end = '2021-10-31',
  adstock = 'geometric',
  dep_var_type = "conversion" #conversion or revenue
)
print(InputCollect)

#hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

plot_adstock(plot = TRUE)
plot_saturation(plot = TRUE)

hyper_limits()

hyperparameters <- list(
  Facebook_alphas = c(0.5, 3),
  Facebook_gammas = c(0.3, 1),
  Facebook_thetas = c(0, 0.3),
  Google_Ads_alphas = c(0.5, 3),
  Google_Ads_gammas = c(0.3, 1),
  Google_Ads_thetas = c(0, 0.3),
  TikTok_alphas = c(0.5, 3),
  TikTok_gammas = c(0.3, 1),
  TikTok_thetas = c(0, 0.3),
  train_size = c(0.5, 0.8)
)

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)

if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS, plot)
}

OutputModels <- robyn_run(
  InputCollect = InputCollect,
  cores = NULL,
  iterations = 2000,
  trials = 5,
  ts_validation = TRUE,
  add_penalty_factor = FALSE
)

print(OutputModels)

OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = 'auto',
  csv_out = 'all',
  plot_folder = robyn_directory,
  plot_pareto = create_files
)

print(OutputCollect)

select_model <- '5_86_3'
exported_model <- robyn_write(InputCollect, OutputCollect, select_model, export = create_files)
print(exported_model)



AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = NULL, # Default last month as initial period
  # total_budget = NULL, # When NULL, default is total spend in date_range
  #channel_constr_low = 0.7,
  #channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  # channel_constr_multiplier = 3,
  scenario = "max_response",
  export = create_files
)
# Print & plot allocator's output
print(AllocatorCollect1)
plot(AllocatorCollect1)


robyn_write(InputCollect, OutputCollect, select_model = '5_86_3')
#coefficients <- coef(robyn_model = '5_86_3')


#response <- robyn_response(
#  InputCollect = InputCollect,
#  OutputCollect = OutputCollect,
#  metric_name = "sales",
#  metric_value = 50000
#)

robyn_write(InputCollect, OutputCollect, select_model = '5_86_3', quiet=FALSE)
model_json = fromJSON('D:/downloads/kaggle/marketing mix/Robyn_202309062111_init/RobynModel-5_86_3.json')
saveRDS(model_json, file = "D:/downloads/kaggle/marketing mix/Robyn_202309062111_init/model.RDS")
model <- robyn_load('D:/downloads/kaggle/marketing mix/Robyn_202309062111_init/model.RDS')
print(model)




json_file <- 'D:/downloads/kaggle/marketing mix/Robyn_202309062111_init/RobynModel-5_86_3.json'

# Optional: Manually read and check data stored in file
json_data <- robyn_read(json_file)
print(json_data)

holidays <- dt_prophet_holidays
# Re-create InputCollect
InputCollectX <- robyn_inputs(
  dt_input = data,
  dt_holidays = holidays,
  json_file = json_file)

# Re-create OutputCollect
OutputCollectX <- robyn_run(
  InputCollect = InputCollectX,
  json_file = json_file,
  export = create_files)

response <- robyn_response(
  InputCollect = InputCollectX,
  OutputCollect = OutputCollectX,
  metric_name = "TikTok",
  metric_value = 150000,
  select_model='5_86_3',
  date_range = 'all'
)



Theta <- 0.2875
Alpha <- 0.5457
Gamma <- 0.6126

grid <- seq(0,1,length.out=30000)

hillcurve <- function(x, alpha, gamma){
  alpha <- as.numeric(alpha)
  gamma <- as.numeric(gamma)
  x_sat <- (x^alpha)/(x^alpha + gamma^alpha)
}

dervitives <- function(x, y){
  dydx <- rep(NA, length(x))
  d2ydx2 <- rep(NA, length(x))
  for(i in 2:(length(x)-1)){
    dydx[i] = (y[i+1] - y[i-1])/(x[i+1] - x[i-1])
    d2ydx2[i] = (y[i+1] - 2*y[i] + y[i-1]) / ((x[i+1] - x[i])**2)
  }
  return (list(dydx, d2ydx2))
}

ys <- hillcurve(grid, Alpha, Gamma)

deriv <- dervitives(grid, ys)



