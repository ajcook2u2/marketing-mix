library(Robyn)
library(readr)
library(dplyr)
library(lubridate)
library(jsonlite)
library(ggplot2)
library(caTools)

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
  json_file  = model_json,
  dt_input = data,
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

prophet_vars <- c('TikTok', 'Google_Ads', 'Facebook')
RobynRefresh <- robyn_refresh(
  json_file = model,
  dt_input = data,
  dt_holidays = holidays,
  refresh_steps = 13,
  refresh_iters = 1000, # 1k is an estimation
  refresh_trials = 1,
  dep_var = 'Sales',
  dep_var_type = 'conversion',
  paid_media_spends = prophet_vars,
  adstock = 'geometric'
  )

inputcollectX = model$Robyn$InputCollect
outputcollectX = model$Robyn$OutputCollect
select_modelX = model$Robyn$ExportedModel$select_model



#    "hyper_values": 
#"Facebook_alphas": [0.9271],
#"Facebook_gammas": [0.6296],
#"Facebook_thetas": [0.237],
#"Google_Ads_alphas": [2.9023],
#Google_Ads_gammas": [0.9588],
#"Google_Ads_thetas": [0.251],
#"lambda": [280.596],
#"TikTok_alphas": [0.5457],
#"TikTok_gammas": [0.6126],
#"TikTok_thetas": [0.2875],
#"train_size": [0.7707]

Theta <- 0.251
Alpha <- 2.9023
Gamma <- 0.9588

grid <- seq(0,2,length.out=60000)

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
deriv[[1]] <- na.omit(deriv[[1]])
deriv[[2]] <- na.omit(deriv[[2]])

plain <- data.frame(grid=grid, ys=ys)

deriv_df1 <- data.frame(deriv=deriv[[1]])
deriv_df1 <- deriv_df1 %>%
  mutate(rownumber=row_number())

deriv_df2 <- data.frame(deriv=deriv[[2]])
deriv_df2 <- deriv_df2 %>%
  mutate(rownumber=row_number())

ggplot(plain) + geom_point(aes(x=grid, y=ys))
ggplot(deriv_df1) + geom_point(aes(x=rownumber, y=deriv))
ggplot(deriv_df2) + geom_point(aes(x=rownumber, y=deriv))

x_min <- ys[which.min(deriv[[2]])]
x_max <- ys[which.max(deriv[[2]])]

tiktok <- OutputCollectX[["mediaVecCollect"]]$Google_Ads
build <- x_max*(max(tiktok) - min(tiktok)) + min(tiktok)
saturated <- x_min*(max(tiktok) - min(tiktok)) + min(tiktok)          

