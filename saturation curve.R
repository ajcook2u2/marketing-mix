library(Robyn)
library(readr)
library(dplyr)
library(lubridate)
library(jsonlite)
library(ggplot2)

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

holding <- list()
holding1 <- list()
for (i in 1:50000) {
  stock <- adstock_geometric(i, 0.2875)
  stock <- stock$x_decayed
  alpha <- 0.5457
  theta <- 0.2875
  gamma <- 0.6126
  saturation <- (stock^alpha) / ((stock^alpha) + (gamma^alpha))
  holding <- append(holding, i)
  holding1 <- append(holding1, saturation)
  print(i)
}


alphas <- list()
gammas <- list()
thetas <- list()

tiktok_alphas <- json_data$ExportedModel$hyper_values$TikTok_alphas
tiktok_gammas <- json_data$ExportedModel$hyper_values$TikTok_gammas
tiktok_thetas <- json_data$ExportedModel$hyper_values$TikTok_thetas

alphas <- append(alphas, tiktok_alphas)
gammas <- append(gammas, tiktok_gammas)
thetas <- append(thetas, tiktok_thetas)

google_ads_alphas <- json_data$ExportedModel$hyper_values$Google_Ads_alphas
google_ads_gammas <- json_data$ExportedModel$hyper_values$Google_Ads_gammas
google_ads_thetas <- json_data$ExportedModel$hyper_values$Google_Ads_thetas

alphas <- append(alphas, google_ads_alphas)
gammas <- append(gammas, google_ads_gammas)
thetas <- append(thetas, google_ads_thetas)

facebook_alphas <- json_data$ExportedModel$hyper_values$Facebook_alphas
facebook_gammas <- json_data$ExportedModel$hyper_values$Facebook_gammas
facebook_thetas <- json_data$ExportedModel$hyper_values$Facebook_thetas

alphas <- append(alphas, facebook_alphas)
gammas <- append(gammas, facebook_gammas)
thetas <- append(thetas, facebook_thetas)

alphas <- unlist(alphas)
gammas <- unlist(gammas)
thetas <- unlist(thetas)
holding <- list()
holding1 <- list()
spend_colname <- c('tiktok_spend', 'google_ads_spend', 'facebook_spend')
response_colname <- c('tiktok_response', 'google_ads_response', 'facebook_response')
deriv_colname <- c('tiktok_deriv', 'google_ads_deriv', 'facebook_deriv')

m = 1
for (z in alphas) {
  holding <- list()
  holding1 <- list()
for (i in 1:50000) {
  stock <- adstock_geometric(i, 0.2875)
  stock <- stock$x_decayed
  alpha <- alphas[m]
  theta <- thetas[m]
  gamma <- gammas[m]
  saturation <- (stock^alpha) / ((stock^alpha) + (gamma^alpha))
  holding1 <- append(holding1, saturation)
  print(i)
}
  holding1 <- unlist(holding1)
  if (m == 1) {
    tiktok_saturation <- holding1
  }
  if (m == 2) {
    google_ads_saturation <- holding1
  }
  if (m == 3) {
    facebook_saturation <- holding1
  }
  m <- m + 1
  }

tiktok_deriv <- diff(tiktok_saturation)
google_ads_deriv <- diff(google_ads_saturation)
facebook_deriv <- diff(facebook_saturation)
facebook_deriv1 <- diff(facebook_deriv)
deriv <- data.frame(
  facebook <- google_ads_saturation)
deriv$index <- seq_along(deriv$facebook)

ggplot(deriv, aes(index, facebook)) + geom_point()

