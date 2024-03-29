library(shiny)
library(magrittr)
library(shinyFeedback)
library(polished)
library(polishedpayments)

app_config <- config::get()

polished:::set_api_url(
  api_url = "https://auth-api-dev.polished.tech/v1"
)

polished_config(
  api_key = app_config$api_key,
  app_name = "polished_payments_min",
  is_invite_required = FALSE
)

polished_payments_config(
  stripe_secret_key = app_config$stripe$keys$secret,
  stripe_public_key = app_config$stripe$keys$public,
  subscription_prices = app_config$stripe$prices,
  trial_period_days = 30
)
