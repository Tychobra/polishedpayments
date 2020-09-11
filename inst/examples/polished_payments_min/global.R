library(shiny)
library(polished)
library(polishedpayments)
library(waiter)

app_config <- config::get()

polished_payments_config(
  stripe_secret_key = app_config$stripe$keys$secret,
  stripe_public_key = app_config$stripe$keys$public,
  stripe_prices = app_config$stripe$prices,
  trial_period_days = 30,
  free_roles = "free_user"
)

global_sessions_config(
  api_key = app_config$api_key,
  app_name = "polished_payments_min",
  api_url = app_config$api_url
)
