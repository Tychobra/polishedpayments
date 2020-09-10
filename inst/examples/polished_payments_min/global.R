library(shiny)
library(polished)
library(polishedpayments)
library(waiter)

app_config <- config::get()

polished_payments_config(app_config$stripe)

global_sessions_config(
  api_key = app_config$api_key,
  app_name = "polished_payments_min",
  api_url = app_config$api_url
)
