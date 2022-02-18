library(shiny)
library(magrittr)
library(shinyFeedback)
library(polished)
library(polishedpayments)

app_config <- config::get()

polished_payments_config(
  stripe_secret_key = app_config$stripe$keys$secret,
  stripe_public_key = app_config$stripe$keys$public
)

polished:::set_api_url(
  api_url = "https://auth-api-dev.polished.tech/v1"
)

polished_config(
  api_key = app_config$api_key,
  app_name = "payment_only",
  is_invite_required = FALSE,
  is_auth_required = FALSE
)
