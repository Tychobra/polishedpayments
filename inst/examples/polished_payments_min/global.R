library(shiny)
library(polished)
library(RPostgres)
library(DBI)
#library(polishedpayments)

app_config <- config::get()

global_sessions_config(
  api_key = app_config$api_key,
  app_name = "polished_payments_min",
  api_url = app_config$api_url
)

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = app_config$db$dbname,
  host = app_config$db$host,
  password = app_config$db$password,
  user = app_config$db$user
)

onStop(function() {
  DBI::dbDisconnect(conn)
})

