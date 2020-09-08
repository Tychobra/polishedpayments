#' @noRd
pricing_module_ui <- function(id) {
  ns <- NS(id)

  all_prices <- app_config$stripe$prices

  price_box_width <-  6 / length(all_prices)

  shiny::fluidRow(
    style = "margin-top: 100px",
    class = "text-center",
    shiny::column(3),
    lapply(all_prices, function(price) {
      price_box_module_ui(
        ns(price),
        width = price_box_width
      )
    }),
    column(
      12,
      br(),
      br(),
      "All prices are in USD and must be paid with a valid credit card. Taxes may apply."
    )
  )
}




#' @noRd
pricing_module <- function(input, output, session, sub_info) {
  ns <- session$ns

  sel_plan <- reactiveVal(NULL)

  # highlight the user's subscription if the user is signed up for a subscription

  disclaimer_text = p(
    class = "text-center",
    "The Monthly Plan grants you full access to the Shiny app",
    tags$br(),
    "By clicking Submitting, you agree to authorize us to
    collect payments in accordance with the terms of this plan."
  )

  all_pricing_plans <- app_config$stripe$prices



  lapply(all_pricing_plans, function(pricing_plan) {
    callModule(
      price_box_module,
      id = pricing_plan,
      plan_id = pricing_plan,
      sub_info = sub_info,
      disclaimer_text = disclaimer_text
    )
  })

}
