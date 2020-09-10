#' @noRd
pricing_module_ui <- function(id) {
  ns <- NS(id)

  all_prices <- getOption("pp")$prices

  n_prices <- length(all_prices)

  if (!(n_prices %in% 1:4)) {
    stop("you must supply between 1 and 4 prices", call. = FALSE)
  }

  if (n_prices == 1) {
    gutter <- shiny::column(4)
    price_box_width <- 4
  } else if (n_prices == 2) {
    gutter <- shiny::column(2)
    price_box_width <- 4
  } else if (n_prices == 3) {
    gutter <- NULL
    price_box_width <- 4
  } else if (n_prices == 4) {
    gutter <- NULL
    price_box_width <- 3
  }

  shiny::fluidRow(
    style = "margin-top: 100px",
    class = "text-center",
    gutter,
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

  all_pricing_plans <- getOption("pp")$prices



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
