#' @noRd
plans_box_module_ui <- function(id) {
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

  shinydashboard::box(
    title = "Plans",
    width = 12,
    class = "text-center",
    background = "navy",
    gutter,
    lapply(all_prices, function(price) {
      plan_column_module_ui(
        ns(price),
        width = price_box_width
      )
    })
  )
}




#' @noRd
#'
#' @importFrom htmltools tags
#'
plans_box_module <- function(input, output, session, sub_info) {
  ns <- session$ns

  sel_plan <- reactiveVal(NULL)

  # highlight the user's subscription if the user is signed up for a subscription

  disclaimer_text = tags$p(
    class = "text-center",
    "This plan grants you full access to the ", getOption('polished')$app_name_display, " app",
    tags$br(),
    "By clicking Submit, you agree to authorize collection of payments in accordance with the terms of this plan."
  )

  all_pricing_plans <- getOption("pp")$prices



  lapply(seq_along(all_pricing_plans), function(i) {

    callModule(
      plan_column_module,
      id = all_pricing_plans[i],
      plan_id = all_pricing_plans[i],
      sub_info = sub_info,
      disclaimer_text = disclaimer_text,
      hide_waiter = if (i == length(all_pricing_plans)) TRUE else FALSE
    )
  })
}
