

#' Credit Card Modal to create a subscription
#'
#' @param price_id the Stripe price id
#' @param title the title to pass to \code{shiny::modalDialog}
#' @param size the size to pas to \code{shiny::modalDialog}
#' @param easyClose the easyClose to pass to \code{shiny::modalDialog}
#' @param fade the fade to pass to \code{shiny::modalDialog}
#'
#' @importFrom shiny showModal modalDialog tagList actionButton textInput callModule
#'
#' @export
#'
create_subscription_modal <- function(input, output, session,
  price_id,
  # modalDialog inputs
  title = NULL,
  size = "s",
  easyClose = FALSE,
  fade = TRUE
) {
  ns <- session$ns

  shiny::showModal(
    shiny::modalDialog(
      title = title,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(
          ns("submit"),
          "Submit",
          class = "btn-primary",
          style = "color: #FFF"
        )
      ),
      size = size,
      easyClose = easyClose,
      fade = fade,
      shiny::textInput(
        ns("cc_name"),
        "Cardholder Name",
        width = "100%"
      ),
      credit_card_module_ui(ns("cc_input"))
    )
  )


  shiny::callModule(
    credit_card_subscription_module,
    ns("cc_input"),
    trigger = reactive(input[[ns("submit")]]),
    price_id = price_id,
    billing_detail = reactive(list(
      name = reactive({input$cc_name})
    ))
  )

  invisible(NULL)
}