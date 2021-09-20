

#' Credit Card Modal to create a subscription
#'
#' @param price_id the Stripe price id
#' @param title the title to pass to \code{shiny::modalDialog}
#' @param size the size to pas to \code{shiny::modalDialog}
#' @param easyClose the easyClose to pass to \code{shiny::modalDialog}
#' @param easyClose the fade to pass to \code{shiny::modalDialog}
#'
#' @export
#'
create_subscription_modal <- function(input, output, session,
  price_id,
  default_payment_method = NA,
  # modalDialog inputs
  title = NULL,
  size = "s",
  easyClose = FALSE,
  fade = TRUE
) {
  ns <- session$ns

  showModal(
    modalDialog(
      title = title,
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          ns("submit"),
          "Submit",
          class = "btn-primary",
          style = "color: #FFF"
        )
      ),
      size = size,
      easyClose = easyClose,
      fade = fade,
      credit_card_module_ui(ns("cc_input"))
    )
  )


  callModule(
    credit_card_subscription_module,
    ns("cc_input"),
    trigger = reactive(input[[ns("submit")]]),
    price_id = price_id
  )

  invisible(NULL)
}