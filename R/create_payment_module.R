#' UI element to create a Stripe Credit Card payment
#'
#' @param id The element's namespaced ID
#'
#' @export
create_payment_module_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    useShinyFeedback(),
    tags$script(src = "polishedpayments/js/create_payment_module.js"),
    tags$script(paste0("create_payment_module('", ns(''), "')"))
  )
}

#' Server logic to create a Stripe credit card payment
#'
#' @param input shiny server input
#' @param output shiny server output
#' @param session shiny server session
#' @param open_modal_trigger The trigger to open the Credit Card modal
#' @param disclaimer_text the Disclaimer text
#' @param payment_amount the amount of the one time payment
#' @param currency the currency of the one time payment
#'
#' @export
#'
create_payment_module <- function(
  input, output, session,
  open_modal_trigger = reactive(0),
  disclaimer_text = "Disclaimer",
  payment_amount = NULL,
  currency = "usd"
) {
  ns <- session$ns

  # The Setup (Subscription) or Payment (One-time-payment) Intent ID
  intent_id <- reactiveVal(NULL)

  # If Customer is enabling billing, we create Setup Intent to
  # attach a (default) payment method to the customer
  # If Customer is making a one time payment, we create a
  # Payment Intent & include checkbox to save Payment Method
  observeEvent(open_modal_trigger(), {

    billing <- session$userData$stripe()

    tryCatch({

      ### CREATE PAYMENT INTENT ###
      setup_res <- httr::POST(
        "https://api.stripe.com/v1/payment_intents",
        body = list(
          "customer" = billing$stripe_customer_id,
          `payment_method_types[0]` = "card",
          "amount" = payment_amount * 100,
          "currency" = currency
        ),
        encode = "form",
        httr::authenticate(
          user = getOption("pp")$keys$secret,
          password = ""
        )
      )

      # TODO: check status and stop for error message and handle it and display error to user

      setup_data <- jsonlite::fromJSON(
        httr::content(setup_res, "text", encoding = "UTF-8")
      )

      intent_id(setup_data$id)

      shiny::showModal(
        shiny::modalDialog(
          shiny::textInput(
            ns("cardholder_name"),
            "Name on Card",
            width = "100%",
            placeholder = 'John K Smith'
          ),
          # shiny::checkboxInput(
          #   ns("attach_payment_method"),
          #   "Save Card for Future Payments",
          #   value = FALSE
          # ),
          tags$br(),
          tags$form(
            action = "/charge",
            method = "post",
            tags$div(
              class = "form-row",
              tags$label(
                `for` = ns("card_element"),
                "Credit Card"
              ),
              tags$div(
                id = ns("card_element")
              ),
              tags$div(
                id = ns("card_errors"),
                role = "alert"
              )
            )
          ),
          tags$hr(),
          tags$p(
            style = "text-align: center;",
            disclaimer_text
          ),
          title = "Billing Information",
          footer = tags$span(
            shiny::modalButton("Cancel"),
            shinyFeedback::loadingButton(
              ns('card_button'),
              'Submit',
              loadingLabel = 'Confirming...'
            )
          ),
          size = 's'
        )
      )

      session$sendCustomMessage(
        ns("create_payment_intent"),
        message = list(
          stripe_key = getOption("pp")$keys$pub,
          card_button_id = ns('card_button'),
          client_secret = setup_data$client_secret
        )
      )

    }, error = function(err) {

      msg <- "Error in Setup Intent"
      print(msg)
      print(err)
      shinyFeedback::showToast("error", msg)
    })

  }, ignoreInit = TRUE)



  # FAILED Payment Method (Payment Intent)
  observeEvent(input$payment_intent_result, {
    hold <- input$payment_intent_result
    billing <- session$userData$stripe()


    if (is.null(hold$error)) {
      shiny::removeModal()
      # if (isTRUE(input$attach_payment_method)) {
      #   tryCatch({
      #
      #     set_default_payment_method(
      #       customer_id = billing$stripe_customer_id,
      #       payment_method_id = hold_payment_method_id
      #     )
      #
      #   }, error = function(err) {
      #     print(err)
      #     shinyFeedback::showToast(
      #       "error",
      #       "Error saving Credit Card for future usage"
      #     )
      #   })
      # }

      intent_id(NULL)
    }
  })

  return(list(
    payment_intent_result = input$payment_intent_result
  ))
}
