#' UI element for Stripe Credit Card input
#'
#' @param id the Shiny module id
#'
#' @importFrom htmltools tagList tags
#' @importFrom shiny NS
#'
#' @export
credit_card_module_ui <- function(
  id
) {
  ns <- shiny::NS(id)

  wrapper_id <- ns("wrapper")

  tagList(
    tags$style(paste0("

#", wrapper_id, "example-1 * {
  font-family: Roboto, Open Sans, Segoe UI, sans-serif;
  font-size: 16px;
  font-weight: 500;
}

#", wrapper_id, " fieldset {
  margin: 0 0 20px 0;
  padding: 0;
  border-style: none;
  background-color: #FFF;/*#7795f8;*/
  box-shadow: 0 6px 9px rgba(50, 50, 93, 0.06), 0 2px 5px rgba(0, 0, 0, 0.08),
    inset 0 1px 0 #829fff;
  border-radius: 4px;
}

#", wrapper_id, " .stripe-row {
  display: -ms-flexbox;
  display: flex;
  -ms-flex-align: center;
  align-items: center;
  margin-left: 15px;
}

#", wrapper_id, " .stripe-row + .stripe-row {
  border-top: 1px solid #819efc;
}

#", wrapper_id, " label {
  width: 15%;
  min-width: 70px;
  padding: 11px 0;
  color: #c4f0ff;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

#", wrapper_id, " input {
  -webkit-appearance: none;
  -moz-appearance: none;
  appearance: none;
  outline: none;
  border-style: none;
}

#", wrapper_id, " input:-webkit-autofill {
  -webkit-text-fill-color: #fce883;
  transition: background-color 100000000s;
  -webkit-animation: 1ms void-animation-out;
}

#", wrapper_id, " .StripeElement--webkit-autofill {
  background: transparent !important;
}

#", wrapper_id, " .StripeElement {
  width: 100%;
  padding: 11px 15px 11px 0;
}

#", wrapper_id, " input {
  width: 100%;
  padding: 11px 15px 11px 0;
  color: #fff;
  background-color: transparent;
  -webkit-animation: 1ms void-animation-out;
}

#", wrapper_id, " input::-webkit-input-placeholder {
  color: #87bbfd;
}

#", wrapper_id, " input::-moz-placeholder {
  color: #87bbfd;
}

#", wrapper_id, " input:-ms-input-placeholder {
  color: #87bbfd;
}
    ")),
    tags$div(
      id = wrapper_id,
      #style = paste0("width: ", width),
      tags$fieldset(
        tags$div(
          class = "stripe-row",
          tags$div(
            id = ns("credit_card")
          )
        )
      )
    ),
    tags$script(paste0("payments.mount_card_element('", ns('credit_card'), "')"))
  )
}



#' Server logic for Stripe Credit Card input
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#' @param trigger the reactive trigger to submit the payment to Stripe
#' @param amount a positive integer representing how much to charge in the
#' smallest currency unit (e.g., 100 cents to charge $1.00 or 100 to charge
#' ¥100, a zero-decimal currency)
#' @param currency The currency. Defaults to "usd" (United States Dollar).
#'
#' @importFrom httr POST authenticate content
#' @importFrom jsonlite fromJSON
#' @importFrom shiny reactiveVal observeEvent reactive
#' @importFrom shinyFeedback showToast
#'
#' @export
#'
credit_card_payment_module <- function(input, output, session,
  trigger = function() NULL,
  amount,
  currency = "usd"
) {
  ns <- session$ns

  intent_id <- reactiveVal(NULL)

  observeEvent(trigger(), {

    billing <- session$userData$stripe()

    tryCatch({

      ### CREATE PAYMENT INTENT ###
      setup_res <- httr::POST(
        "https://api.stripe.com/v1/payment_intents",
        body = list(
          "customer" = billing$stripe_customer_id,
          `payment_method_types[0]` = "card",
          "amount" = amount,
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

      session$sendCustomMessage(
        ns("create_payment"),
        message = list(
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

  observeEvent(input$payment_intent_result, {
    hold <- input$payment_intent_result
    billing <- session$userData$stripe()

    if (is.null(hold$error)) {

      if (isTRUE(input$attach_payment_method)) {
        tryCatch({

          set_stripe_payment_method(
            customer_id = billing$stripe_customer_id,
            payment_method_id = hold$payment_method_id
          )

        }, error = function(err) {
          print(err)
          shinyFeedback::showToast(
            "error",
            "Error saving Credit Card for future usage"
          )
        })
      }

    }
  })

  return(list(
    payment_result = reactive({input$payment_intent_result})
  ))
}


#' Server logic for Stripe Credit Card input
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#' @param trigger the reactive trigger to submit the payment to Stripe
#' @param billing_details reactive returning a named list of billing details. Valid list element names
#' are:
#' - "name"
#' - "email"
#' - "phone"
#'
#' @importFrom httr POST authenticate content
#' @importFrom jsonlite fromJSON
#' @importFrom shiny observeEvent reactive
#' @importFrom shinyFeedback showToast
#'
#' @export
#'
credit_card_module <- function(input, output, session,
  trigger = function() NULL,
  billing_details = function() list()
) {
  ns <- session$ns


  observeEvent(trigger(), {

    billing <- session$userData$stripe()
    hold_details <- billing_details()


    body_out <- list(
      "customer" = billing$stripe_customer_id,
      `payment_method_types[0]` = "card"
    )


    tryCatch({
      setup_res <- httr::POST(
        "https://api.stripe.com/v1/setup_intents",
        body = body_out,
        encode = "form",
        httr::authenticate(
          user = getOption("pp")$keys$secret,
          password = ""
        )
      )

      # TODO: handle actual error message
      httr::stop_for_status(setup_res)

      setup_data <- jsonlite::fromJSON(
        httr::content(setup_res, "text", encoding = "UTF-8")
      )

      session$sendCustomMessage(
        "confirm_card_setup",
        message = list(
          ns_prefix = ns(""),
          client_secret = setup_data$client_secret,
          billing_details = hold_details
        )
      )

    }, error = function(err) {

      msg <- "Error in subscription setup"
      print(msg)
      print(err)
      shinyFeedback::showToast("error", msg)
    })

  })

  return(list(
    setup_intent_result = reactive({input$setup_intent_result})
  ))
}

