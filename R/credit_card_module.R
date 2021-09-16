#' UI element for Stripe Credit Card input
#'
#' @param id The element's namespaced ID
#'
#' @export
credit_card_module_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    tags$script(src = "polishedpayments/js/credit_card_module.js"),
    tags$script(paste0("credit_card_module('", ns(''), "')"))
  )
}

#' Server logic for Stripe Credit Card input
#'
#' @param input
#' @param output
#' @param session
#' @param open_modal_trigger The trigger to open the Credit Card modal
#' @param disclaimer_text the Disclaimer text
#' @param plan_to_enable the subscription plan to enable (requires `subscription = TRUE`)
#' @param sub_info the Customer's subscription info
#' @param subscription whether the Credit Card is for a subscription payment (default) or one time payment
#' @param payment_amount the amount of the one time payment (requires `subscription = FALSE`)
#' @param currency the currency of the one time payment (requires `subscription = FALSE`)
#'
#' @export
credit_card_module <- function(
  input, output, session,
  open_modal_trigger = reactive(0),
  disclaimer_text = "Disclaimer",
  plan_to_enable = NULL,
  subscription = TRUE,
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
      if (isTRUE(subscription)) {
        ### CREATE SETUP INTENT ###
        setup_res <- httr::POST(
          "https://api.stripe.com/v1/setup_intents",
          body = list(
            "customer" = billing$stripe_customer_id,
            `payment_method_types[0]` = "card"
          ),
          encode = "form",
          httr::authenticate(
            user = getOption("pp")$keys$secret,
            password = ""
          )
        )
      } else if (isFALSE(subscription)) {
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
      }

      httr::stop_for_status(setup_res)

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
            tags$div(
              class = 'pull-left',
              shiny::actionButton(
                ns('close_billing_modal'),
                'Cancel'
              )
            ),
            shinyFeedback::loadingButton(
              ns('card_button'),
              'Submit',
              loadingLabel = 'Confirming...'
            )
          ),
          size = 's'
        )
      )

      if (isTRUE(subscription)) {
        # shinyjs::hide("attach_payment_method")

        ### COLLECT CARD DETAILS ###
        session$sendCustomMessage(
          ns("create_setup_intent"),
          message = list(
            stripe_key = getOption("pp")$keys$pub,
            card_button_id = ns('card_button'),
            client_secret = setup_data$client_secret
          )
        )
      } else if (isFALSE(subscription)) {
        # shinyjs::show("attach_payment_method")

        session$sendCustomMessage(
          ns("create_payment_intent"),
          message = list(
            stripe_key = getOption("pp")$keys$pub,
            card_button_id = ns('card_button'),
            client_secret = setup_data$client_secret#,
            # attach_payment_method = input$attach_payment_method
          )
        )
      }

    }, error = function(err) {
      print(err)
      shinyFeedback::showToast("error", "Error in Setup Intent")
    })

  }, ignoreInit = TRUE)

  # CANCEL Update Payment
  observeEvent(input$close_billing_modal, {
    shiny::removeModal()
  })

  # FAILED Payment Method (Setup Intent)
  observeEvent(input$setup_intent_error, {
    hold_error <- input$setup_intent_error
    print(hold_error)
    shinyFeedback::showToast(
      type = 'error',
      message = hold_error$message
    )
  })

  # SUCCESS Payment Method (Setup Intent)
  observeEvent(input$setup_intent_success, {
    billing <- session$userData$stripe()
    setup_intent_id <- intent_id()

    tryCatch({

      # GET payment method ID for this Setup Intent
      si_payment_method <- httr::GET(
        paste0("https://api.stripe.com/v1/setup_intents/", setup_intent_id),
        encode = "form",
        httr::authenticate(
          user = getOption("pp")$keys$secret,
          password = ""
        )
      )

      #httr::stop_for_status(si_payment_method)

      si_payment_method_out <- jsonlite::fromJSON(
        httr::content(si_payment_method, "text", encoding = "UTF-8")
      )


      default_payment_method <- si_payment_method_out$payment_method

      if (is.null(plan_to_enable)) {
        # user is changing their default payment method.  No need to mess with subscription
        # UPDATE the default payment method
        res <- httr::POST(
          paste0("https://api.stripe.com/v1/subscriptions/", billing$subscription$stripe_subscription_id),
          body = list(
            default_payment_method = default_payment_method,
            trial_from_plan = "true"
          ),
          encode = "form",
          httr::authenticate(
            user = getOption("pp")$keys$secret,
            password = ""
          )
        )

        # handle possible errors
        if (!identical(httr::status_code(res), 200L)) {
          res_content <- jsonlite::fromJSON(
            httr::content(res, "text", encoding = "UTF-8")
          )
          # print full Stripe error returned from API
          print(res_content)
          stop("unable to update credit card", call. = FALSE)
        }

      } else {

        post_body <- list(
          "customer" = billing$stripe_customer_id,
          `items[0][plan]` = plan_to_enable,
          "default_payment_method" = default_payment_method
        )

        # if user has already created a free trial, and then canceled their free trial part way through,
        # we keep track of their free trial days used and send them with the create subscription request
        # so that the user does not get to completely restart their free trial.
        if (is.na(billing$subscription$trial_days_remaining)) {
          post_body$trial_period_days <- getOption("pp")$trial_period_days
        } else {
          post_body$trial_period_days <- floor(as.numeric(billing$subscription$trial_days_remaining))
        }

        # Create the subscription and attach Customer & payment method to newly created subscription
        res <- httr::POST(
          "https://api.stripe.com/v1/subscriptions",
          body = post_body,
          encode = "form",
          httr::authenticate(
            user = getOption("pp")$keys$secret,
            password = ""
          )
        )

        res_content <- jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        )

        if (!identical(httr::status_code(res), 200L)) {

          print(res_content)
          stop("unable to create subscription", call. = FALSE)

        } else {

          # update the Stripe subscription id saved to the database
          new_subscription_id <- res_content$id

          res <- httr::PUT(
            url = paste0(getOption("polished")$api_url, "/subscriptions"),
            encode = "json",
            body = list(
              subscription_uid = billing$subscription$uid,
              stripe_subscription_id = new_subscription_id
            ),
            httr::authenticate(
              user = getOption("polished")$api_key,
              password = ""
            )
          )

          if (!identical(httr::status_code(res), 200L)) {

            res_content <- jsonlite::fromJSON(
              httr::content(res, "text", encoding = "UTF-8")
            )

            stop(res_content, call. = FALSE)
          }

        }

      }

      session$userData$stripe_trigger(session$userData$stripe_trigger() + 1)

      shinyFeedback::showToast(
        type = 'success',
        message = 'Your payment method and subscription have been updated'
      )
    }, error = function(err) {
      print(err)
      shinyFeedback::showToast("error", "Payment method authenticated, but there was an error saving your Payment Method")
    })

    intent_id(NULL)
    shiny::removeModal()
  })

  # FAILED Payment Method (Payment Intent)
  observeEvent(input$payment_intent_error, {
    hold_error <- input$payment_intent_error
    print(hold_error)
    shinyFeedback::showToast(
      type = 'error',
      message = hold_error$message
    )
  })

  observeEvent(input$payment_intent_success, {
    billing <- session$userData$stripe()
    # hold_payment_method_id <- input$payment_method_id

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

    shinyFeedback::showToast(
      type = 'success',
      message = 'Payment completed successfully'
    )

    intent_id(NULL)
    shiny::removeModal()
  })

}
