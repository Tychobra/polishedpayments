credit_card_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$script(src = "polishedpayments/js/credit_card_module.js"),
    tags$script(paste0("credit_card_module('", ns(''), "')"))
  )
}


credit_card_module <- function(
  input, output, session,
  open_modal_trigger = reactive(0),
  disclaimer_text = "Disclaimer",
  plan_to_enable = NULL,
  sub_info = NULL
) {
  ns <- session$ns

  setup_intent_id <- reactiveVal(NULL)

  # Customer is enabling billing, so we create Setup Intent to
  # attach a (default) payment method to the customer
  observeEvent(open_modal_trigger(), {

    billing <- session$userData$billing()

    tryCatch({
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

      httr::stop_for_status(setup_res)

      setup_data <- jsonlite::fromJSON(
        httr::content(setup_res, "text", encoding = "UTF-8")
      )

      setup_intent_id(setup_data$id)

      shiny::showModal(
        shiny::modalDialog(
          shiny::textInput(
            ns("cardholder_name"),
            "Name on Card",
            width = "100%",
            placeholder = 'John K Smith'
          ),
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

      ### COLLECT CARD DETAILS ###
      session$sendCustomMessage(
        ns("create_setup_intent"),
        message = list(
          stripe_key = getOption("pp")$keys$pub,
          card_button_id = ns('card_button'),
          client_secret = setup_data$client_secret
        )
      )

    }, error = function(err) {
      print(err)
      shinyFeedback::showToast("error", "Error in Setup Intent")
    })

  }, ignoreInit = TRUE)

  # CANCEL Update Payment
  observeEvent(input$close_billing_modal, {
    shiny::removeModal()
  })

  # FAILED Payment Method
  observeEvent(input$setup_intent_error, {
    hold_error <- input$setup_intent_error
    print(hold_error)
    shinyFeedback::showToast(
      type = 'error',
      message = hold_error$message
    )
  })

  # SUCCESS Payment Method
  observeEvent(input$setup_intent_success, {
    billing <- session$userData$billing()
    setup_intent_id <- setup_intent_id()
    hold_sub_info <- sub_info()

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
          paste0("https://api.stripe.com/v1/subscriptions/", billing$stripe_subscription_id),
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
        if (is.na(billing$free_trial_days_remaining_at_cancel)) {
          post_body$trial_period_days <- getOption("pp")$trial_period_days
        } else {
          post_body$trial_period_days <- floor(as.numeric(billing$free_trial_days_remaining_at_cancel))
        }

        # Create the subscription and attach Customer & payment method to newly created subscription
        res <- httr::POST(
          paste0("https://api.stripe.com/v1/subscriptions"),
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
              subscription_uid = billing$uid,
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

      session$userData$billing_trigger(session$userData$billing_trigger() + 1)
      session$userData$sub_info_trigger(session$userData$sub_info_trigger() + 1)


      shinyFeedback::showToast(
        type = 'success',
        message = 'Your payment method and subscription have been updated'
      )
    }, error = function(err) {
      print(err)
      shinyFeedback::showToast("error", "Payment method authenticated, but there was an error saving your Payment Method")
    })

    setup_intent_id(NULL)
    shiny::removeModal()
  })

}
