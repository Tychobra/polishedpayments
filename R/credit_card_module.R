#' UI element for Stripe Credit Card input
#'
#' @param id The element's namespaced ID
#' @param width valid css unit for width
#'
#' @export
credit_card_module_ui <- function(
  id,
  width = "100%"
) {
  ns <- shiny::NS(id)

  tagList(
    tags$div(
      style = paste0("width: ", width),
      tags$fieldset(
        tags$div(
          id = ns("credit_card")
        )
      )
    ),
    tags$script(src = "polishedpayments/js/credit_card_module.js"),
    tags$script(paste0("credit_card_module('", ns(''), "')"))
  )
}



#' Server logic for Stripe Credit Card input
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#' @param trigger the reactive trigger to submit the payment to Stripe
#' @param amount the amount of the payment
#' @param the currency.  Defaults to "usd" (United States Dollar).
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

    } else {
      intent_id(NULL)
    }
  })

  return(list(
    payment_result = reactive({input$payment_intent_result})
  ))
}

credit_card_subscription_module <- function(input, output, session) {

  intent_id <- reactiveVal(NULL)

  observeEvent(trigger(), {

    tryCatch({
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

      # TODO: handle actual error message
      httr::stop_for_status(setup_res)

      setup_data <- jsonlite::fromJSON(
        httr::content(setup_res, "text", encoding = "UTF-8")
      )

      intent_id(setup_data$intent)

      session$sendCustomMessage(
        ns("create_setup_intent"),
        message = list(
          client_secret = setup_data$client_secret
        )
      )

    }, function(err) {
      msg <- "Error in subscription setup"
      print(msg)
      print(err)
      shinyFeedback::showToast("error", msg)
    })

  })

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
  })
}





credit_card_change_module <- function(input, output, session) {
  # observeEvent(trigger(), {
  #   hold_intent_id <- intent_id()
  #
  #   tryCatch({
  #
  #     # GET payment method ID for this Setup Intent
  #
  #
  #
  #     si_payment_method <- httr::GET(
  #       paste0("https://api.stripe.com/v1/setup_intents/", hold_intent_id),
  #       encode = "form",
  #       httr::authenticate(
  #         user = getOption("pp")$keys$secret,
  #         password = ""
  #       )
  #     )
  #
  #
  #
  #     #httr::stop_for_status(si_payment_method)
  #
  #     si_payment_method_out <- jsonlite::fromJSON(
  #       httr::content(si_payment_method, "text", encoding = "UTF-8")
  #     )
  #
  #
  #     si_default_payment_method <- si_payment_method_out$payment_method
  #
  #
  #     # user is changing their default payment method.  No need to mess with subscription
  #     # UPDATE the default payment method
  #     res <- httr::POST(
  #       paste0("https://api.stripe.com/v1/subscriptions/", billing$stripe_subscription_id),
  #       body = list(
  #         default_payment_method = default_payment_method,
  #         trial_from_plan = "true"
  #       ),
  #       encode = "form",
  #       httr::authenticate(
  #         user = getOption("pp")$keys$secret,
  #         password = ""
  #       )
  #     )
  #
  #     # handle possible errors
  #     if (!identical(httr::status_code(res), 200L)) {
  #       res_content <- jsonlite::fromJSON(
  #         httr::content(res, "text", encoding = "UTF-8")
  #       )
  #       # print full Stripe error returned from API
  #       print(res_content)
  #       stop("unable to update credit card", call. = FALSE)
  #     }
  #
  #     # TODO: do the same as above except for payments rather than subscriptions
  #
  #   }, error = function(err) {
  #
  #     msg <- "unable to update default payment method"
  #     print(msg)
  #     print(err)
  #     showToast("error", msg)
  #
  #   })
  #
  #
  # })
}

