

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

  trigger <- reactive({
    browser()
    input$submit
  })

  credit_card_module_return <- shiny::callModule(
    credit_card_module,
    ns("cc_input"),
    trigger = trigger,
    billing_detail = reactive(list(
      name = input$cc_name
    ))
  )

  observeEvent(credit_card_module_return$setup_intent_result(), {

    browser()

    billing <- session$userData$stripe()
    setup_intent_id <- credit_card_module_return$setup_intent_result()

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
        `items[0][price]` = price_id,
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

        update_customer_res <- update_customer(
          customer_uid = billing$polished_customer_uid,
          stripe_subscription_id = new_subscription_id,
          default_payment_method = default_payment_method
        )

        if (!identical(httr::status_code(update_customer_res$response), 200L)) {

          stop(update_customer_res$content, call. = FALSE)
        }

      }

      # session$userData$stripe_trigger(session$userData$stripe_trigger() + 1)

      shinyFeedback::showToast(
        type = 'success',
        message = 'Your payment method and subscription have been updated'
      )
    }, error = function(err) {

      msg <-  "Payment method authenticated, but there was an error saving your Payment Method"
      print(msg)
      print(err)
      shinyFeedback::showToast("error", msg)
    })

    intent_id(NULL)
  })


  invisible(NULL)
}