#' Shiny module ui for creating a one time payment
#'
#' @param id the module id
#'
#' @export
#'
create_payment_module_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_out"))
}

#' Shiny module server for creating a one time payment
#'
#' @param open_modal_trigger reactive trigger to open the modal
#' @param amount the amount of the one time payment
#' @param title the title to pass to \code{shiny::modalDialog}
#' @param size the size to pas to \code{shiny::modalDialog}
#' @param easyClose the easyClose to pass to \code{shiny::modalDialog}
#' @param fade the fade to pass to \code{shiny::modalDialog}
#'
#' @importFrom shiny showModal modalDialog tagList textInput callModule
#' @importFrom shinyFeedback loadingButton
#'
#' @export
#'
create_payment_module <- function(input, output, session,
  amount,
  ui = NULL,
  # modalDialog inputs
  title = NULL,
  size = "s",
  easyClose = FALSE,
  fade = TRUE
) {
  ns <- session$ns


  ui_prep <- reactive({
    hold_stripe <- session$userData$stripe()

    browser()

    if (is.na(hold_stripe$default_payment_method)) {

      out <- div(
        shiny::textInput(
          ns("cc_name"),
          "Cardholder Name",
          width = "100%"
        ),
        credit_card_module_ui(ns("cc_input")),
        checkboxInput(
          ns("save_cc"),
          "Save Credit Card for Future Payments"
        ),
        ui,
        shinyFeedback::loadingButton(
          ns("submit_cc"),
          "Pay",
          loadingLabel = "Confirming...",
          class = "btn btn-lg btn-primary",
          style = "color: #FFF; width: 100%"
        )
      )



    } else {
      pm <- NULL
      tryCatch({

        pm <- get_stripe_payment_method(hold_stripe$default_payment_method)


      }, error = function(err) {

      })




      out <- div(
        hr(),
        h4(paste0("Pay with ", tools::toTitleCase(pm$card_brand), " ending in ", pm$card_last4)),
        h5(paste0("expires ", pm$exp_month, "/", pm$exp_year)),
        hr(),
        br(),
        ui,
        shinyFeedback::loadingButton(
          ns("submit_no_cc"),
          "Pay",
          loadingLabel = "Confirming...",
          class = "btn btn-lg btn-primary",
          style = "color: #FFF; width: 100%"
        )
      )

    }

    out
  })

  output$ui_out <- renderUI({
    ui_prep()
  })

  credit_card_module_return <- shiny::callModule(
    credit_card_module,
    ns("cc_input"),
    trigger = reactive({input$submit_cc}),
    billing_detail = reactive(list(
      name = input$cc_name
    ))
  )


  observeEvent(credit_card_module_return$setup_intent_result(), {

    billing <- session$userData$stripe()
    setup_intent_res <- credit_card_module_return$setup_intent_result()
    if (is.null(setup_intent_res$error)) {
      setup_intent <- setup_intent_res$setupIntent

      tryCatch({

        # GET payment method ID for this Setup Intent
        si_payment_method <- httr::GET(
          paste0("https://api.stripe.com/v1/setup_intents/", setup_intent$id),
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
        if (is.na(billing$trial_days_remaining)) {
          post_body$trial_period_days <- getOption("pp")$trial_period_days
        } else {
          post_body$trial_period_days <- floor(as.numeric(billing$trial_days_remaining))
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

        shinyFeedback::showToast(
          type = 'success',
          message = 'Your payment method and subscription have been updated'
        )

        removeModal()

      }, error = function(err) {

        msg <-  "Payment method authenticated, but there was an error saving your Payment Method"
        print(msg)
        print(err)
        shinyFeedback::showToast("error", msg)
      })

    } else {

      msg <- "error getting setup intent"
      print(msg)
      print(setup_intent_res)
      showToast("error", setup_intent_res$error$message)
    }

  })

  observeEvent(input$submit_no_cc, {

    browser()

  })


  invisible(NULL)
}