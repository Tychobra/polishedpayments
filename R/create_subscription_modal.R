

#' Credit Card Modal to create a subscription
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#' @param open_modal_trigger the reactive trigger to open the modal
#' @param price_id the Stripe price id
#' @param ui custom UI added to bottom of the modal
#' @param title the title to pass to \code{shiny::modalDialog}
#' @param size the size to pas to \code{shiny::modalDialog}
#' @param easyClose the easyClose to pass to \code{shiny::modalDialog}
#' @param fade the fade to pass to \code{shiny::modalDialog}
#'
#' @importFrom htmltools tags tagList
#' @importFrom httr GET authenticate content
#' @importFrom jsonlite fromJSON
#' @importFrom shiny showModal modalDialog modalButton actionButton textInput callModule observeEvent reactive removeModal
#' @importFrom shinyFeedback showToast
#'
#'
create_subscription_modal <- function(input, output, session,
  open_modal_trigger,
  price_id,
  ui = tags$p(
    style = "text-align: center",
    "You are signing up for a subscription"
  ),
  # modalDialog inputs
  title = NULL,
  size = "s",
  easyClose = FALSE,
  fade = TRUE
) {
  ns <- session$ns

  observeEvent(open_modal_trigger(), {
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
        credit_card_module_ui(ns("cc_input")),
        tags$br(),
        ui
      )
    )
  }, ignoreInit = TRUE)



  credit_card_module_return <- shiny::callModule(
    credit_card_module,
    ns("cc_input"),
    trigger = reactive({input$submit}),
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

        # if user has already created a free trial, and then canceled their free trial part way through,
        # we keep track of their free trial days used and send them with the create subscription request
        # so that the user does not get to completely restart their free trial.
        if (is.na(billing$trial_days_remaining)) {
          trial_period_days <- getOption("pp")$trial_period_days
        } else {
          trial_period_days <- floor(as.numeric(billing$trial_days_remaining))
        }

        new_subscription_id <- create_stripe_subscription(
          customer_id = billing$stripe_customer_id,
          plan_to_enable = price_id,
          days_remaining = trial_period_days,
          default_payment_method = default_payment_method
        )




        # update the Stripe subscription id saved to the database
        update_customer_res <- update_customer(
          customer_uid = billing$polished_customer_uid,
          stripe_subscription_id = new_subscription_id,
          default_payment_method = default_payment_method
        )

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


  invisible(NULL)
}
