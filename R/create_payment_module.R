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
#' @return a list with 1 reactiveVal
#' - payment_response - returns the response to the payment attempt
#'
#' @importFrom shiny showModal modalDialog tagList textInput callModule
#' @importFrom shinyFeedback loadingButton
#'
#' @export
#'
create_payment_module <- function(input, output, session,
  amount,
  currency = "usd",
  send_receipt_email = TRUE,
  description = NULL,
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
      err_msg <- NULL
      tryCatch({

        pm <- get_stripe_payment_method(hold_stripe$default_payment_method)

      }, error = function(err) {

        msg <- "unable to get payment method info"
        print(msg)
        print(err)
        err_msg <<- err$message
        showToast("error", msg)

        invisible(NULL)
      })

      if (!is.null(err_msg)) {
        return()
      }



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


  payment_out <- reactiveVal(NULL)

  observeEvent(credit_card_module_return$setup_intent_result(), {
    hold_user <- session$userData$user()
    hold_stripe <- session$userData$stripe()
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

        si_payment_method_out <- jsonlite::fromJSON(
          httr::content(si_payment_method, "text", encoding = "UTF-8")
        )

        if (!identical(httr::status_code(si_payment_method), 200L)) {
          print(si_payment_method_out)
          stop(si_payment_method_out$error$message, call. = FALSE)
        }

        default_payment_method <- si_payment_method_out$payment_method


        if (isTRUE(send_receipt_email)) {
          receipt_email <- hold_user$email
        } else {
          receipt_email <- NULL
        }

        payment_res <- create_payment(
          customer_id = hold_stripe$stripe_customer_id,
          payment_method_id = default_payment_method,
          amount = amount,
          currency = currency,
          receipt_email = receipt_email,
          description = description
        )


        if (isTRUE(input$save_cc)) {
          update_customer_res <- update_customer(
            customer_uid = hold_stripe$polished_customer_uid,
            default_payment_method = default_payment_method
          )

          session$userData$stripe(get_stripe(
            user_uid = hold_user$user_uid,
            user_roles = hold_user$roles,
          ))
        }

        shinyFeedback::showToast(
          type = "success",
          message = "Payment Processed"
        )

        payment_out(payment_res)

      }, error = function(err) {

        payment_out(list(
          error = err$message
        ))
        msg <-  "unable to process payment"
        print(msg)
        print(err)
        shinyFeedback::showToast("error", msg)
      })

    } else {

      msg <- "error getting setup intent"
      print(msg)
      print(setup_intent_res)
      payment_out(setup_intent_res)
      showToast("error", setup_intent_res$error$message)
    }

    resetLoadingButton("submit_cc")

  })

  observeEvent(input$submit_no_cc, {
    hold_stripe <- session$userData$stripe()
    hold_user <- session$userData$user()


    if (isTRUE(send_receipt_email)) {
      receipt_email <- hold_user$email
    } else {
      receipt_email <- NULL
    }

    tryCatch({

      payment_res <- create_payment(
        customer_id = hold_stripe$stripe_customer_id,
        payment_method_id = hold_stripe$default_payment_method,
        amount = amount,
        currency = currency,
        receipt_email = receipt_email,
        description = description
      )

      showToast("success", "Payment Processed")
      payment_out(payment_res)

    }, error = function(err) {

      msg <- "unable to process payment"
      print(msg)
      print(err)
      payment_out(list(
        error = err$message
      ))
      showToast("error", err$message)

    })

    resetLoadingButton("submit_no_cc")

  })


  return(list(
    payment_response = payment_out
  ))
}