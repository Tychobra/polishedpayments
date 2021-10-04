

#' Modal to set/update the Stripe payment method
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#' @param open_modal_trigger the reactive trigger to open the modal
#' @param title the title of the modal
#'
#' @importFrom htmltools tagList tags
#' @importFrom httr status_code
#' @importFrom shiny observeEvent showModal modalDialog modalButton textInput callModule reactive removeModal
#' @importFrom shinyFeedback loadingButton resetLoadingButton
#'
set_payment_method_modal <- function(input, output, session,
  open_modal_trigger,
  title = "Enable Billing"
) {
  ns <- session$ns

  observeEvent(open_modal_trigger(), {

    shiny::showModal(
      shiny::modalDialog(
        title = title,
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shinyFeedback::loadingButton(
            ns("submit"),
            "Submit",
            loadingLabel = "Confirming...",
            class = "btn btn-primary",
            style = "color: #FFF; width: 150px;"
          )
        ),
        size = "s",
        shiny::textInput(
          ns("cc_name"),
          "Cardholder Name",
          width = "100%"
        ),
        credit_card_module_ui(ns("cc_input")),
        tags$br(),
        tags$h5(
          style = "text-align: center",
          "This card will be used for future payments"
        )
      )
    )
  })


  cc_module_return <- shiny::callModule(
    credit_card_module,
    ns("cc_input"),
    trigger = reactive({input$submit}),
    billing_detail = reactive(list(
      name = input$cc_name
    ))
  )

  observeEvent(cc_module_return$setup_intent_result(), {
    hold_user <- session$userData$user()
    hold_stripe <- session$userData$stripe()
    setup_intent_res <- cc_module_return$setup_intent_result()


    if (is.null(setup_intent_res$error)) {

      setup_intent <- setup_intent_res$setupIntent
      tryCatch({

        # update customer via polished API to have the default_payment_method and
        update_res <- update_customer(
          customer_uid = hold_stripe$polished_customer_uid,
          default_payment_method = setup_intent$payment_method
        )

        if (!identical(httr::status_code(update_res$response), 200L)) {
          stop("unable to enable payment method", call. = FALSE)
        }

        removeModal()

        session$userData$stripe(
          get_stripe(
            user_uid = hold_user$user_uid,
            user_roles = hold_user$roles,
            is_on_payments = TRUE
          )
        )

        showToast("success", "Payment Method Updated")

      }, error = function(err) {

        showToast("error", "error updating payment method")
        shinyFeedback::resetLoadingButton("submit")

        print(err)
        msg <- "unable to enable billing"
        print(msg)
        showToast("error", msg)

        invisible(NULL)
      })
    } else {

      shinyFeedback::resetLoadingButton("submit")
      msg <- "error getting enable billing setup intent"
      print(msg)
      print(setup_intent_res)
      showToast("error", setup_intent_res$error$message)

    }

  })

  return(list())
}
