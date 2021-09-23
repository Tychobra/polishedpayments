


set_payment_method_modal <- function(input, output, session,
  open_modal_trigger,
  title = "Enable Billing"
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
        size = "s",
        shiny::textInput(
          ns("cc_name"),
          "Cardholder Name",
          width = "100%"
        ),
        credit_card_module_ui(ns("cc_input")),
        br(),
        h5(
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

      }, error = function(err) {

        print(err)
        msg <- "unable to enable billing"
        print(msg)
        showToast("error", msg)

        invisible(NULL)
      })
    } else {

      msg <- "error getting enable billing setup intent"
      print(msg)
      print(setup_intent_res)
      showToast("error", setup_intent_res$error$message)

    }

  })

  return(list())
}