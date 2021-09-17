

server <- function(input, output, session) {

  observeEvent(input$sign_out, {

    polished::sign_out_from_shiny()
    session$reload()

  })

  observeEvent(input$go_to_payments, {
    polishedpayments::go_to_account()
  })


  observeEvent(session$userData$stripe(), {

    polished_user_prep <- reactive({
      hold_user <- session$userData$user()

      data.frame(
        name = names(hold_user),
        value = unlist(hold_user, use.names = FALSE)
      )
    })

    output$polished_user <- DT::renderDT({
      out <- polished_user_prep()

      DT::datatable(
        out,
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          ordering = FALSE
        )
      )
    })



    output$polished_subscription <- renderPrint({
      session$userData$stripe()
    })


    callModule(
      free_trial_banner_module,
      "trial_banner"
    )

  }, once = TRUE)

  payment_return <- shiny::callModule(
    credit_card_payment_module,
    "payment",
    trigger = reactive({input$submit_card_payment}),
    amount = 1000
  )

  observeEvent(payment_return$payment_result(), {
    result <- payment_return$payment_result()

    shinyFeedback::resetLoadingButton("submit_card_payment")
    if (is.null(result$error)) {
      showToast("success", "payment made")
    } else {
      showToast("error", result$error$message)
    }

    print(list(
      payment_result = payment_return$payment_result()
    ))
  })




  subscription_return <- shiny::callModule(
    credit_card_subscription_module,
    "subscription",
    trigger = reactive({input$subscription_card_payment}),
    price_id = "price_1HPGITCJv951GRc3Zuad1uoX"
  )

  observe({
    print(list(
      subscription_result = subscription_return$subscription_result()
    ))
  })

}

payments_server(server) %>%
  secure_server(account_module = polishedpayments::app_module)
