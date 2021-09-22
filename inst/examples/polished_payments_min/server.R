

server <- function(input, output, session) {

  observeEvent(input$sign_out, {

    polished::sign_out_from_shiny()
    session$reload()

  })

  observeEvent(input$go_to_payments, {
    polishedpayments::go_to_account()
  })



  output$polished_user <- renderPrint({
    session$userData$user()
  })



  output$polished_subscription <- renderPrint({
    session$userData$stripe()
  })


  callModule(
    free_trial_banner_module,
    "trial_banner"
  )


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

}

payments_server(server) %>%
  secure_server()
