

server <- function(input, output, session) {



  observeEvent(input$sign_out, {

    print(session$user)
    polished::sign_out_from_shiny()
    session$reload()

  })

  observeEvent(input$go_to_payments, {
    polishedpayments::go_to_payments()
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
    create_payment_module,
    "pay_10",
    amount = 1000,
    send_receipt_email = FALSE,
    description = "a $10 one time payment"
  )

  observeEvent(payment_return$payment_response(), {

    print(list(
      payment_response = payment_return$payment_response()
    ))
  })

}

payments_server(server) %>%
  secure_server()
