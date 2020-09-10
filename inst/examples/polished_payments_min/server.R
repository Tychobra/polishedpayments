

server <- function(input, output, session) {

  # check user's subscription status and payment method
  check_user_subscription()

  observeEvent(input$sign_out, {

    polished::sign_out_from_shiny()
    session$reload()

  })

  observeEvent(input$go_to_payments, {
    shiny::updateQueryString(
      queryString = "?page=account",
      session = session,
      mode = "replace"
    )
    session$reload()
  })


  observeEvent(session$userData$subscription(), {

    output$polished_user <- renderPrint({
      session$userData$user()
    })

    output$polished_subscription <- renderPrint({
      session$userData$subscription()
    })

  }, once = TRUE)


}

secure_server(
  server,
  account_module = polishedpayments::app_module
)
