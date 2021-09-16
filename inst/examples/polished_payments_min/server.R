

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

  shiny::callModule(
    credit_card_module,
    "one_time_payment",
    open_modal_trigger = reactive({input$single_payment_test}),
    disclaimer_text = tags$p(
      class = "text-center",
      "You will be charged immediately for this one time payment."
    ),
    subscription = FALSE,
    payment_amount = 20.00
  )

}

payments_server(server) %>%
  secure_server(account_module = polishedpayments::app_module)
