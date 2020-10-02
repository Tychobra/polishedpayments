

server <- function(input, output, session) {

  # check user's subscription status and payment method
  check_user_subscription()

  observeEvent(input$sign_out, {

    polished::sign_out_from_shiny()
    session$reload()

  })

  observeEvent(input$go_to_payments, {
    polishedpayments::go_to_account()
  })


  observeEvent(session$userData$subscription(), {

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

    polished_subscription_prep <- reactive({
      hold_sub <- session$userData$subscription()

      data.frame(
        name = names(hold_sub),
        value = unlist(hold_sub, use.names = FALSE)
      )
    })

    output$polished_subscription <- DT::renderDT({
      out <- polished_subscription_prep()

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

    waiter_hide()

    callModule(
      free_trial_banner_module,
      "trial_banner"
    )

  }, once = TRUE)

}

secure_server(
  server,
  account_module = polishedpayments::app_module
)
