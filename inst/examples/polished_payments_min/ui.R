
ui <- fluidPage(
  fluidRow(
    column(
      12,
      h1(
        style = "display: inline-block;",
        "Shiny with Stripe Subscription!"
      ),
      actionLink(
        "sign_out",
        "Sign Out",
        icon = icon("sign-out"),
        style = "display: inline-block; margin-top: 25px; margin-left: 20px;",
        class = "pull-right"
      ),
      actionLink(
        "go_to_payments",
        "Payments",
        icon = icon("credit-card"),
        style = "display: inline-block; margin-top: 25px;",
        class = "pull-right"
      ),
      br(),
      br(),
      br()
    )
  ),
  fluidRow(
    column(
      width = 6,
      h2("Polished User"),
      DT::DTOutput("polished_user")
    ),
    column(
      width = 6,
      h2("Stripe Subscription"),
      verbatimTextOutput("polished_subscription")
    ),
    column(
      12,
      h2("Test One-time Payment"),
      shiny::actionButton(
        "single_payment_test",
        "Make Payment"
      )
    )
  ),


  column(
    3,
    credit_card_module_ui("payment_1"),
    br(),
    shinyFeedback::loadingButton(
      "submit_card_payment",
      "Submit",
      loadingLabel = 'Confirming...'
    )
  ),

  free_trial_banner_module_ui("trial_banner")
)

payments_ui(ui) %>%
  secure_ui(
    sign_in_page_ui = sign_in_ui_default(
      color = "#5469d4",
      company_name = "Tychobra",
      logo_top = tags$div(
        style = "width: 300px; max-width: 100%; color: #5469d4;",
        class = "text-center",
        h1("Polished", style = "margin-bottom: 0; margin-top: 20px;"),
        h1("Payments", style = "margin-bottom: 0; margin-top: 10px;"),
        h1("Demo", style = "margin-bottom: 15px; margin-top: 10px;")
      ),
      background_image = "background_image.png"
    ),
    account_module_ui = polishedpayments::app_module_ui("account")
  )
