
ui <- fluidPage(
  useShinyFeedback(),
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
      6,
      tabsetPanel(
        tabPanel(
          "Stripe Subscription",
          br(),
          verbatimTextOutput("polished_subscription")
        ),
        tabPanel(
          "Polished User",
          br(),
          verbatimTextOutput("polished_user")
        )
      )
    ),
    column(
      6,
      tabsetPanel(
        tabPanel(
          "Single Payment",
          br(),
          div(
            style="max-width: 400px; margin: 0 auto;",
            wellPanel(
              h2(
                class = "text-center",
                style = "line-height: 1.75;",
                "Make one time $10 payment"
              ),
              br(),
              br(),
              credit_card_module_ui("payment"),
              shinyFeedback::loadingButton(
                "submit_card_payment",
                "Pay $10",
                loadingLabel = 'Confirming Payment...',
                class = "btn-primary btn-lg",
                style = "width: 100%"
              )
            )
          )
        ),
        tabPanel(
          "Subscription",
          br(),
          div(
            style="max-width: 400px; margin: 0 auto;",
            wellPanel(
              h2(
                class = "text-center",
                style = "line-height: 1.75;",
                "Subscribe to $10/month Subscription"
              ),
              br(),
              br(),
              credit_card_module_ui("subscription"),
              shinyFeedback::loadingButton(
                "subscription_card_payment",
                "Subscribe",
                loadingLabel = 'Confirming Payment...',
                class = "btn-primary btn-lg",
                style = "width: 100%"
              )
            )
          )
        )
      )
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
