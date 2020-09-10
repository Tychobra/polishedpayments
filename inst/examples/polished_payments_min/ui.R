
ui <- fluidPage(
  waiter::use_waiter(),
  waiter::waiter_show_on_load(html = waiter::spin_fading_circles()),
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
      class = "text-center",
      h2("Authenticated User"),
      verbatimTextOutput("polished_user")
    ),
    column(
      width = 6,
      class = "text-center",
      h2("Stripe Subscription"),
      verbatimTextOutput("polished_subscription")
    )
  )
)

secure_ui(
  ui,
  account_module_ui = polishedpayments::app_module_ui("account")
)
