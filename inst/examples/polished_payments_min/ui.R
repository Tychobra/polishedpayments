
ui <- fluidPage(
  fluidRow(
    column(
      12,

      h1(
        style = "display: inline-block;",
        "Hello Shiny!"
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
    )
  ),
  fluidRow(
    column(
      12,
      verbatimTextOutput("polished_user")
    )
  )
)

secure_ui(ui)
