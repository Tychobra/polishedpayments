
ui <- fluidPage(
  fluidRow(
    column(
      12,
      h1("Hello Shiny!")
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
