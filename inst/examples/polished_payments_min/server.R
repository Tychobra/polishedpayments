

server <- function(input, output, session) {

  observeEvent(input$sign_out, {

    polished::sign_out_from_shiny()
    session$reload()

  })

  observeEvent(input$go_to_payments, {
    shiny::updateQueryString(
      queryString = "?page=payments",
      session = session,
      mode = "replace"
    )
    session$reload()
  })


  output$polished_user <- renderPrint({
    session$userData$user()
  })

}

secure_server(server)
