

server <- function(input, output, session) {

  output$polished_user <- renderPrint({
    session$userData$user()
  })

}

secure_server(server)
