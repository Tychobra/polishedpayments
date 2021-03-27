
#' @noRd
#'
#' @importFrom htmltools tags tagList
#' @importFrom shiny actionButton
#' @importFrom shinyjs hidden
#' @importFrom shinydashboard box
plan_column_module_ui <- function(id, width) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shinydashboard::box(
      width = width,
      title = shiny::textOutput(ns("plan_name")),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$h1(style="color: #000", shiny::textOutput(ns("price_out"))),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$div(
        id = ns("sign_up_div"),
        shiny::actionButton(
          ns("sign_up"),
          "Sign Up Now",
          class = "btn-primary btn-lg",
          style = "color: #FFF; width: 100%;",
        )
      ),
      shinyjs::hidden(tags$div(
        id = ns("change_plan_div"),
        shiny::actionButton(
          ns("change_plan"),
          "Change Plan",
          class = "btn-default btn-lg",
          style = "width: 100%;",
        )
      )),
      shinyjs::hidden(
        shiny::actionButton(
          ns("your_plan"),
          label = "Your Plan",
          class = "btn btn-primary btn-lg",
          style = "
            width: 100%;
            color: #FFF;
          "
        )
      )
    ),
    credit_card_module_ui(ns("change_plan_modal"))
  )
}

plan_column_module <- function(input, output, session,
  plan_id,
  sub_info,
  disclaimer_text = "I am a disclaimer",
  hide_waiter = FALSE
) {
  ns <- session$ns



  plan_data <- reactive({

    out <- NULL
    tryCatch({

      res <- httr::GET(
        paste0("https://api.stripe.com/v1/plans/", plan_id),
        httr::authenticate(
          user = getOption("pp")$keys$secret,
          password = ""
        )
      )

      res_content <- jsonlite::fromJSON(
        httr::content(res, "text", encoding = "UTF-8")
      )
      if (!identical(httr::status_code(res), 200L)) {
        stop(paste0("unable to find Stripe plan ", plan_id), call. = FALSE)
      }

      out <- res_content

    }, error = function(err) {
      print(err)
      shinyFeedback::showToast("error", err$message)
    })

    if (isTRUE(hide_waiter)) {
      waiter::waiter_hide()
    }

    out
  })

  output$plan_name <- renderText({
    plan_data()$nickname
  })

  output$price_out <- renderText({
    hold_plan <- plan_data()

    paste0(
      "$",
      format(as.numeric(hold_plan$amount) / 100, big.mark = ","),
      "/",
      hold_plan$interval
    )
  })



  # if plan is selected, show the selected plan banner and hide the button to sign up
  # for the plan
  observeEvent(sub_info(), {

    if (is.null(sub_info())) {
      shinyjs::showElement("sign_up_div")
      shinyjs::hideElement("your_plan")
      shinyjs::hideElement("change_plan_div")
    } else if (sub_info()$plan_id == plan_id) {
      shinyjs::hideElement("sign_up_div")
      shinyjs::showElement("your_plan")
      shinyjs::hideElement("change_plan_div")
    } else {
      shinyjs::hideElement("sign_up_div")
      shinyjs::hideElement("your_plan")
      shinyjs::showElement("change_plan_div")
    }
  }, ignoreNULL = FALSE)

  open_credit_card <- reactiveVal(0)
  open_confirm <- reactiveVal(0)

  observeEvent(input$sign_up, {

    plan_to_sign_up <- input$sign_up

    if (is.null(sub_info()) || is.na(sub_info()$default_payment_method)) {

      open_credit_card(open_credit_card() + 1)

    } else {

      # this should not happen
      showToast("error", "You already have a plan")

    }

  }, ignoreInit = TRUE)

  observeEvent(input$change_plan, {

    if (!is.null(sub_info()) && is.na(sub_info()$default_payment_method)) {
      open_credit_card(open_credit_card() + 1)
    } else {
      open_confirm(open_confirm() + 1)
    }

  }, ignoreInit = TRUE)

  callModule(
    credit_card_module,
    "change_plan_modal",
    open_modal_trigger = open_credit_card,
    plan_to_enable = plan_id,
    disclaimer_text = disclaimer_text,
    sub_info = sub_info
  )



  observeEvent(open_confirm(), {
    req(open_confirm() > 0)

    showModal(
      modalDialog(
        disclaimer_text,
        title = "Plan Change Confirmation",
        footer = tags$span(
          tags$button(
            type = "button", class = "btn btn-default pull-left",
            `data-dismiss` = "modal",
            "Cancel"
          ),
          shinyFeedback::loadingButton(
            ns('new_plan'),
            'Submit',
            loadingLabel = 'Confirming...'
          )
        ),
        size = 's'
      )
    )

  })

  observeEvent(input$new_plan, {

    billing <- session$userData$billing()
    hold_sub_info <- sub_info()
    shiny::removeModal()


    # update the pricing plan for an existing subscription
    tryCatch({
      res <- httr::POST(
        paste0("https://api.stripe.com/v1/subscriptions/", billing$stripe_subscription_id),
        body = list(
          cancel_at_period_end="false",
          proration_behavior="create_prorations",
          `items[0][id]`= hold_sub_info$item_id,
          `items[0][price]`= plan_id
        ),
        encode = "form",
        httr::authenticate(
          user = getOption("pp")$keys$secret,
          password = ""
        )
      )

      if (!identical(httr::status_code(res), 200L)) {
        res_error <- jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        )
        print(res_error)
        stop("Error changing pricing plan", call. = FALSE)
      }

      session$userData$sub_info_trigger(session$userData$sub_info_trigger() + 1)

      shinyFeedback::showToast("success", "Pricing Successfully Changed")

    }, error = function(err) {

      print(err)
      shinyFeedback::showToast("error", err$message)

    })

  }, ignoreInit = TRUE)

  #observeEvent(input$your_plan, {
  #  # to to the Shiny app
  #  polished::remove_query_string()
  #  session$reload()
  #})


  invisible()
}

