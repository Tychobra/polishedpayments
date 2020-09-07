pricing_module_ui <- function(id) {
  ns <- NS(id)


  shiny::fluidRow(
    style = "margin-top: 100px",
    class = "text-center",
    shiny::column(3),
    shinydashboard::box(
      width = 3,
      title = "Monthly Plan",
      tags$br(),
      tags$br(),
      tags$br(),
      tags$h1("$100/month"),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$div(
        id = ns("sign_up_monthly_div"),
        shiny::actionButton(
          ns("sign_up_monthly"),
          "Sign Up Now",
          class = "btn-primary btn-lg",
          style = "color: #FFF; width: 100%; margin-top: 54px;"
        )
      ),
      shinyjs::hidden(actionButton(
        ns("your_plan_monthly"),
        "Your Plan",
        class = "btn-primary btn-lg",
        style = "color: #FFF; width: 100%; height: 100px;"
      ))
    ),
    shinydashboard::box(
      width = 3,
      title = "Annual Plan",
      br(),
      br(),
      br(),
      h1("$1,000/year"),
      br(),
      br(),
      br(),
      br(),
      div(
        id = ns("sign_up_yearly_div"),
        actionButton(
          ns("sign_up_yearly"),
          "Sign Up Now",
          class = "btn-primary btn-lg",
          style = "color: #FFF; width: 100%; margin-top: 54px;"
        )
      ),
      shinyjs::hidden(actionButton(
        ns("your_plan_yearly"),
        "Your Plan",
        class = "btn-primary btn-lg",
        style = "color: #FFF; width: 100%; height: 100px;"
      ))
    ),
    column(
      12,
      br(),
      br(),
      "All prices are in USD and must be paid with a valid credit card. Taxes may apply."
    ),
    credit_card_module_ui(ns("change_plan_monthly")),
    credit_card_module_ui(ns("change_plan_yearly"))
  )
}





pricing_module <- function(input, output, session, plans, sub_info) {
  ns <- session$ns

  sel_plan <- reactiveVal(NULL)

  # highlight the user's subscription if the user is signed up for a subscription



  observeEvent(sub_info(), {

    hold <- sub_info()

    if (is.null(hold)) {
      hideElement("your_plan_monthly")
      hideElement("your_plan_yearly")

      showElement("sign_up_yearly_div")
      showElement("sign_up_monthly_div")

    } else if (hold$plan_id == "price_1HHABWEEdtiBEUJqW35Bqkbq") {

      hideElement("sign_up_yearly_div")
      showElement("your_plan_yearly")

      hideElement("your_plan_monthly")
      showElement("sign_up_monthly_div")


    } else if (hold$plan_id == "price_1HH6mYEEdtiBEUJq8jDffNmR") {
      hideElement("sign_up_monthly_div")
      showElement("your_plan_monthly")

      hideElement("your_plan_yearly")
      showElement("sign_up_yearly_div")
    }

  }, ignoreNULL = FALSE)

  open_credit_card_monthly <- reactiveVal(0)
  open_confirm_monthly <- reactiveVal(0)

  observeEvent(input$sign_up_monthly, {

    if (is.null(sub_info()) || is.na(sub_info()$default_payment_method)) {

      open_credit_card_monthly(open_credit_card_monthly() + 1)

    } else {

      open_confirm_monthly(open_confirm_monthly() + 1)

    }

  }, ignoreInit = TRUE)


  disclaimer_text_montly = p(
    class = "text-center",
    "The Monthly Plan costs $100.00/month & grants you full access to Alpha Architect's ",
    tags$b("Portfolio Architect"), " application tool.",
    tags$br(),
    "By clicking Submitting, you agree to authorize ",
    tags$b("Alpha Architect"),
    " to collect payments in accordance with the terms of this plan."
  )

  callModule(
    credit_card_module,
    "change_plan_monthly",
    open_modal_trigger = open_credit_card_monthly,
    plan_to_enable = app_config$stripe$prices[[1]],
    disclaimer_text = disclaimer_text_montly,
    sub_info = sub_info
  )
  new_plan <- reactiveVal(NULL)
  observeEvent(open_confirm_monthly(), {
    req(open_confirm_monthly() > 0)

    new_plan("price_1HH6mYEEdtiBEUJq8jDffNmR")

    showModal(
      modalDialog(
        disclaimer_text_montly,
        title = "Plan Change Confirmation",
        footer = tags$span(
          modalButton(
            label = "Cancel"
          ),
          shinyFeedback::loadingButton(
            ns('new_plan_monthly'),
            'Submit',
            loadingLabel = 'Confirming...'
          )
        ),
        size = 's'
      )
    )

  })

  open_credit_card_yearly <- reactiveVal(0)
  open_confirm_yearly <- reactiveVal(0)

  observeEvent(input$sign_up_yearly, {
    hold_sub_info <- sub_info()


    if (is.null(hold_sub_info) || is.na(hold_sub_info$default_payment_method)) {

      open_credit_card_yearly(open_credit_card_yearly() + 1)

    } else {

      open_confirm_yearly(open_confirm_yearly() + 1)

    }

  }, ignoreInit = TRUE)

  disclaimer_text_yearly <- p(
    class = "text-center",
    "The Annual Plan costs $1,000.00/year & grants you full access to Alpha Architect's ",
    tags$b("Portfolio Architect"), " application tool.",
    tags$br(),
    "By clicking Submit, you agree to authorize ",
    tags$b("Alpha Architect"),
    " to collect payments in accordance with the terms of this plan."
  )

  callModule(
    credit_card_module,
    "change_plan_yearly",
    open_modal_trigger = open_credit_card_yearly,
    plan_to_enable = app_config$stripe$prices[[2]],
    disclaimer_text = disclaimer_text_yearly,
    sub_info = sub_info
  )



  observeEvent(open_confirm_yearly(), {
    req(open_confirm_yearly() > 0)

    new_plan("price_1HHABWEEdtiBEUJqW35Bqkbq")

    showModal(
      modalDialog(
        disclaimer_text_yearly,
        title = "Plan Change Confirmation",
        footer = tags$span(
          modalButton(
            label = "Cancel"
          ),
          shinyFeedback::loadingButton(
            ns('new_plan_yearly'),
            'Submit',
            loadingLabel = 'Confirming...'
          )
        ),
        size = 's'
      )
    )

  })

  observeEvent(list(
    input$new_plan_yearly,
    input$new_plan_monthly
  ), {

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
          `items[0][price]`= new_plan(),
          trial_from_plan="true"
        ),
        encode = "form",
        httr::authenticate(
          user = app_config$stripe$keys$secret,
          password = ""
        )
      )

      httr::stop_for_status(res)

      session$userData$sub_info_trigger(session$userData$sub_info_trigger() + 1)

      shinyFeedback::showToast("success", "Pricing Successfully Changed")

    }, error = function(err) {

      print(err)
      shinyFeedback::showToast("error", "Error Changing Pricing")

    })



  }, ignoreInit = TRUE)

}
