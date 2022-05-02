
#' @noRd
#'
#' @importFrom htmltools tags tagList
#' @importFrom shiny actionButton textOutput
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
        id = ns("choose_plan_div"),
        shiny::actionButton(
          ns("choose_plan"),
          "Choose Plan",
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
    )
  )
}

#' @noRd
#'
#' @importFrom htmltools tags HTML
#' @importFrom httr GET POST authenticate content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom shiny reactive renderText observeEvent reactiveVal callModule showModal modalDialog modalButton removeModal
#' @importFrom shinyFeedback showToast loadingButton
#' @importFrom shinyjs hideElement showElement
#' @importFrom waiter waiter_hide
plan_column_module <- function(input, output, session,
  plan_id,
  hide_waiter = FALSE,
  stripe_secret_key = .pp$keys$secret
) {
  ns <- session$ns



  plan_data <- reactive({

    out <- NULL
    tryCatch({

      res <- httr::GET(
        paste0("https://api.stripe.com/v1/plans/", plan_id),
        httr::authenticate(
          user = stripe_secret_key,
          password = ""
        )
      )

      res_content <- jsonlite::fromJSON(
        httr::content(res, "text", encoding = "UTF-8")
      )
      if (!identical(httr::status_code(res), 200L)) {
        print(res_content)
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
  observeEvent(session$userData$stripe(), {
    sub_info <- session$userData$stripe()$subscription

    if (is.na(sub_info[1])) {
      shinyjs::showElement("choose_plan_div")
      shinyjs::hideElement("your_plan")
      shinyjs::hideElement("change_plan_div")
    } else if (sub_info$plan_id == plan_id) {
      shinyjs::hideElement("choose_plan_div")
      shinyjs::showElement("your_plan")
      shinyjs::hideElement("change_plan_div")
    } else {
      shinyjs::hideElement("choose_plan_div")
      shinyjs::hideElement("your_plan")
      shinyjs::showElement("change_plan_div")
    }
  }, ignoreNULL = FALSE)

  open_credit_card <- reactiveVal(0)
  open_confirm <- reactiveVal(0)

  observeEvent(input$choose_plan, {
    hold_stripe <- session$userData$stripe()


    if (is.na(hold_stripe$default_payment_method)) {

      open_credit_card(open_credit_card() + 1)

    } else {

      open_confirm(open_confirm() + 1)

    }

  }, ignoreInit = TRUE)

  observeEvent(input$change_plan, {
    sub_info <- session$userData$stripe()

    if (is.na(sub_info$default_payment_method)) {
      open_credit_card(open_credit_card() + 1)
    } else {
      open_confirm(open_confirm() + 1)
    }

  }, ignoreInit = TRUE)


  callModule(
    create_subscription_modal,
    "change_plan_modal",
    open_modal_trigger = open_credit_card,
    price_id = plan_id,
    title = "Change Plan"
  )




  observeEvent(open_confirm(), {
    req(open_confirm() > 0)
    hold_plan <- plan_data()


    showModal(
      modalDialog(
        tags$div(
          class = "text-center",
          style = "padding: 30px; line-height: 1.7",
          tags$h3(htmltools::HTML(
            paste0("Confirm purchase of the ", tags$b(hold_plan$nickname), " plan.")
          ))
        ),
        title = "Plan Change",
        footer = tags$span(
          shiny::modalButton("Cancel"),
          shinyFeedback::loadingButton(
            ns('new_plan'),
            'Yes, Confirm',
            loadingLabel = 'Confirming...'
          )
        ),
        size = "m"
      )
    )

  })

  observeEvent(input$new_plan, {
    hold_user <- session$userData$user()
    hold_stripe <- session$userData$stripe()
    hold_sub_info <- hold_stripe$subscription
    shiny::removeModal()

    # update the pricing plan for an existing subscription
    tryCatch({

      if (is.na(hold_sub_info[1])) {
        # user does not have a subscription, so create a new subscription
        res <- httr::POST(
          "https://api.stripe.com/v1/subscriptions",
          body = list(
            customer = hold_stripe$stripe_customer_id,
            cancel_at_period_end = "false",
            default_payment_method = hold_stripe$default_payment_method,
            trial_period_days = max(c(floor(hold_stripe$trial_days_remaining), 0), na.rm = TRUE),
            `items[0][price]`= plan_id
          ),
          encode = "form",
          httr::authenticate(
            user = stripe_secret_key,
            password = ""
          )
        )

        res_content <- jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        )

        if (!identical(httr::status_code(res), 200L)) {
          print(res_content)
          stop("Error changing pricing plan", call. = FALSE)
        }

        update_customer(
          customer_uid = hold_stripe$polished_customer_uid,
          stripe_subscription_id = res_content$id
        )


      } else {
        # user has an existing subscription, so switch the user to the new subscription
        res <- httr::POST(
          paste0("https://api.stripe.com/v1/subscriptions/", hold_sub_info$stripe_subscription_id),
          body = list(
            cancel_at_period_end="false",
            proration_behavior="create_prorations",
            `items[0][id]`= hold_sub_info$item_id,
            `items[0][price]`= plan_id
          ),
          encode = "form",
          httr::authenticate(
            user = stripe_secret_key,
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
      }

      session$userData$stripe(
        get_stripe(
          user_uid = hold_user$user_uid
        )
      )

      shinyFeedback::showToast("success", "Subscription Successfully Updated")

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

