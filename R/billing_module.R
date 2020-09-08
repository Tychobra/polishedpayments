#' billing_module_ui
#'
#' @param id the module id
#'
#'
billing_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(stringr::str_interp("
      #${ns('invoices_table')} th,
      #${ns('invoices_table')} td {
        text-align: center;
      }
    ")),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Account Information",
        width = 12,
        shiny::column(
          12,
          tags$div(
            style = "width: 150px; display: inline-block;",
            tags$h4(tags$strong("Plan"))
          ),
          tags$div(
            style = "display: inline-block",
            tags$h4(textOutput(ns("plan_name_out")))
          ),
          tags$hr(style = "margin: 0;")
        ),
        shiny::column(
          12,
          tags$div(
            style = "width: 150px; display: inline-block;",
            tags$h4(tags$strong("Amount"))
          ),
          tags$div(
            style = "display: inline-block",
            tags$h4(textOutput(ns("plan_amount_out")))
          ),
          tags$hr(style = "margin: 0;")
        ),
        shiny::column(
          12,
          tags$div(
            style = "width: 150px; display: inline-block;",
            tags$h4(tags$strong("Start Date"))
          ),
          tags$div(
            style = "display: inline-block",
            tags$h4(textOutput(ns("account_created_out")))
          ),
          tags$hr(style = "margin: 0;")
        ),
        shiny::column(
          12,
          id = ns("has_trial"),
          tags$div(
            style = "width: 150px; display: inline-block;",
            h4(tags$strong("Trial End"))
          ),
          tags$div(
            style = "display: inline-block",
            h4(textOutput(ns("trial_end_out")))
          ),
          tags$hr(style = "margin: 0;")
        ),
        shiny::column(
          12,
          tags$br(),
          shinyjs::hidden(shiny::actionButton(
            ns("cancel_subscription"),
            "Cancel Subscription",
            class = "btn-primary pull-right",
            style = "color: #FFF; width: 150px;"
          ))
        )
      )
    ),



    tags$div(
      id = ns("billing_info_box"),
      shiny::fluidRow(
        shinydashboard::box(
          title = "Billing Information",
          width = 12,
          tags$div(
            id = ns("billing_info"),
            shiny::column(
              12,
              tags$div(
                style = "width: 150px; display: inline-block;",
                tags$h4(tags$strong("Name"))
              ),
              tags$div(
                style = "display: inline-block",
                h4(textOutput(ns("name_out")))
              ),
              tags$hr(style = "margin: 0;")
            ),
            shiny::column(
              12,
              tags$div(
                style = "width: 150px; display: inline-block;",
                tags$h4(tags$strong("Zip Code"))
              ),
              tags$div(
                style = "display: inline-block",
                tags$h4(textOutput(ns("postal_code")))
              ),
              tags$hr(style = "margin: 0;")
            ),
            shiny::column(
              12,
              tags$div(
                style = "width: 150px; display: inline-block;",
                tags$h4(tags$strong("Brand"))
              ),
              tags$div(
                style = "display: inline-block",
                tags$h4(textOutput(ns("card_brand_out")))
              ),
              tags$hr(style = "margin: 0;")
            ),
            shiny::column(
              12,
              tags$div(
                style = "width: 150px; display: inline-block;",
                tags$h4(tags$strong("Number"))
              ),
              tags$div(
                style = "display: inline-block",
                tags$h4(textOutput(ns("last_4_out")))
              ),
              tags$hr(style = "margin: 0;")
            ),
            shiny::column(
              12,
              tags$div(
                style = "width: 150px; display: inline-block;",
                tags$h4(tags$strong("Expires"))
              ),
              tags$div(
                style = "display: inline-block",
                tags$h4(textOutput(ns("card_exp_out")))
              ),
              tags$hr(style = "margin: 0;")
            ),
            shiny::column(
              12,
              tags$br(),
              shiny::actionButton(
                ns("update_billing_info"),
                "Update Billing",
                class = "btn-primary pull-right",
                style = "color: #FFF; width: 150px;"
              )
            )
          ),
          shinyjs::hidden(tags$div(
            id = ns("enable_billing_button"),
            class = "text-center",
            h3("Billing is not enabled"),
            br(),
            shiny::actionButton(
              ns("enable_billing"),
              "Enable Billing",
              class = "btn-primary btn-lg",
              style = "color: #FFF"
            ),
            br(),
            br(),
            br(),
            br()
          ))
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Invoices",
        width = 12,
        DT::DTOutput(ns("invoices_table"))
      )
    ),
    br(), br(), br(),
    credit_card_module_ui(
      ns("enable_billing")
    ),
    credit_card_module_ui(
      ns("change_credit_card")
    )
  )
}


#' billing module
#'
#' @param app_url the url of the app
#' @param sub_infor the subscription information
#'
#' @importFrom dplyr %>% select mutate
#'
billing_module <- function(input, output, session, sub_info) {
  ns <- session$ns


  ### CANCEL SUBSCRIPTION ###
  observeEvent(input$cancel_subscription, {
    req(sub_info())
    subscription_name <- sub_info()$nickname

    showModal(
      shiny::modalDialog(
        title = "Cancel Subscription",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            ns("submit_cancel"),
            "Submit",
            class = "btn-danger",
            style = "color: #FFF"
          )
        ),
        size = "m",
        easyClose = TRUE,
        div(
          class = "text-center",
          br(),
          h3(
            style = "line-height: 1.5",
            HTML(paste0(
              'Are you sure you want to cancel the subscription ', tags$b(subscription_name), '?'
            ))
          ),
          br(), br()
        )
      )
    )
  })

  observeEvent(input$submit_cancel, {
    req(sub_info())

    billing <- session$userData$billing()
    subscription <- sub_info()

    tryCatch({

      ## Remove Subscription
      res <- httr::DELETE(
        paste0("https://api.stripe.com/v1/subscriptions/", subscription$id),
        encode = "form",
        httr::authenticate(
          user = app_config$stripe$keys$secret,
          password = ""
        )
      )

      res_content <- jsonlite::fromJSON(
        httr::content(res, "text", encoding = "UTF-8")
      )

      res_code <- httr::status_code(res)
      if (!identical(res_code, 200L)) {
        print(res_content)
        print(paste0("status code: ", res_code))
        stop("unable to delete subscription")
      }

      # Remove Subscription ID from 'billing' table ##
      dbExecute(
        conn,
        "UPDATE billing SET stripe_subscription_id=?, free_trial_days_remaining_at_cancel=? WHERE uid=?",
        params = list(
          NA,
          subscription$trial_days_remaining,
          billing$uid
        )
      )

      session$userData$billing_trigger(session$userData$billing_trigger() + 1)
      shinyFeedback::showToast("success", "Subscription Cancelled Successfully")
    }, error = function(err) {

      print(err)
      shinyFeedback::showToast("error", "Error Cancelling Subscription")
    })

    removeModal()
  })

  output$session_data <- renderPrint({
    checkout_session_data_out()
  })

  observeEvent(session$userData$billing(), {
    billing <- session$userData$billing()

    if (is.na(billing$stripe_subscription_id)) {
      shinyjs::hide("cancel_subscription")
      shinyjs::hide("billing_info_box")
    } else {
      shinyjs::show("cancel_subscription")
      shinyjs::show("billing_info_box")
    }
  })

  output$plan_name_out <- renderText({
    billing <- session$userData$billing()

    if (is.na(billing$stripe_subscription_id)) {
      out <- "No Plan"
    } else {
      req(sub_info())
      out <- sub_info()$nickname
    }

    out
  })

  output$plan_amount_out <- renderText({
    billing <- session$userData$billing()

    if (is.na(billing$stripe_subscription_id)) {
      out <- "$0"
    } else {
      req(sub_info())

      hold <- sub_info()

      amount_out <- paste0(
        "$",
        formatC(hold$amount / 100, format = "f", digits = 2, big.mark = ","),
        "/",
        hold$interval
      )

      # No trial or current time is after trial end
      if (hold$trial_days_remaining <= 0) {

        out <- amount_out
      } else {

        out <- paste0(
          round(hold$trial_days_remaining, 0),
          " Days Remaining in Free Trial then ",
          amount_out
        )
      }
    }

    out
  })

  output$account_created_out <- renderText({
    billing <- session$userData$billing()

    format(billing$created_at, format = "%Y-%m-%d")

    # req(sub_info())
    # format(as.POSIXct(sub_info()$start_date, origin = "1970-01-01"), format = "%Y-%m-%d")
  })

  observe({
    if (is.null(sub_info()$trial_end)) {
      shinyjs::hideElement("has_trial")
    } else {
      shinyjs::showElement("has_trial")
    }
  })

  output$trial_end_out <- renderText({
    # req(sub_info())
    # format(as.POSIXct(sub_info()$trial_end, origin = "1970-01-01"), format = "%Y-%m-%d")

    billing <- session$userData$billing()

    format(billing$created_at + lubridate::days(31), format = "%Y-%m-%d")
  })

  # get payment method information for display to user
  payment_methods <- reactive({
    req(sub_info())
    billing <- session$userData$billing()
    req(!is.na(billing$stripe_subscription_id))

    default_payment_method <- sub_info()$default_payment_method

    res <- httr::GET(
      paste0("https://api.stripe.com/v1/payment_methods/", default_payment_method),
      encode = "form",
      httr::authenticate(
        user = app_config$stripe$keys$secret,
        password = ""
      )
    )


    dat <- jsonlite::fromJSON(
      httr::content(res, "text", encoding = "UTF-8")
    )

    if (!identical(httr::status_code(res), 200L)) {
      print("error getting payment information")
      print(dat)
      return(NULL)
    }


    list(
      "name" = dat$billing_details$name,
      "address" = list(
        "city" = dat$billing_details$address$city,
        "line1" = dat$billing_details$address$line1,
        "line2" = dat$billing_details$address$line2,
        "postal_code" = dat$billing_details$address$postal_code,
        "state" = dat$billing_details$address$state
      ),
      "card_brand" = dat$card$brand,
      "card_last4" = dat$card$last4,
      "exp_month" = dat$card$exp_month,
      "exp_year" = dat$card$exp_year
    )
  })

  observeEvent(payment_methods(), {

    if (is.null(payment_methods())) {

      shinyjs::hideElement("billing_info")
      shinyjs::showElement("enable_billing_button")
    } else {
      shinyjs::showElement("billing_info")
      shinyjs::hideElement("enable_billing_button")
    }

  }, ignoreNULL = FALSE)


  # billing information outputs -----------
  output$name_out <- renderText({
    req(payment_methods())

    payment_methods()$name
  })

  output$postal_code <- renderText({
    req(payment_methods())
    payment_methods()$address$postal_code
  })

  # credit card output --------------------
  output$card_brand_out <- renderText({
    req(payment_methods())
    payment_methods()$card_brand
  })

  output$last_4_out <- renderText({
    req(payment_methods())
    paste0("XXXX-XXXX-XXXX-", payment_methods()$card_last4)
  })

  output$card_exp_out <- renderText({
    req(payment_methods())
    paste0(payment_methods()$exp_month, "/", payment_methods()$exp_year)
  })

  invoices_table_prep <- reactive({
    # Trigger after a subscription change
    session$userData$sub_info_trigger()

    billing <- session$userData$billing()

    res <- httr::GET(
      "https://api.stripe.com/v1/invoices",
      query = list(
        customer = billing$stripe_customer_id
      ),
      encode = "form",
      httr::authenticate(
        user = app_config$stripe$keys$secret,
        password = ""
      )
    )

    httr::stop_for_status(res)

    dat <- jsonlite::fromJSON(
      httr::content(res, "text", encoding = "UTF-8")
    )

    dat$data %>%
      dplyr::select(period_start, period_end, amount_due, amount_paid, amount_remaining) %>%
      dplyr::mutate(
        amount_due = amount_due / 100,
        amount_paid = amount_paid / 100,
        amount_remaining = amount_remaining / 100,
        period_end = as.Date(as.POSIXct(period_end, origin = "1970-01-01")),
        period_start = as.Date(as.POSIXct(period_start, origin = "1970-01-01"))
      )
  })

  output$invoices_table <- DT::renderDT({
    req(invoices_table_prep())
    out <- invoices_table_prep()

    if (nrow(out) > 10) {
      dom_ <- 'lftip'
    } else {
      dom_ <- 'lfti'
    }

    DT::datatable(
      out,
      rownames = FALSE,
      class = "compact cell-border stripe",
      colnames = c(
        "Period Start",
        "Period End",
        "Amount Due",
        "Amount Paid",
        "Amount Remaining"
      ),
      callback = DT::JS("$( table.table().container() ).addClass( 'table-responsive' ); return table;"),
      selection = "none",
      options = list(
        dom = dom_
      )
    ) %>%
      DT::formatCurrency(3:5)
  })

  callModule(
    credit_card_module,
    "enable_billing",
    open_modal_trigger = reactive({input$enable_billing}),
    disclaimer_text = p(
      class = "text-center",
      "Your subscription payments will be paid using the above credit card."
    ),
    sub_info = sub_info
  )

  callModule(
    credit_card_module,
    "change_credit_card",
    open_modal_trigger = reactive({input$update_billing_info}),
    disclaimer_text = p(
      class = "text-center",
      "Future subscription payments will be paid using the above credit card."
    ),
    sub_info = sub_info
  )



}
