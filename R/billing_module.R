#' billing_module_ui
#'
#' @param id the module id
#'
#'
billing_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(paste0("
      #", ns('invoices_table'), " th,
      #", ns('invoices_table'), " td {
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
            tags$h4(shiny::textOutput(ns("plan_name_out")))
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
            tags$h4(shiny::textOutput(ns("plan_amount_out")))
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
            tags$h4(shiny::textOutput(ns("account_created_out")))
          ),
          tags$hr(style = "margin: 0;")
        ),
        shiny::column(
          12,
          id = ns("has_trial"),
          tags$div(
            style = "width: 150px; display: inline-block;",
            tags$h4(tags$strong("Trial End"))
          ),
          tags$div(
            style = "display: inline-block",
            tags$h4(shiny::textOutput(ns("trial_end_out")))
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


    shiny::fluidRow(
      plans_box_module_ui(ns("my_plans"))
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
                tags$h4(shiny::textOutput(ns("name_out")))
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
                tags$h4(shiny::textOutput(ns("postal_code")))
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
                tags$h4(shiny::textOutput(ns("card_brand_out")))
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
                tags$h4(shiny::textOutput(ns("last_4_out")))
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
                tags$h4(shiny::textOutput(ns("card_exp_out")))
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
            tags$h3("Billing is not enabled"),
            tags$br(),
            shiny::actionButton(
              ns("enable_billing"),
              "Enable Billing",
              class = "btn-primary btn-lg",
              style = "color: #FFF"
            ),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br()
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
    tags$br(), tags$br(), tags$br(),
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
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#' @param sub_info the subscription information
#'
#' @importFrom dplyr %>% select mutate .data
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON
#' @importFrom shiny callModule req showModal modalDialog modalButton removeModal observeEvent renderPrint renderText observe reactive
#' @importFrom htmltools tags HTML
#' @importFrom httr DELETE PUT GET content status_code
#'
billing_module <- function(input, output, session, sub_info) {
  ns <- session$ns


  ### CANCEL SUBSCRIPTION ###
  observeEvent(input$cancel_subscription, {
    req(sub_info())
    subscription_name <- sub_info()$nickname

    shiny::showModal(
      shiny::modalDialog(
        title = "Cancel Subscription",
        footer = list(
          shiny::modalButton("No, Close"),
          shiny::actionButton(
            ns("submit_cancel"),
            "Yes, Submit",
            class = "btn-danger",
            style = "color: #FFF"
          )
        ),
        size = "m",
        easyClose = TRUE,
        tags$div(
          class = "text-center",
          tags$br(),
          tags$h3(
            style = "line-height: 1.5",
            htmltools::HTML(paste0(
              'Are you sure you want to cancel the ', tags$b(subscription_name), 'subscription?'
            ))
          ),
          tags$br(), tags$br()
        )
      )
    )
  })

  shiny::observeEvent(input$submit_cancel, {
    shiny::req(sub_info())

    billing <- session$userData$billing()
    subscription <- sub_info()

    tryCatch({

      ## Remove Subscription
      res <- httr::DELETE(
        paste0("https://api.stripe.com/v1/subscriptions/", subscription$id),
        encode = "form",
        httr::authenticate(
          user = getOption("pp")$keys$secret,
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

      # Remove Subscription ID from 'billing' table and update the free trial days
      # remaining at cancel. The "free_trial_days_remaining_at_cancel" will be used
      # to set the proper amount of free trial days if the user restarts their subscription.
      res <- httr::PUT(
        url = paste0(getOption("polished")$api_url, "/subscriptions"),
        encode = "json",
        body = list(
          subscription_uid = billing$uid,
          stripe_subscription_id = NA,
          free_trial_days_remaining_at_cancel = subscription$trial_days_remaining
        ),
        httr::authenticate(
          user = getOption("polished")$api_key,
          password = ""
        )
      )

      if (!identical(httr::status_code(res), 200L)) {

        res_content <- jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        )

        stop(res_content, call. = FALSE)
      }

      session$userData$billing_trigger(session$userData$billing_trigger() + 1)
      shinyFeedback::showToast("success", "Subscription Cancelled Successfully")
    }, error = function(err) {

      print(err)
      shinyFeedback::showToast("error", "Error Cancelling Subscription")
    })

    shiny::removeModal()
  })

  shiny::observeEvent(session$userData$billing(), {
    billing <- session$userData$billing()

    if (is.na(billing$stripe_subscription_id)) {
      shinyjs::hide("cancel_subscription")
      shinyjs::hide("billing_info_box")
    } else {
      shinyjs::show("cancel_subscription")
      shinyjs::show("billing_info_box")
    }
  })


  output$plan_name_out <- shiny::renderText({
    billing <- session$userData$billing()

    if (is.na(billing$stripe_subscription_id)) {
      out <- "No Plan"
    } else {
      shiny::req(sub_info())
      out <- sub_info()$nickname
    }

    out
  })

  output$plan_amount_out <- shiny::renderText({
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

  output$account_created_out <- shiny::renderText({
    billing <- session$userData$billing()

    as.character(as.Date(billing$created_at))
  })

  shiny::observe({
    if (is.null(sub_info()$trial_end)) {
      shinyjs::hideElement("has_trial")
    } else {
      shinyjs::showElement("has_trial")
    }
  })

  output$trial_end_out <- renderText({
    req(sub_info())
    hold <- sub_info()
    as.character(Sys.Date() + hold$trial_days_remaining)
  })


  callModule(
    plans_box_module,
    "my_plans",
    sub_info = sub_info
  )


  # get payment method information for display to user
  payment_methods <- shiny::reactive({
    req(sub_info())
    billing <- session$userData$billing()
    req(!is.na(billing$stripe_subscription_id))

    default_payment_method <- sub_info()$default_payment_method

    if (is.na(default_payment_method)) {
      out <- NULL
    } else {
      res <- httr::GET(
        paste0("https://api.stripe.com/v1/payment_methods/", default_payment_method),
        encode = "form",
        httr::authenticate(
          user = getOption("pp")$keys$secret,
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


      out <- list(
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
    }

    out
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
  output$name_out <- shiny::renderText({
    req(payment_methods())

    payment_methods()$name
  })

  output$postal_code <- shiny::renderText({
    req(payment_methods())
    payment_methods()$address$postal_code
  })

  # credit card output --------------------
  output$card_brand_out <- shiny::renderText({
    req(payment_methods())
    payment_methods()$card_brand
  })

  output$last_4_out <- shiny::renderText({
    req(payment_methods())
    paste0("XXXX-XXXX-XXXX-", payment_methods()$card_last4)
  })

  output$card_exp_out <- shiny::renderText({
    req(payment_methods())
    paste0(payment_methods()$exp_month, "/", payment_methods()$exp_year)
  })

  invoices_table_prep <- shiny::reactive({
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
        user = getOption("pp")$keys$secret,
        password = ""
      )
    )

    httr::stop_for_status(res)

    dat <- jsonlite::fromJSON(
      httr::content(res, "text", encoding = "UTF-8")
    )

    dat$data %>%
      dplyr::select(.data$period_start, .data$period_end, .data$amount_due, .data$amount_paid, .data$amount_remaining) %>%
      dplyr::mutate(
        amount_due = .data$amount_due / 100,
        amount_paid = .data$amount_paid / 100,
        amount_remaining = .data$amount_remaining / 100,
        period_end = as.Date(as.POSIXct(.data$period_end, origin = "1970-01-01")),
        period_start = as.Date(as.POSIXct(.data$period_start, origin = "1970-01-01"))
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

  shiny::callModule(
    credit_card_module,
    "enable_billing",
    open_modal_trigger = reactive({input$enable_billing}),
    disclaimer_text = tags$p(
      class = "text-center",
      "Your subscription payments will be paid using the above credit card."
    ),
    sub_info = sub_info
  )

  shiny::callModule(
    credit_card_module,
    "change_credit_card",
    open_modal_trigger = reactive({input$update_billing_info}),
    disclaimer_text = tags$p(
      class = "text-center",
      "Future subscription payments will be paid using the above credit card."
    ),
    sub_info = sub_info
  )



}
