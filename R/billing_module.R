#' billing_module_ui
#'
#' @param id the module id
#'
#'
billing_module_ui <- function(id) {
  ns <- NS(id)

  subscription_ui <- tagList(
    if (!is.null(.pp$prices)) {
      tagList(
        shiny::fluidRow(
          shinydashboard::box(
            title = "Subscription Plan",
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
        )
      )
    }
  )

  tagList(
    tags$style(paste0("
      #", ns('invoices_table'), " th,
      #", ns('invoices_table'), " td {
        text-align: center;
      }
    ")),
    subscription_ui,
    tags$div(
      id = ns("billing_info_box"),
      shiny::fluidRow(
        shinydashboard::box(
          title = "Payment Method",
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
                ns("change_payment_method"),
                "Change",
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

    #credit_card_module_ui(
    #  ns("change_credit_card")
    #)
  )
}


#' billing module
#'
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#'
#' @importFrom dplyr %>% select mutate .data arrange bind_rows desc
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON
#' @importFrom shiny callModule req showModal modalDialog modalButton removeModal observeEvent renderPrint renderText observe reactive
#' @importFrom htmltools tags HTML
#' @importFrom httr DELETE PUT GET content status_code
#'
billing_module <- function(input, output, session) {
  ns <- session$ns

  ### SUBSCRIPTION ONLY LOGIC ###
  if (!is.null(.pp$prices)) {

    ### CANCEL SUBSCRIPTION ###
    observeEvent(input$cancel_subscription, {
      req(session$userData$stripe(), !is.na(session$userData$stripe()$subscription))
      sub_info <- session$userData$stripe()$subscription
      subscription_name <- sub_info$nickname

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
                'Are you sure you want to cancel the ', tags$b(subscription_name), ' subscription?'
              ))
            ),
            tags$br(), tags$br()
          )
        )
      )
    })

    shiny::observeEvent(input$submit_cancel, {
      shiny::req(session$userData$stripe(), !is.na(session$userData$stripe()$subscription))

      hold_user <- session$userData$user()
      hold_stripe <- session$userData$stripe()
      subscription <- hold_stripe$subscription


      tryCatch({

        ## Remove Subscription
        res <- httr::DELETE(
          paste0("https://api.stripe.com/v1/subscriptions/", subscription$stripe_subscription_id),
          encode = "form",
          httr::authenticate(
            user = .pp$keys$secret,
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
        # remaining at cancel. The "trial_days_remaining" will be used
        # to set the proper amount of free trial days if the user restarts their subscription.
        update_res <- update_customer(
          customer_uid = hold_stripe$polished_customer_uid,
          cancel_subscription = TRUE,
          free_trial_days_remaining_at_cancel = hold_stripe$trial_days_remaining
        )

        if (!identical(httr::status_code(update_res$response), 200L)) {

          stop(update_res$content$error, call. = FALSE)
        }

        session$userData$stripe(
          get_stripe(
            user_uid = hold_user$user_uid
          )
        )

        #session$userData$stripe_trigger(session$userData$stripe_trigger() + 1)
        shinyFeedback::showToast("success", "Subscription Cancelled Successfully")
      }, error = function(err) {

        print(err)
        shinyFeedback::showToast("error", "Error Cancelling Subscription")
      })

      shiny::removeModal()
    })

    shiny::observeEvent(session$userData$stripe(), {

      subscription <- session$userData$stripe()$subscription

      if (is.na(subscription[[1]]) || is.na(subscription$stripe_subscription_id)) {
        shinyjs::hide("cancel_subscription")
      } else {
        shinyjs::show("cancel_subscription")
      }
    })


    output$plan_name_out <- shiny::renderText({
      req(session$userData$stripe())
      subscription <- session$userData$stripe()$subscription

      if (is.na(subscription[[1]]) || is.na(subscription$stripe_subscription_id)) {
        out <- "No Plan"
      } else {
        out <- subscription$nickname
      }

      out
    })

    output$plan_amount_out <- shiny::renderText({
      req(session$userData$stripe())
      hold_stripe <- session$userData$stripe()
      subscription <- hold_stripe$subscription


      if (is.na(subscription[[1]]) || is.na(subscription$stripe_subscription_id)) {

        if (is.na(hold_stripe$trial_days_remaining)) {
          out <- ""
        } else if (hold_stripe$trial_days_remaining > 0) {
          out <- paste0("You have ", ceiling(hold_stripe$trial_days_remaining), " days left in your trial.  Enable a plan below to resume your free trial.")
        } else {
          out <- "Enable a subscription below"
        }

      } else {
        amount_out <- paste0(
          "$",
          formatC(subscription$amount / 100, format = "f", digits = 2, big.mark = ","),
          "/",
          subscription$interval
        )

        # No trial or current time is after trial end
        if (hold_stripe$trial_days_remaining <= 0) {

          out <- amount_out
        } else {

          out <- paste0(
            round(hold_stripe$trial_days_remaining, 0),
            " Days Remaining in Free Trial then ",
            amount_out
          )
        }
      }

      out
    })

    output$account_created_out <- shiny::renderText({
      req(!is.na(session$userData$stripe()$subscription))
      subscription <- session$userData$stripe()$subscription

      as.character(as.Date(subscription$created_at))
    })

    shiny::observe({
      req(!is.na(session$userData$stripe()$subscription))

      if (is.null(session$userData$stripe()$subscription$trial_end)) {
        shinyjs::hideElement("has_trial")
      } else {
        shinyjs::showElement("has_trial")
      }
    })


    callModule(
      plans_box_module,
      "my_plans"
    )


  } else {
    # Subscription mode NOT enabled

    waiter::waiter_hide()
  }


  # get payment method information for display to user
  default_payment_method <- shiny::reactive({
    req(session$userData$stripe())


    hold_stripe <- session$userData$stripe()
    if (is.na(hold_stripe$default_payment_method)) {
      return(NULL)
    }


    out <- NULL

    tryCatch({

      out <- get_stripe_payment_method(hold_stripe$default_payment_method)

    }, error = function(err) {

      msg <- "unable to get payment method"
      print(msg)
      print(err)
      showToast("error", msg)

      invisible(NULL)
    })

    out
  })

  observeEvent(default_payment_method(), {


    if (is.null(default_payment_method())) {
      shinyjs::hideElement("billing_info")
      shinyjs::showElement("enable_billing_button")
    } else {
      shinyjs::showElement("billing_info")
      shinyjs::hideElement("enable_billing_button")
    }

  }, ignoreNULL = FALSE)


  # billing information outputs -----------
  output$name_out <- shiny::renderText({
    req(default_payment_method())

    default_payment_method()$name
  })



  output$postal_code <- shiny::renderText({
    req(default_payment_method())
    default_payment_method()$address$postal_code
  })

  # credit card output --------------------
  output$card_brand_out <- shiny::renderText({
    req(default_payment_method())
    default_payment_method()$card_brand
  })

  output$last_4_out <- shiny::renderText({
    req(default_payment_method())
    paste0("XXXX-XXXX-XXXX-", default_payment_method()$card_last4)
  })

  output$card_exp_out <- shiny::renderText({
    req(default_payment_method())
    paste0(default_payment_method()$exp_month, "/", default_payment_method()$exp_year)
  })

  empty_invoices_table <- tibble::tibble(
    created = as.Date(character(0)),
    description = character(0),
    amount = double(0),
    amount_paid = double(0),
    seller_message = character(0),
    paid_with = character(0)
  )

  invoices_table_prep <- shiny::reactive({

    billing <- session$userData$stripe()

    out <- NULL
    if (is.null(billing)) {
      out <- empty_invoices_table
    } else {
      tryCatch({
        res <- httr::GET(
          "https://api.stripe.com/v1/charges",
          query = list(
            customer = billing$stripe_customer_id
          ),
          encode = "form",
          httr::authenticate(
            user = .pp$keys$secret,
            password = ""
          )
        )

        dat <- jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        )

        if (!identical(httr::status_code(res), 200L)) {
          print(list(stripe_customer_id = billing$stripe_customer_id))
          stop(dat, call. = FALSE)
        }


        if (identical(length(dat$data), 0L)) {
          out <- empty_invoices_table
        } else {
          out <- dat$data %>%
            dplyr::select(.data$id, .data$receipt_url, .data$created, .data$description, .data$amount, .data$status) %>%
            dplyr::mutate(
              amount = .data$amount / 100,
              receipt_url = paste0("<a href='", receipt_url, "' target = 'blank_'>", id, "</a>")
              #amount_paid = .data$amount_captured / 100#,
              #created = as.Date(as.POSIXct(.data$created, origin = "1970-01-01"))
            ) %>%
            select(-id)

          pm <- dat$data$payment_method_details$card %>%
            dplyr::select(network, last4) %>%
            dplyr::mutate(paid_with = paste0(tools::toTitleCase(network), " ending in ", last4)) %>%
            dplyr::select(paid_with)

          out <- dplyr::bind_cols(
            out,
            pm
          ) %>%
            dplyr::arrange(dplyr::desc(created)) %>%
            dplyr::mutate(created = as.Date(as.POSIXct(.data$created, origin = "1970-01-01")))
        }

      }, error = function(err) {

        msg <- "unable to get invoices"
        print(msg)
        print(err)
        shinyFeedback::showToast("error", msg)

        invisible()
      })
    }


    out
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
      escape = -1,
      colnames = c(
        "Invoice",
        "Date",
        "Description",
        "Amount",
        "Status",
        "Paid With"
      ),
      callback = DT::JS("$( table.table().container() ).addClass( 'table-responsive' ); return table;"),
      selection = "none",
      options = list(
        dom = dom_
      )
    ) %>%
      DT::formatCurrency(4)
  })



  # enable billing by setting the default payment method without choosing a subscription
  shiny::callModule(
    set_payment_method_modal,
    ns("enable_billing_modal"),
    open_modal_trigger = reactive({input$enable_billing}),
    title = "Enable Billing"
  )

  # change default payment method
  shiny::callModule(
    set_payment_method_modal,
    ns("change_pm_modal"),
    open_modal_trigger = reactive({input$change_payment_method}),
    title = "Change Card"
  )

}
