# Polished Payments

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

R package to easily add a [Stripe](https://stripe.com/) subscription with multiple price tiers to a Shiny app using the [polished R package](https://github.com/Tychobra/polished).

# Don't use this yet.  Package is still undocumented and in flux. 

### Installation

```
# Install `polished` if you don't already have it
remotes::install_github("tychobra/polished")

remotes::install_github("tychobra/polishedpayments")
```

### Getting Started

1. Create a Stripe account

2. Create a new [Stripe](https://stripe.com/) "Product". 

![](https://res.cloudinary.com/dxqnb8xjb/image/upload/v1599855757/Screen_Shot_2020-09-11_at_3.19.38_PM_hq6c89.png)

3. Create 1 to 4 Stripe "Price"s

![](https://res.cloudinary.com/dxqnb8xjb/image/upload/v1599855858/Screen_Shot_2020-09-11_at_4.23.43_PM_pgrt4r.png)

4. Add `polished_payments_config()` & `polishedpayments::app_module` to your Shiny app.  

    - In `global.R`:
    ```
    polished_payments_config(
      stripe_secret_key = <stripe_secret_key>,
      stripe_public_key = <stripe_public_key>,
      stripe_prices = <stripe_price(s)>,
      trial_period_days = <stripe_trial_period_days>,
      # Assign 'User Role(s)' with 'polished' to users that don't require payment
      #     and supply the name of the role(s) to 'free_roles' argument below
      free_roles = <polished_role_for_free_users>
    )
    ```

    - In `polished` secure `Server` & `UI`:
    ```
    # Server
    polished::secure_server(
      server,
      account_module = polishedpayments::app_module
    )
    
    # UI
    secure_ui(
      ui,
      account_module_ui = polishedpayments::app_module_ui("account")
    )
    ```
    
    - In `server`:
    ```
    # This function should be placed at the top of your Shiny server function
    polishedpayments::check_user_subscription()
    
    # Add a button/link to go to the 'Account' (i.e. payments) page & update the query string for this event to redirect to the 'Account' page
    observeEvent(input$go_to_payments, {
      shiny::updateQueryString(
        queryString = "?page=account",
        session = session,
        mode = "replace"
      )
      session$reload()
    })
    ```

    - Example app available <a href="https://github.com/Tychobra/polishedpayments/tree/master/inst/examples/polished_payments_min">https://github.com/Tychobra/polishedpayments/tree/master/inst/examples/polished_payments_min</a>

