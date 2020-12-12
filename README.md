# Polished Payments

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

R package to easily add a [Stripe](https://stripe.com/) subscription with multiple price tiers to a Shiny app using the [polished R package](https://github.com/Tychobra/polished).

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

4. Configure your Shiny app with your Stripe information using `polished_payments_config` in `global.R`.  
  
      ```
      polishedpayments::polished_payments_config(
        stripe_secret_key = <stripe_secret_key>,
        stripe_public_key = <stripe_public_key>,
        stripe_prices = <stripe_price(s)>,
        trial_period_days = <stripe_trial_period_days>,
        free_roles = <polished_role_for_free_users>
      )
      ```

5. Wrap your Shiny server in `payments_server()`. e.g.

      ```
      my_server <- polishedpayments::payments_server(function(input, output, session) (
        
        # your custom Shiny app's server logic
        
      ))
      
      ```
  
6. Add the "Account" page to your app using `app_module_ui` in `secure_ui` & `app_module` in `secure_server`   
  
    - **NOTE**: You must use "account" as the `id` for `app_module_ui`  
    
    ```
    # Server
    polished::secure_server(my_server, account_module = polishedpayments::app_module)
    
    # UI
    polished::secure_ui(
      ui,
      account_module_ui = polishedpayments::app_module_ui("account")
    )
    ```
  
Your user's subscription status will now be checked before your Shiny app's server logic starts.  If the user does not have a subscription, `polishedpayments` will set them up with the default subscription.  If the user has a subscription that is either in it's free trial period, or the user has set up a payment method, then `polishedpayments` will allow the user to continue to your Shiny app.   If your user has a subscription in which the free trial has expired, but they have not yet enabled a payment method, then `polishedpayments` will redirect them to their account page where they can set up their subscription.   
  
7. (Optional) Add button or link to redirect user to 'Account' page with `go_to_account()`  

- Example app available: <a href="https://github.com/Tychobra/polishedpayments/tree/master/inst/examples/polished_payments_min">https://github.com/Tychobra/polishedpayments/tree/master/inst/examples/polished_payments_min</a>

