# Polished Payments

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

R package to easily add a [Stripe](https://stripe.com/) subscription with multiple price tiers to a Shiny app using the [polished R package](https://github.com/Tychobra/polished).

### Installation & Dependencies

1. This package requires the most recent version of the `{polished}` package. Install it from `CRAN` using the following command:

```R
# Install `polished` if you don't already have it
install.packages("polished")

# Alternatively, install the most recent development version
#   - NOTE: The most recent development version of `polished` may contain bugs or breaking changes. We recommend using the most recent CRAN version.
remotes::install_github("tychobra/polished")
```

2. Create a [Polished](https://polished.tech) account at the [Polished Dashboard](https://dashboard.polished.tech) if you don't have one already.  


3. Then install the most recent version of `{polishedpayments}`:
```R
remotes::install_github("tychobra/polishedpayments")
```


### Getting Started

1. Create a [Stripe](https://stripe.com/) account  

2. Create a new Stripe **Product**.  
  
![](./docs/images/01_stripe_product_screenshot.png)

3. Create 1 to 4 Stripe **Prices**  
  
![](./docs/images/02_stripe_prices_screenshot.png)

4. Configure your Shiny app with your Stripe information using `polished_payments_config()` in `global.R`.  
  
      ```R
      polishedpayments::polished_payments_config(
        stripe_secret_key = <your Stripe secret key>,
        stripe_public_key = <your Stripe public key>,
        subscription_prices = <your Stripe subscription price(s)>,
        trial_period_days = <Stripe subscription trial period days>
      )
      ```

5. Wrap your Shiny server in `polishedpayments::payments_server()` and `polished::secure_server()`. e.g.

      ```R
      my_server <- polishedpayments::payments_server(
        server = function(input, output, session) (
        
        # your custom Shiny app's server logic
        
        )
      )
      
      polished::secure_server(my_server)
      
      ```
  
6. Wrap your Shiny UI in `polishedpayments::payments_ui()` and `polished::secure_ui()`.   
    
    ```R
    my_ui <- polishedpayments::payments_ui(fluidPage(
      h1("My Shiny App")
    ))
    
    polished::secure_ui(my_ui)
    ```
  
Each user's subscription status will be checked before the Shiny app's server logic starts.  If the user does not have a subscription, `polishedpayments` will set them up with the default subscription.  If the user has a subscription that is either in it's free trial period, or the user has set up a payment method for a subscription, then `polishedpayments` will allow the user to continue to the Shiny app after signing in.  If the user has a subscription in which the free trial has expired, and they still have not enabled a payment method, `polishedpayments` will redirect the user to the **Payments** page to enable billing for their subscription.  In this case (expired trial period), the user will not be able to access the main Shiny app until they've enabled billing.
  
7. (Optional) Add a button or link to redirect user to the **Payments** page with `go_to_payments()`  

- Source code for example apps is available [here](https://github.com/Tychobra/polishedpayments/tree/master/inst/examples)
