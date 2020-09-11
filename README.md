# Polished Hosting

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

R package to easily add a [Stripe](https://stripe.com/) subscription with multiple price tiers to a Shiny app using the [polished R package](https://github.com/Tychobra/polished).

# Don't use this yet.  Package is still undocumented and in flux. 

### Installation

```
remotes::install_github("tychobra/polished")
```

### Getting Started

1. Create a Stripe account

2. Create a new [Stripe](https://stripe.com/) "Product". 

![](https://res.cloudinary.com/dxqnb8xjb/image/upload/v1599855757/Screen_Shot_2020-09-11_at_3.19.38_PM_hq6c89.png)

3. Create 1 to 4 Stripe "Price"s

![](https://res.cloudinary.com/dxqnb8xjb/image/upload/v1599855858/Screen_Shot_2020-09-11_at_4.23.43_PM_pgrt4r.png)

4. Call `polished_payments_config()` in your global.R and pass the `polishedpayments::app_module` to your `polished::secure_server()` and `polished::secure_ui()` function.  Example app available <a href="https://github.com/Tychobra/polishedpayments/tree/master/inst/examples/polished_payments_min">https://github.com/Tychobra/polishedpayments/tree/master/inst/examples/polished_payments_min</a>

