function credit_card_module(ns_prefix) {

  var elements = stripe.elements({
    fonts: [
      {
        cssSrc: 'https://fonts.googleapis.com/css?family=Roboto',
      },
    ],
    // Stripe's examples are localized to specific languages, but if
    // you wish to have Elements automatically detect your user's locale,
    // use `locale: 'auto'` instead.
    locale: 'auto'
    // locale: window.__exampleLocale
  });

  var cardElement = elements.create('card', {
    iconStyle: 'solid',
    style: {
      base: {
        iconColor: '#2491eb',
        color: '#000',
        fontWeight: 500,
        fontFamily: 'Roboto, Open Sans, Segoe UI, sans-serif',
        fontSize: '16px',
        fontSmoothing: 'antialiased',
        ':-webkit-autofill': {
          color: '#fce883',
        },
        '::placeholder': {
          color: '#87BBFD',
        },
      },
      invalid: {
        iconColor: '#dd4b39',
        color: '#dd4b39',
      },
    },
  });

  const credit_card_id = ns_prefix + "credit_card"
  cardElement.mount("#" + credit_card_id)

  const credit_card_el = document.getElementById(credit_card_id)




  Shiny.addCustomMessageHandler(
    ns_prefix + "create_payment",
    function(message) {

      debugger
      // TODO: Allow saved cards (after removing `is.na(default_payment_method)`) checks in R

      // var attachPaymentMethod = document.getElementById(ns_prefix + "attach_payment_method").checked;
      // var hold_future_usage;
      // if (attachPaymentMethod) {
      //   hold_future_usage = "on_session";
      // } else {
      //   hold_future_usage = null;
      // }

      stripe.confirmCardPayment(
        message.client_secret,
        {
          payment_method: {
            card: cardElement,
            billing_details: {
              name: "Andy" //cardholderName.value,
            },
          },
          //setup_future_usage: hold_future_usage
        }
      ).then(function(result) {

        if (result.error) {
        // Display error.message in your UI.
          console.error(result.error)
        }
        // send the result back to Shiny
        Shiny.setInputValue(ns_prefix + "payment_intent_result", result, { priority: "event"});

      })
    }

  )


  Shiny.addCustomMessageHandler(
    ns_prefix + "create_subscription",
    function(message) {
      stripe.confirmCardSetup(
      clientSecret,
      //    {
      //      payment_method: {
      //        card: cardElement,
      //        billing_details: {
      //          name: cardholderName.value,
      //        },
      //      },
      //    }
      //  ).then(function(result) {
      )
    }

  )




      //    if (result.error) {
      //      // Display error.message in your UI.

      //      Shiny.setInputValue(ns_prefix + "setup_intent_error", result.error, { priority: "event"});
      //      console.log(result.error);
      //    } else {
      //      // The setup has succeeded. Display a success message.
      //      Shiny.setInputValue(ns_prefix + "setup_intent_success", 1, { priority: "event"});
      //    }
      //  });
      //});


  //  }
  //)

  //Shiny.addCustomMessageHandler(
  //  ns + "payment_method",
    // TODO: Allow saved cards (after removing `is.na(default_payment_method)`) checks in R

    // var attachPaymentMethod = document.getElementById(ns_prefix + "attach_payment_method").checked;
    // var hold_future_usage;
    // if (attachPaymentMethod) {
    //   hold_future_usage = "on_session";
    // } else {
    //   hold_future_usage = null;
    // }

  //  stripe.confirmCardPayment(
  //    clientSecret,
  //    {
  //      payment_method: {
  //        card: cardElement,
  //        billing_details: {
  //          name: cardholderName.value,
  //        },
  //      },
  //      //setup_future_usage: hold_future_usage
  //    }
  //  ).then(function(result) {

  //    if (result.error) {
  //    // Display error.message in your UI.
  //      toastr.error(result.error.message)
  //      LoadingButtons.resetLoading(message.card_button_id)
  //      console.error(result.error)
  //    } else {
  //      toastr.success("Payment completed successfully")
  //    }
  //    // send the result back to Shiny
  //    Shiny.setInputValue(ns_prefix + "payment_intent_result", result, { priority: "event"});
  //  });
  //)

}
