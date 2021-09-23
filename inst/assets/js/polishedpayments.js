const payments = (() => {

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



  const mount_card_element = (el_id) => {
    const card_element = elements.create('card', {
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

    card_element.mount("#" + el_id)

    return card_element
  }





  $(document).on("shiny:sessioninitialized", () => {
    Shiny.addCustomMessageHandler(
      "confirm_card_payment",
      function(message) {

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
              card: card_element,
              billing_details: message.billing_details,
            },
            //setup_future_usage: hold_future_usage
          }
        ).then(function(result) {

          if (result.error) {
          // Display error.message in your UI.
            console.error(result.error)
          }
          // send the result back to Shiny
          Shiny.setInputValue(message.ns_prefix + "payment_intent_result", result, { priority: "event"});

        })
      }

    )


    Shiny.addCustomMessageHandler(
      "confirm_card_setup",
      function(message) {

        const card_element = elements.getElement("card")

        stripe.confirmCardSetup(
          message.client_secret,
          {
            payment_method: {
              card: card_element,
              billing_details: message.billing_details
            }
          }
        ).then(function(result) {
          if (result.error) {
            // Display error.message in your UI.

            console.log(result.error);
          }

          // The setup has succeeded. Display a success message.
          Shiny.setInputValue(message.ns_prefix + "setup_intent_result", result, { priority: "event"});

        })

      }
    )
  })



  return {
    mount_card_element: mount_card_element
  }
})()
