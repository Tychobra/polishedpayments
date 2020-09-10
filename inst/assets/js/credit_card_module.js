function credit_card_module(ns_prefix) {


  Shiny.addCustomMessageHandler(
    ns_prefix + "create_setup_intent",
    function(message) {

      // COLLECT CARD DETAILS
      var stripe = Stripe(message.stripe_key);


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
      cardElement.mount("#" + ns_prefix + "card_element");

      // CONFIRM SETUP INTENT
      var cardholderName = document.getElementById(ns_prefix + "cardholder_name");
      var cardButton = document.getElementById(message.card_button_id);
      var clientSecret = message.client_secret;

      cardButton.addEventListener("click", function(ev) {

        stripe.confirmCardSetup(
          clientSecret,
          {
            payment_method: {
              card: cardElement,
              billing_details: {
                name: cardholderName.value,
              },
            },
          }
        ).then(function(result) {
          if (result.error) {
            // Display error.message in your UI.

            Shiny.setInputValue(ns_prefix + "setup_intent_error", result.error, { priority: "event"});
            console.log(result.error);
          } else {
            // The setup has succeeded. Display a success message.
            Shiny.setInputValue(ns_prefix + "setup_intent_success", 1, { priority: "event"});
          }
        });
      });


    }
  );
}
