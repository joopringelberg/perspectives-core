exports._gui = function (speak)
{
  return function(listen)
  {
    return function(channel)
    {
      return function (onError, onSuccess)
      {
        // Listen to Perspectives.
        // Listen applied to the channel yields a promise.
        listen(channel)().then(
          function(v)
          {
            console.log("Received an answer: " + v);
          }
        )
        // Generate an event:
        setTimeout(
          function(v)
          {
            // speak is a function in Eff. We need to run it.
            // The message will be sent as soon as the channel is empty,
            // probably because we've listened to it!
            speak("Hello")(channel)();
            onSuccess({});
          },
          1500,
          "Hello");

        // Return a canceler, which is just another Aff effect.
        return function (cancelError, cancelerError, cancelerSuccess) {
          // cancel whatever
          cancelerSuccess(); // invoke the success callback for the canceler
        };
      };
    };
  };
};
