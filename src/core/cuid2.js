const init = require ('@paralleldrive/cuid2').init;

// The init function returns a custom createId function with the specified
// configuration. All configuration properties are optional.
exports.createSystemIdentifier = init({
  // A custom random function with the same API as Math.random.
  // You can use this to pass a cryptographically secure random function.
  random: Math.random,
  // the length of the id
  length: 10,
  // A custom fingerprint for the host environment. This is used to help
  // prevent collisions when generating ids in a distributed system.
  fingerprint: 'a-custom-host-fingerprint',
});

// A function that takes the user ID and uses that as fingerprint.
exports.cuid2 = function (userId) 
{
  return init({
  // A custom random function with the same API as Math.random.
  // You can use this to pass a cryptographically secure random function.
  random: Math.random,
  // the length of the id
  length: 10,
  // A custom fingerprint for the host environment. This is used to help
  // prevent collisions when generating ids in a distributed system.
  fingerprint: userId,
});
}
