import {init} from '@paralleldrive/cuid2';

// The init function returns a custom createId function with the specified
// configuration. All configuration properties are optional.
export const createSystemIdentifier = init({
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
export function cuid2 (userId) 
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
