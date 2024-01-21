// NOTE. This module is not complete. Neither is it necessary, because in the near future 
// we'll generate passwords and so have no need to hash them to protect users who
// re-use passwords.
// For now, as we're bound to do more cryptographic work, I just let this code exist as an inspiration


function hashPasswordForRabbitmq( password )
{
  const salt = generateRandomSalt();
  // prepend a random 4 byte salt to the utf8 encoding of the password.
  const s1 = concat( salt, utf8Encode( password ));

  // take the SHA-256 hash
  const s2 = s1

  // Concatenate the salt again:
  const s3 = concat( salt, s2 );

  // Convert to base64 encoding

  return s1;
}

// https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues
// Uint8Array
function generateRandomSalt ()
{
  const array = new Uint8Array(4);
  self.crypto.getRandomValues(array);
  return array;
}

// utf8 encode a string.
// See: https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder/encodeInto
function utf8Encode (s)
{
  const textEncoder = new TextEncoder();
  const utf8 = new Uint8Array(s.length*3);
  textEncoder.encodeInto(s, utf8);
  // We now have almost certainly too long an Uint8Array.
  return utf8.slice(0, utf8.findIndex(x => x == 0))
}

function concat(a1, a2) {
  // sum of individual array lengths
  let result = new Uint8Array(a1.length + a2.length);
  result.set(a1, 0);
  result.set(a2, a1.length);
  return result;
}

