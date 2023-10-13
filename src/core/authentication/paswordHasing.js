// Password to hash
const password = 'user_password';

// Derivation parameters
const iterations = 10000;
const keyLength = 20;
const algorithm = 'PBKDF2';
const hashFunction = 'SHA-1';

// Convert the password to an ArrayBuffer
const encoder = new TextEncoder();
const passwordData = encoder.encode(password);

// Generate a random salt
crypto.subtle.generateKey(
  { name: 'AES-CBC', length: 256 },
  true,
  ['encrypt', 'decrypt']
).then((key) => {
  const salt = new Uint8Array(key);

  // Perform the key derivation
  return crypto.subtle.importKey(
    'raw',
    passwordData,
    { name: algorithm },
    false,
    ['deriveBits']
  ).then((baseKey) => {
    return crypto.subtle.deriveBits(
      {
        name: algorithm,
        salt: salt,
        iterations: iterations,
        hash: { name: hashFunction }
      },
      baseKey,
      keyLength * 8 // Output key length in bits
    );
  });
}).then((derivedKey) => {
  // Convert the derived key to a hex string
  const derivedKeyArray = Array.from(new Uint8Array(derivedKey));
  const derivedKeyHex = derivedKeyArray.map(byte => byte.toString(16).padStart(2, '0')).join('');
  
  console.log('Derived Key (hex):', derivedKeyHex);
}).catch((error) => {
  console.error('Error:', error);
});
