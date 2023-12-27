
const idbKeyval = require('idb-keyval')

exports.getCryptoKeyImpl = idbKeyval.get;

exports.bytesToBase64DataUrlImpl = function (bytes) {
  const type = "application/octet-stream";
  return new Promise((resolve, reject) => 
  {
    const reader = Object.assign(new FileReader(), 
      {
        onload: function() {return resolve(reader.result)},
        onerror: function() { return reject(reader.error) },
      });
    reader.readAsDataURL(new File([bytes], "", { type: type }));
  })
}

exports.dataUrlToBytesImpl = function (dataUrl) 
{
  return fetch(dataUrl)
    .then( function(res) 
      { 
        return res.arrayBuffer()
      })
    .then( function (buff) { return new Uint8Array( buff) } ) 
  }
