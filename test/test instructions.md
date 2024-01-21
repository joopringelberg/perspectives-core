Running testscripts for the PDR
======================

### COOKIES
First of all, running the PDR in the node environment requires XHR-cookies. We depend on AffJax for requests over the internet and this package was built with the cookies-agnostic XHR2 package. I've made a fork of Affjax () that uses XHR2-COOKIES instead.

In order to use XHR2-COOKIES rather than XHR2, you have to edit the next two lines in the file persistenceAPI.js:

```
// var PouchDB = require('pouchdb-browser').default;
var PouchDB = require('pouchdb');
```

### SSL
When testing with an endpoint protected with SSL, make sure to disable node certificate checking:

```
export NODE_TLS_REJECT_UNAUTHORIZED="0"
```

### LOCAL CERTIFICATES

In order to use the self-signed certificate on perspectives.domains, run this in the terminal:

```
export NODE_EXTRA_CA_CERTS="$(mkcert -CAROOT)/rootCA.pem"
```

It exports the root certificate. Without that, the following error will be thrown:

```
"request to https://perspectives.domains/models_perspectives_domains/ failed, reason: unable to verify the first certificate","type":"system","errno":"UNABLE_TO_VERIFY_LEAF_SIGNATURE","code":"UNABLE_TO_VERIFY_LEAF_SIGNATURE"
```

