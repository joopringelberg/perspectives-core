// Various Javascript engines seem to have different shapes for errors.
// That is difficult to handle in Purescript.
// The function below applies an empirically grown collection of tests to see if the error
// originates from a signal from Couchdb that the request is not authorized.

export function isUnauthorized (e)
{
  var unauthorizedRegex = new RegExp(/unauthorized/i);
  return (
    (e.message && null != e.message.match(unauthorizedRegex)) ||
    (e.error && null != e.error.match(unauthorizedRegex)) ||
    (e.name && null != e.name.match(unauthorizedRegex)) ||
    (e.status == 401)
    );
}
