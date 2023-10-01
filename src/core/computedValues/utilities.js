exports.formatDateTimeImpl = function( datetime, locale, options)
{
  return new Intl.DateTimeFormat(locale, options).format( datetime );
}