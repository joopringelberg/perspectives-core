exports.formatDateTimeImpl = function( epoch, locale, options)
{
  return new Intl.DateTimeFormat(locale, JSON.parse(options)).format( new Date( epoch ) );
}

exports.pdrVersion = __PDRVersion__;