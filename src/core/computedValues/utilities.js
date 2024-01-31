exports.formatDateTimeImpl = function( epoch, locale, options)
{
  const opts = JSON.parse(options);
  // opts.timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;
  opts.timeZone = "UTC"
  return new Intl.DateTimeFormat(locale, opts).format( new Date( epoch ) );
}

exports.pdrVersion = __PDRVersion__;
