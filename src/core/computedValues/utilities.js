export function formatDateTimeImpl( epoch, locale, options)
{
  const opts = JSON.parse(options);
  // opts.timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;
  opts.timeZone = "UTC"
  return new Intl.DateTimeFormat(locale, opts).format( new Date( epoch ) );
}

export const pdrVersion = (typeof __PDRVersion__ == 'undefined') ? "" : __PDRVersion__;
export const mycontextsUrl = __MYCONTEXTS__;