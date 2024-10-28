import * as idbKeyval from 'idb-keyval';

export const getValueByKeyImpl = idbKeyval.get;

export const setKeyValueImpl = idbKeyval.set;

export function clear () { idbKeyval.clear(); }