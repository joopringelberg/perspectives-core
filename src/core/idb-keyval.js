const idbKeyval = require('idb-keyval')

exports.getValueByKeyImpl = idbKeyval.get;

exports.setKeyValueImpl = idbKeyval.set;

exports.clear = function () { idbKeyval.clear(); }