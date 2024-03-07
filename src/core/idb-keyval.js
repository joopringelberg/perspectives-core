const idbKeyval = require('idb-keyval')

exports.getValueByKeyImpl = idbKeyval.get;

exports.setKeyValueImpl = idbKeyval.set;