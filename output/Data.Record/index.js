// Generated by purs version 0.11.7
"use strict";
var Data_Eq = require("../Data.Eq");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Record_Unsafe = require("../Data.Record.Unsafe");
var Data_Symbol = require("../Data.Symbol");
var Prelude = require("../Prelude");
var Type_Row = require("../Type.Row");
var EqualFields = function (equalFields) {
    this.equalFields = equalFields;
};
var set = function (dictIsSymbol) {
    return function (dictRowCons) {
        return function (dictRowCons1) {
            return function (l) {
                return function (b) {
                    return function (r) {
                        return Data_Record_Unsafe.unsafeSetFn(Data_Symbol.reflectSymbol(dictIsSymbol)(l), b, r);
                    };
                };
            };
        };
    };
};
var insert = function (dictIsSymbol) {
    return function (dictRowLacks) {
        return function (dictRowCons) {
            return function (l) {
                return function (a) {
                    return function (r) {
                        return Data_Record_Unsafe.unsafeSetFn(Data_Symbol.reflectSymbol(dictIsSymbol)(l), a, r);
                    };
                };
            };
        };
    };
};
var get = function (dictIsSymbol) {
    return function (dictRowCons) {
        return function (l) {
            return function (r) {
                return Data_Record_Unsafe.unsafeGetFn(Data_Symbol.reflectSymbol(dictIsSymbol)(l), r);
            };
        };
    };
};
var modify = function (dictIsSymbol) {
    return function (dictRowCons) {
        return function (dictRowCons1) {
            return function (l) {
                return function (f) {
                    return function (r) {
                        return set(dictIsSymbol)(dictRowCons)(dictRowCons1)(l)(f(get(dictIsSymbol)(dictRowCons)(l)(r)))(r);
                    };
                };
            };
        };
    };
};
var equalFieldsNil = new EqualFields(function (v) {
    return function (v1) {
        return function (v2) {
            return true;
        };
    };
});
var equalFields = function (dict) {
    return dict.equalFields;
};
var equalFieldsCons = function (dictIsSymbol) {
    return function (dictEq) {
        return function (dictRowCons) {
            return function (dictEqualFields) {
                return new EqualFields(function (v) {
                    return function (a) {
                        return function (b) {
                            var get$prime = get(dictIsSymbol)(dictRowCons)(Data_Symbol.SProxy.value);
                            var equalRest = equalFields(dictEqualFields)(Type_Row.RLProxy.value);
                            return Data_Eq.eq(dictEq)(get$prime(a))(get$prime(b)) && equalRest(a)(b);
                        };
                    };
                });
            };
        };
    };
};
var equal = function (dictRowToList) {
    return function (dictEqualFields) {
        return function (a) {
            return function (b) {
                return equalFields(dictEqualFields)(Type_Row.RLProxy.value)(a)(b);
            };
        };
    };
};
var $$delete = function (dictIsSymbol) {
    return function (dictRowLacks) {
        return function (dictRowCons) {
            return function (l) {
                return function (r) {
                    return Data_Record_Unsafe.unsafeDeleteFn(Data_Symbol.reflectSymbol(dictIsSymbol)(l), r);
                };
            };
        };
    };
};
var rename = function (dictIsSymbol) {
    return function (dictIsSymbol1) {
        return function (dictRowCons) {
            return function (dictRowLacks) {
                return function (dictRowCons1) {
                    return function (dictRowLacks1) {
                        return function (prev) {
                            return function (next) {
                                return function (record) {
                                    return insert(dictIsSymbol1)(dictRowLacks1)(dictRowCons1)(next)(get(dictIsSymbol)(dictRowCons)(prev)(record))($$delete(dictIsSymbol)(dictRowLacks)(dictRowCons)(prev)(record));
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
module.exports = {
    get: get,
    set: set,
    modify: modify,
    insert: insert,
    "delete": $$delete,
    rename: rename,
    equal: equal,
    EqualFields: EqualFields,
    equalFields: equalFields,
    equalFieldsCons: equalFieldsCons,
    equalFieldsNil: equalFieldsNil
};
