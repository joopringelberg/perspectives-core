// Generated by purs version 0.11.7
"use strict";
var $foreign = require("./foreign");
var Data_Array = require("../Data.Array");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Prelude = require("../Prelude");
var OG = function (x) {
    return x;
};
var objectsGetterCache = new Data_Tuple.Tuple([  ], [  ]);
var newtypeOG = new Data_Newtype.Newtype(function (n) {
    return n;
}, OG);
var lookupObjectsGetterByName = function (name) {
    var v = Data_Array.elemIndex(Data_Eq.eqString)(name)(Data_Tuple.snd(objectsGetterCache));
    if (v instanceof Data_Maybe.Nothing) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_Maybe.Just) {
        var v1 = Data_Array.index(Data_Tuple.fst(objectsGetterCache))(v.value0);
        if (v1 instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        if (v1 instanceof Data_Maybe.Just) {
            return Data_Maybe.Just.create(Data_Newtype.unwrap(newtypeOG)(v1.value0));
        };
        throw new Error("Failed pattern match at Perspectives.ObjectGetterLookup line 28, column 15 - line 30, column 34: " + [ v1.constructor.name ]);
    };
    throw new Error("Failed pattern match at Perspectives.ObjectGetterLookup line 26, column 34 - line 30, column 34: " + [ v.constructor.name ]);
};
var objectsGetterCacheInsert = function (name) {
    return function (getter) {
        var v = lookupObjectsGetterByName(name);
        if (v instanceof Data_Maybe.Nothing) {
            var ignore2 = $foreign.addToArray(getter)(Data_Tuple.fst(objectsGetterCache));
            var ignore1 = $foreign.addToArray(name)(Data_Tuple.snd(objectsGetterCache));
            return Data_Unit.unit;
        };
        return Data_Unit.unit;
    };
};
var eqOG = new Data_Eq.Eq($foreign.objectsGettersEqual);
var lookupObjectsGetterName = function (getter) {
    var v = Data_Array.elemIndex(eqOG)(getter)(Data_Tuple.fst(objectsGetterCache));
    if (v instanceof Data_Maybe.Nothing) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_Maybe.Just) {
        return Data_Array.index(Data_Tuple.snd(objectsGetterCache))(v.value0);
    };
    throw new Error("Failed pattern match at Perspectives.ObjectGetterLookup line 21, column 34 - line 23, column 47: " + [ v.constructor.name ]);
};
module.exports = {
    OG: OG,
    lookupObjectsGetterName: lookupObjectsGetterName,
    lookupObjectsGetterByName: lookupObjectsGetterByName,
    objectsGetterCacheInsert: objectsGetterCacheInsert,
    objectsGetterCache: objectsGetterCache,
    newtypeOG: newtypeOG,
    eqOG: eqOG,
    addToArray: $foreign.addToArray,
    objectsGettersEqual: $foreign.objectsGettersEqual
};
