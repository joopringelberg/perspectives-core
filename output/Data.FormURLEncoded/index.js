// Generated by purs version 0.11.7
"use strict";
var Control_Apply = require("../Control.Apply");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Global = require("../Global");
var Prelude = require("../Prelude");
var FormURLEncoded = function (x) {
    return x;
};
var toArray = function (v) {
    return v;
};
var showFormUrlEncoded = Data_Show.showArray(Data_Tuple.showTuple(Data_Show.showString)(Data_Maybe.showMaybe(Data_Show.showString)));
var semigroupFormUrlEncoded = Data_Semigroup.semigroupArray;
var ordFormUrlEncoded = Data_Ord.ordArray(Data_Tuple.ordTuple(Data_Ord.ordString)(Data_Maybe.ordMaybe(Data_Ord.ordString)));
var newtypeFormUrlEncoded = new Data_Newtype.Newtype(function (n) {
    return n;
}, FormURLEncoded);
var monoidFormUrlEncoded = Data_Monoid.monoidArray;
var genericFormUrlEncoded = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.FormURLEncoded.FormURLEncoded" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(FormURLEncoded))(Data_Generic.fromSpine(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericString)(Data_Generic.genericMaybe(Data_Generic.genericString))))(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.FormURLEncoded.FormURLEncoded", [ {
        sigConstructor: "Data.FormURLEncoded.FormURLEncoded",
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericString)(Data_Generic.genericMaybe(Data_Generic.genericString))))(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.FormURLEncoded.FormURLEncoded", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericString)(Data_Generic.genericMaybe(Data_Generic.genericString))))(v);
    } ]);
});
var fromArray = FormURLEncoded;
var eqFormUrlEncoded = Data_Eq.eqArray(Data_Tuple.eqTuple(Data_Eq.eqString)(Data_Maybe.eqMaybe(Data_Eq.eqString)));
var encode = (function () {
    var encodePart = function (v) {
        if (v.value1 instanceof Data_Maybe.Nothing) {
            return Global["encodeURIComponent"](v.value0);
        };
        if (v.value1 instanceof Data_Maybe.Just) {
            return Global["encodeURIComponent"](v.value0) + ("=" + Global["encodeURIComponent"](v.value1.value0));
        };
        throw new Error("Failed pattern match at Data.FormURLEncoded line 43, column 5 - line 43, column 56: " + [ v.constructor.name ]);
    };
    return function ($21) {
        return Data_String.joinWith("&")(Data_Functor.map(Data_Functor.functorArray)(encodePart)(toArray($21)));
    };
})();
module.exports = {
    FormURLEncoded: FormURLEncoded,
    fromArray: fromArray,
    toArray: toArray,
    encode: encode,
    genericFormUrlEncoded: genericFormUrlEncoded,
    newtypeFormUrlEncoded: newtypeFormUrlEncoded,
    eqFormUrlEncoded: eqFormUrlEncoded,
    ordFormUrlEncoded: ordFormUrlEncoded,
    showFormUrlEncoded: showFormUrlEncoded,
    semigroupFormUrlEncoded: semigroupFormUrlEncoded,
    monoidFormUrlEncoded: monoidFormUrlEncoded
};
