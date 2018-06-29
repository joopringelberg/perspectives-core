// Generated by purs version 0.11.7
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array_NonEmpty = require("../Data.Array.NonEmpty");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Identity = require("../Data.Identity");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String_NonEmpty = require("../Data.String.NonEmpty");
var Prelude = require("../Prelude");
var Text_Parsing_Parser = require("../Text.Parsing.Parser");
var URI_Common = require("../URI.Common");
var RegName = function (x) {
    return x;
};
var unsafeToString = function (v) {
    return v;
};
var unsafeFromString = RegName;
var toString = function (v) {
    return URI_Common["decodeURIComponent'"](v);
};
var showRegName = new Data_Show.Show(function (v) {
    return "(RegName.unsafeFromString " + (Data_Show.show(Data_String_NonEmpty.showNonEmptyString)(v) + ")");
});
var semigroupRegName = Data_String_NonEmpty.semigroupNonEmptyString;
var regNameChar = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(URI_Common.unreserved)(URI_Common.subDelims);
var print = function ($6) {
    return Data_String_NonEmpty.toString(unsafeToString($6));
};
var parser = (function () {
    var p = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(URI_Common.pctEncoded)(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_String_NonEmpty.singleton)(regNameChar));
    return Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(function ($7) {
        return RegName(Data_String_NonEmpty.join1With(Data_Array_NonEmpty.foldable1NonEmptyArray)("")($7));
    })(Data_Array_NonEmpty.some(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(p));
})();
var ordRegName = Data_String_NonEmpty.ordNonEmptyString;
var fromString = function ($8) {
    return RegName(URI_Common["printEncoded'"](regNameChar)($8));
};
var eqRegName = Data_String_NonEmpty.eqNonEmptyString;
module.exports = {
    fromString: fromString,
    toString: toString,
    unsafeFromString: unsafeFromString,
    unsafeToString: unsafeToString,
    parser: parser,
    print: print,
    regNameChar: regNameChar,
    eqRegName: eqRegName,
    ordRegName: ordRegName,
    semigroupRegName: semigroupRegName,
    showRegName: showRegName
};
