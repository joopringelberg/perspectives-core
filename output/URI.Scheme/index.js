// Generated by purs version 0.11.7
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Identity = require("../Data.Identity");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_String_NonEmpty = require("../Data.String.NonEmpty");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Text_Parsing_Parser = require("../Text.Parsing.Parser");
var Text_Parsing_Parser_String = require("../Text.Parsing.Parser.String");
var URI_Common = require("../URI.Common");
var Scheme = function (x) {
    return x;
};
var toString = function (v) {
    return v;
};
var showScheme = new Data_Show.Show(function (v) {
    return "(Scheme.unsafeFromString " + (Data_Show.show(Data_Show.showString)(Data_String_NonEmpty.toString(v)) + ")");
});
var print = function (v) {
    return Data_String_NonEmpty.toString(v) + ":";
};
var parseScheme = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(URI_Common.alpha)(function (v) {
    return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Data_Array.many(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(URI_Common.alphaNum)(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("+")))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("-")))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("."))))(function (v1) {
        return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Data_String_NonEmpty.appendString(Data_String_NonEmpty.singleton(v))(Data_String.fromCharArray(v1)));
    });
});
var parser = Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Scheme)(parseScheme))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)(":"));
var ordScheme = Data_String_NonEmpty.ordNonEmptyString;
var fromString = function ($13) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(Scheme)(Data_Either.hush(Data_Function.flip(Text_Parsing_Parser.runParser)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(parseScheme)(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)))($13)));
};
var unsafeFromString = function (s) {
    var v = fromString(s);
    if (v instanceof Data_Maybe.Just) {
        return v.value0;
    };
    if (v instanceof Data_Maybe.Nothing) {
        return Partial_Unsafe.unsafeCrashWith("Scheme value is invalid: `" + (Data_Show.show(Data_Show.showString)(s) + "`"));
    };
    throw new Error("Failed pattern match at URI.Scheme line 61, column 22 - line 65, column 1: " + [ v.constructor.name ]);
};
var eqScheme = Data_String_NonEmpty.eqNonEmptyString;
module.exports = {
    fromString: fromString,
    toString: toString,
    unsafeFromString: unsafeFromString,
    parser: parser,
    print: print,
    eqScheme: eqScheme,
    ordScheme: ordScheme,
    showScheme: showScheme
};
