// Generated by purs version 0.11.7
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show");
var Data_Identity = require("../Data.Identity");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_String_NonEmpty = require("../Data.String.NonEmpty");
var Data_Symbol = require("../Data.Symbol");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Global = require("../Global");
var Prelude = require("../Prelude");
var Text_Parsing_Parser = require("../Text.Parsing.Parser");
var Text_Parsing_Parser_Combinators = require("../Text.Parsing.Parser.Combinators");
var Text_Parsing_Parser_String = require("../Text.Parsing.Parser.String");
var URI_Common = require("../URI.Common");
var URI_Query = require("../URI.Query");
var Value = function (x) {
    return x;
};
var QueryPairs = function (x) {
    return x;
};
var Key = function (x) {
    return x;
};
var valueToString = function (v) {
    return Global["decodeURIComponent"](v);
};
var unsafeValueToString = function (v) {
    return v;
};
var unsafeValueFromString = Value;
var unsafeKeyToString = function (v) {
    return v;
};
var unsafeKeyFromString = Key;
var showValue = new Data_Show.Show(function (v) {
    return "(QueryPairs.unsafeValueFromString " + (Data_Show.show(Data_Show.showString)(v) + ")");
});
var showKey = new Data_Show.Show(function (v) {
    return "(QueryPairs.unsafeKeyFromString " + (Data_Show.show(Data_Show.showString)(v) + ")");
});
var semigroupValue = Data_Semigroup.semigroupString;
var semigroupQueryPairs = Data_Semigroup.semigroupArray;
var semigroupKey = Data_Semigroup.semigroupString;
var print = function (printK) {
    return function (printV) {
        return function (v) {
            var printPart = function (v1) {
                if (v1.value1 instanceof Data_Maybe.Nothing) {
                    return unsafeKeyToString(printK(v1.value0));
                };
                if (v1.value1 instanceof Data_Maybe.Just) {
                    return unsafeKeyToString(printK(v1.value0)) + ("=" + unsafeValueToString(printV(v1.value1.value0)));
                };
                throw new Error("Failed pattern match at URI.Extra.QueryPairs line 101, column 17 - line 105, column 78: " + [ v1.constructor.name ]);
            };
            return URI_Query.unsafeFromString(Data_String.joinWith("&")(Data_Array.fromFoldable(Data_Foldable.foldableArray)(Data_Functor.map(Data_Functor.functorArray)(printPart)(v))));
        };
    };
};
var ordValue = Data_Ord.ordString;
var ordQueryPairs = function (dictOrd) {
    return function (dictOrd1) {
        return Data_Ord.ordArray(Data_Tuple.ordTuple(dictOrd)(Data_Maybe.ordMaybe(dictOrd1)));
    };
};
var ordKey = Data_Ord.ordString;
var monoidValue = Data_Monoid.monoidString;
var monoidQueryPairs = Data_Monoid.monoidArray;
var monoidKey = Data_Monoid.monoidString;
var keyToString = function (v) {
    return Global["decodeURIComponent"](v);
};
var keyPartChar = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(URI_Common.unreserved)(Text_Parsing_Parser_String.oneOf(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)([ "!", "$", "'", "(", ")", "*", "+", ",", ":", "@", "/", "?" ]));
var valueFromString = function ($42) {
    return Value(URI_Common.printEncoded(keyPartChar)($42));
};
var valuePartChar = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(keyPartChar)(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("="));
var parsePart = function (parseK) {
    return function (parseV) {
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(URI_Common.wrapParser(Data_Identity.monadIdentity)(function ($43) {
            return parseK(Key($43));
        })(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_String_NonEmpty.joinWith(Data_Foldable.foldableArray)(""))(Data_Array.some(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_String_NonEmpty.singleton)(keyPartChar))(URI_Common.pctEncoded)))))(function (v) {
            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(URI_Common.wrapParser(Data_Identity.monadIdentity)(Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Data_Either.applicativeEither)(function ($44) {
                return parseV(Value($44));
            }))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("="))(function (v1) {
                return Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_String_NonEmpty.joinWith(Data_Foldable.foldableArray)(""))(Data_Array.many(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_String_NonEmpty.singleton)(valuePartChar))(URI_Common.pctEncoded)));
            }))))(function (v1) {
                return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(new Data_Tuple.Tuple(v, v1));
            });
        });
    };
};
var parse = function (parseK) {
    return function (parseV) {
        return function ($45) {
            return Data_Bifunctor.bimap(Data_Either.bifunctorEither)(function (v) {
                return v.value0;
            })(QueryPairs)(Data_Function.flip(Text_Parsing_Parser.runParser)(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_Array.fromFoldable(Data_List_Types.foldableList))(Text_Parsing_Parser_Combinators.sepBy(Data_Identity.monadIdentity)(parsePart(parseK)(parseV))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("&"))))(URI_Query.unsafeToString($45)));
        };
    };
};
var keyFromString = function ($46) {
    return Key(URI_Common.printEncoded(keyPartChar)($46));
};
var genericQueryPairs = new Data_Generic_Rep.Generic(function (x) {
    return x;
}, function (x) {
    return x;
});
var showQueryPairs = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericQueryPairs)(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Show.showArray(Data_Tuple.showTuple(dictShow)(Data_Maybe.showMaybe(dictShow1)))))(new Data_Symbol.IsSymbol(function () {
            return "QueryPairs";
        }))));
    };
};
var eqValue = Data_Eq.eqString;
var eqQueryPairs = function (dictEq) {
    return function (dictEq1) {
        return Data_Eq.eqArray(Data_Tuple.eqTuple(dictEq)(Data_Maybe.eqMaybe(dictEq1)));
    };
};
var eqKey = Data_Eq.eqString;
module.exports = {
    QueryPairs: QueryPairs,
    parse: parse,
    print: print,
    keyPartChar: keyPartChar,
    valuePartChar: valuePartChar,
    keyFromString: keyFromString,
    keyToString: keyToString,
    unsafeKeyFromString: unsafeKeyFromString,
    unsafeKeyToString: unsafeKeyToString,
    valueFromString: valueFromString,
    valueToString: valueToString,
    unsafeValueFromString: unsafeValueFromString,
    unsafeValueToString: unsafeValueToString,
    eqQueryPairs: eqQueryPairs,
    ordQueryPairs: ordQueryPairs,
    genericQueryPairs: genericQueryPairs,
    showQueryPairs: showQueryPairs,
    semigroupQueryPairs: semigroupQueryPairs,
    monoidQueryPairs: monoidQueryPairs,
    eqKey: eqKey,
    ordKey: ordKey,
    semigroupKey: semigroupKey,
    monoidKey: monoidKey,
    showKey: showKey,
    eqValue: eqValue,
    ordValue: ordValue,
    semigroupValue: semigroupValue,
    monoidValue: monoidValue,
    showValue: showValue
};
