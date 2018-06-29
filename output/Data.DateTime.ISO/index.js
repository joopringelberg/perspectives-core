// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Argonaut_Decode = require("../Data.Argonaut.Decode");
var Data_Argonaut_Decode_Class = require("../Data.Argonaut.Decode.Class");
var Data_Argonaut_Encode = require("../Data.Argonaut.Encode");
var Data_Argonaut_Encode_Class = require("../Data.Argonaut.Encode.Class");
var Data_Array = require("../Data.Array");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Boolean = require("../Data.Boolean");
var Data_Bounded = require("../Data.Bounded");
var Data_Date = require("../Data.Date");
var Data_Date_Component = require("../Data.Date.Component");
var Data_DateTime = require("../Data.DateTime");
var Data_Either = require("../Data.Either");
var Data_Enum = require("../Data.Enum");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Identity = require("../Data.Identity");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_Time = require("../Data.Time");
var Data_Time_Component = require("../Data.Time.Component");
var Data_Traversable = require("../Data.Traversable");
var Prelude = require("../Prelude");
var Text_Parsing_Parser = require("../Text.Parsing.Parser");
var Text_Parsing_Parser_Combinators = require("../Text.Parsing.Parser.Combinators");
var Text_Parsing_Parser_String = require("../Text.Parsing.Parser.String");
var ISO = function (x) {
    return x;
};
var parseDigit = function (dictMonad) {
    return function (dictStringLike) {
        return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_Combinators.choice(Data_Foldable.foldableArray)(dictMonad)([ Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("0"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(0)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("1"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(1)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("2"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(2)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("3"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(3)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("4"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(4)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("5"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(5)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("6"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(6)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("7"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(7)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("8"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(8)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("9"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(9)) ]))("expected digit (0-9)");
    };
};
var padl$prime = function ($copy_n) {
    return function ($copy_chr) {
        return function ($copy_chrs) {
            var $tco_var_n = $copy_n;
            var $tco_var_chr = $copy_chr;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(n, chr, chrs) {
                if (n <= 0) {
                    $tco_done = true;
                    return chrs;
                };
                if (Data_Boolean.otherwise) {
                    $tco_var_n = n - 1 | 0;
                    $tco_var_chr = chr;
                    $copy_chrs = Data_Array.cons(chr)(chrs);
                    return;
                };
                throw new Error("Failed pattern match at Data.DateTime.ISO line 131, column 1 - line 131, column 49: " + [ n.constructor.name, chr.constructor.name, chrs.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_n, $tco_var_chr, $copy_chrs);
            };
            return $tco_result;
        };
    };
};
var padl = function (n) {
    return function (chr) {
        return function (str) {
            return Data_String.fromCharArray(padl$prime(n - Data_String.length(str) | 0)(chr)(Data_String.toCharArray(str)));
        };
    };
};
var showISO = new Data_Show.Show(function (v) {
    var showInt = function (dictBoundedEnum) {
        return function ($46) {
            return Data_Show.show(Data_Show.showInt)(Data_Enum.fromEnum(dictBoundedEnum)($46));
        };
    };
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(Data_Semigroup.append(Data_Semigroup.semigroupString))("")([ showInt(Data_Date_Component.boundedEnumYear)(Data_Date.year(v.value0)), "-", padl(2)("0")(showInt(Data_Date_Component.boundedEnumMonth)(Data_Date.month(v.value0))), "-", padl(2)("0")(showInt(Data_Date_Component.boundedEnumDay)(Data_Date.day(v.value0))), "T", padl(2)("0")(showInt(Data_Time_Component.boundedEnumHour)(Data_Time.hour(v.value1))), ":", padl(2)("0")(showInt(Data_Time_Component.boundedEnumMinute)(Data_Time.minute(v.value1))), ":", padl(2)("0")(showInt(Data_Time_Component.boundedEnumSecond)(Data_Time.second(v.value1))), ".", showInt(Data_Time_Component.boundedEnumMillisecond)(Data_Time.millisecond(v.value1)), "Z" ]);
});
var newtypeISO = new Data_Newtype.Newtype(function (n) {
    return n;
}, ISO);
var unwrapISO = Data_Newtype.unwrap(newtypeISO);
var maybeFail = function (dictMonad) {
    return function (str) {
        return Data_Maybe.maybe(Text_Parsing_Parser.fail(dictMonad)(str))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad)));
    };
};
var foldDigits = function (dictFoldable) {
    return Data_Foldable.foldl(dictFoldable)(function (acc) {
        return function (d) {
            return (acc * 10 | 0) + d | 0;
        };
    })(0);
};
var parseDigits = function (dictMonad) {
    return function (dictStringLike) {
        return function ($47) {
            return Data_Functor.map(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(foldDigits(Data_Foldable.foldableArray))(Data_Traversable.sequence(Data_Traversable.traversableArray)(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_Function.flip(Data_Array.replicate)(parseDigit(dictMonad)(dictStringLike))($47)));
        };
    };
};
var parseISODate = function (dictMonad) {
    return function (dictStringLike) {
        var dash = Text_Parsing_Parser_Combinators.optional(dictMonad)(Text_Parsing_Parser_Combinators["try"](dictMonad)(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("-")));
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(parseDigits(dictMonad)(dictStringLike)(4))(Data_Enum.toEnum(Data_Date_Component.boundedEnumYear)))(maybeFail(dictMonad)("bad year")))(function (v) {
            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(dash)(function (v1) {
                return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(parseDigits(dictMonad)(dictStringLike)(2))(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)))(maybeFail(dictMonad)("bad month")))(function (v2) {
                    return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(dash)(function (v3) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(parseDigits(dictMonad)(dictStringLike)(2))(Data_Enum.toEnum(Data_Date_Component.boundedEnumDay)))(maybeFail(dictMonad)("bad day")))(function (v4) {
                            return maybeFail(dictMonad)("bad date")(Data_Date.exactDate(v)(v2)(v4));
                        });
                    });
                });
            });
        });
    };
};
var parseISOTime = function (dictMonad) {
    return function (dictStringLike) {
        var truncate = function (n) {
            return Data_Maybe.fromMaybe(n)(Data_Int.fromString(Data_String.take(3)(Data_Show.show(Data_Show.showInt)(n) + "000")));
        };
        var colon = Text_Parsing_Parser_Combinators.optional(dictMonad)(Text_Parsing_Parser_Combinators["try"](dictMonad)(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)(":")));
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(parseDigits(dictMonad)(dictStringLike)(2))(Data_Enum.toEnum(Data_Time_Component.boundedEnumHour)))(maybeFail(dictMonad)("bad hour")))(function (v) {
            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(colon)(function (v1) {
                return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(parseDigits(dictMonad)(dictStringLike)(2))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMinute)))(maybeFail(dictMonad)("bad minute")))(function (v2) {
                    return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(colon)(function (v3) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(parseDigits(dictMonad)(dictStringLike)(2))(Data_Enum.toEnum(Data_Time_Component.boundedEnumSecond)))(maybeFail(dictMonad)("bad second")))(function (v4) {
                            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_Combinators.option(dictMonad)(Data_Bounded.bottom(Data_Time_Component.boundedMillisecond))(Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(((dictMonad.Bind1()).Apply0()).Functor0()))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(dictMonad))(Text_Parsing_Parser_Combinators["try"](dictMonad)(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)(".")))(Data_Array.some(Text_Parsing_Parser.alternativeParserT(dictMonad))(Text_Parsing_Parser.lazyParserT)(parseDigit(dictMonad)(dictStringLike))))(function ($48) {
                                return (function ($49) {
                                    return Data_Maybe.fromMaybe(Data_Bounded.top(Data_Time_Component.boundedMillisecond))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMillisecond)($49));
                                })(truncate(foldDigits(Data_Foldable.foldableArray)($48)));
                            })))(function (v5) {
                                return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(new Data_Time.Time(v, v2, v4, v5));
                            });
                        });
                    });
                });
            });
        });
    };
};
var parseISO = function (dictMonad) {
    return function (dictStringLike) {
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(parseISODate(dictMonad)(dictStringLike))(function (v) {
            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Text_Parsing_Parser_String["char"](dictStringLike)(dictMonad)("T"))(function (v1) {
                return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(parseISOTime(dictMonad)(dictStringLike))(function (v2) {
                    return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Data_Newtype.wrap(newtypeISO)(new Data_DateTime.DateTime(v, v2)));
                });
            });
        });
    };
};
var encodeJsonISO = new Data_Argonaut_Encode_Class.EncodeJson(function ($50) {
    return Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonJString)(Data_Show.show(showISO)($50));
});
var decodeJsonISO = new Data_Argonaut_Decode_Class.DecodeJson(Control_Bind.composeKleisli(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeJsonString))(function ($51) {
    return Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Text_Parsing_Parser.parseErrorMessage)(Data_Function.flip(Text_Parsing_Parser.runParser)(parseISO(Data_Identity.monadIdentity)(Text_Parsing_Parser_String.stringLikeString))($51));
}));
module.exports = {
    ISO: ISO,
    unwrapISO: unwrapISO,
    newtypeISO: newtypeISO,
    showISO: showISO,
    decodeJsonISO: decodeJsonISO,
    encodeJsonISO: encodeJsonISO
};
