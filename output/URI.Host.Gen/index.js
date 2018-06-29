// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Gen = require("../Control.Monad.Gen");
var Control_Monad_Gen_Class = require("../Control.Monad.Gen.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Data_Char_Gen = require("../Data.Char.Gen");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_String_Gen = require("../Data.String.Gen");
var Data_String_NonEmpty = require("../Data.String.NonEmpty");
var Prelude = require("../Prelude");
var URI_Host = require("../URI.Host");
var URI_Host_IPv4Address = require("../URI.Host.IPv4Address");
var URI_Host_RegName = require("../URI.Host.RegName");
var genRegName = function (dictMonadGen) {
    return function (dictMonadRec) {
        var genAlphaNumeric = Control_Monad_Gen.choose(dictMonadGen)(Data_Char_Gen.genAlpha(dictMonadGen))(Data_Char_Gen.genDigitChar(dictMonadGen));
        return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genAlphaNumeric)(function (v) {
            return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Data_String_Gen.genString(dictMonadRec)(dictMonadGen)(genAlphaNumeric))(function (v1) {
                return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(URI_Host_RegName.fromString(Data_String_NonEmpty.cons(v)(v1)));
            });
        });
    };
};
var genIPv4 = function (dictMonadGen) {
    return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Control_Monad_Gen_Class.chooseInt(dictMonadGen)(0)(255))(function (v) {
        return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Control_Monad_Gen_Class.chooseInt(dictMonadGen)(0)(255))(function (v1) {
            return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Control_Monad_Gen_Class.chooseInt(dictMonadGen)(0)(255))(function (v2) {
                return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Control_Monad_Gen_Class.chooseInt(dictMonadGen)(0)(255))(function (v3) {
                    return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(URI_Host_IPv4Address.unsafeFromInts(v)(v1)(v2)(v3));
                });
            });
        });
    });
};
var genHost = function (dictMonadGen) {
    return function (dictMonadRec) {
        return Control_Monad_Gen.choose(dictMonadGen)(Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(URI_Host.NameAddress.create)(genRegName(dictMonadGen)(dictMonadRec)))(Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(URI_Host.IPv4Address.create)(genIPv4(dictMonadGen)));
    };
};
module.exports = {
    genIPv4: genIPv4,
    genRegName: genRegName,
    genHost: genHost
};
