// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Lazy = require("../Control.Lazy");
var Control_Monad_Gen = require("../Control.Monad.Gen");
var Control_Monad_Gen_Class = require("../Control.Monad.Gen.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Array = require("../Data.Array");
var Data_Boolean = require("../Data.Boolean");
var Data_Char = require("../Data.Char");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_StrMap = require("../Data.StrMap");
var Data_String = require("../Data.String");
var Data_Unfoldable = require("../Data.Unfoldable");
var Prelude = require("../Prelude");
var genJson = function (dictMonadGen) {
    return function (dictMonadRec) {
        return function (dictLazy) {
            var genJNumber = Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_Argonaut_Core.fromNumber)(Control_Monad_Gen_Class.chooseFloat(dictMonadGen)(-1000000.0)(1000000.0));
            var genJBoolean = Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_Argonaut_Core.fromBoolean)(Control_Monad_Gen_Class.chooseBool(dictMonadGen));
            var genJArray = Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_Argonaut_Core.fromArray)(Control_Monad_Gen.unfoldable(dictMonadRec)(dictMonadGen)(Data_Unfoldable.unfoldableArray)(Control_Lazy.defer(dictLazy)(function (v) {
                return genJson(dictMonadGen)(dictMonadRec)(dictLazy);
            })));
            var genChar = Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_Char.fromCharCode)(Control_Monad_Gen_Class.chooseInt(dictMonadGen)(0)(65535));
            var genString = Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_String.fromCharArray)(Control_Monad_Gen.unfoldable(dictMonadRec)(dictMonadGen)(Data_Unfoldable.unfoldableArray)(genChar));
            var genJString = Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_Argonaut_Core.fromString)(genString);
            var genLeaf = Control_Monad_Gen.oneOf(dictMonadGen)(Data_NonEmpty.foldable1NonEmpty(Data_Foldable.foldableArray))(new Data_NonEmpty.NonEmpty(Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(Data_Argonaut_Core.jsonNull), [ genJBoolean, genJNumber, genJString ]));
            var extendJObj = function (obj) {
                return function (k) {
                    return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(genJson(dictMonadGen)(dictMonadRec)(dictLazy))(function (v) {
                        return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(Data_Argonaut_Core.foldJsonObject(Data_Argonaut_Core.jsonSingletonObject(k)(v))(function ($8) {
                            return Data_Argonaut_Core.fromObject(Data_StrMap.insert(k)(v)($8));
                        })(obj));
                    });
                };
            };
            var genJObject = Control_Bind.bindFlipped((dictMonadGen.Monad0()).Bind1())(Data_Array.foldM(dictMonadGen.Monad0())(extendJObj)(Data_Argonaut_Core.jsonEmptyObject))(Control_Monad_Gen.unfoldable(dictMonadRec)(dictMonadGen)(Data_Unfoldable.unfoldableArray)(genString));
            var genJson$prime = function (size) {
                if (size > 1) {
                    return Control_Monad_Gen_Class.resize(dictMonadGen)(function (v) {
                        return v - 1 | 0;
                    })(Control_Monad_Gen.choose(dictMonadGen)(genJArray)(genJObject));
                };
                if (Data_Boolean.otherwise) {
                    return genLeaf;
                };
                throw new Error("Failed pattern match at Data.Argonaut.Gen line 20, column 3 - line 20, column 30: " + [ size.constructor.name ]);
            };
            return Control_Monad_Gen_Class.resize(dictMonadGen)(Data_Ord.min(Data_Ord.ordInt)(5))(Control_Monad_Gen_Class.sized(dictMonadGen)(genJson$prime));
        };
    };
};
module.exports = {
    genJson: genJson
};
