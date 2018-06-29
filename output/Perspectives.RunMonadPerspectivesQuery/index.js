// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_State = require("../Control.Monad.State");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_StrMap = require("../Data.StrMap");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Perspectives_Effects = require("../Perspectives.Effects");
var Perspectives_EntiteitAndRDFAliases = require("../Perspectives.EntiteitAndRDFAliases");
var Perspectives_GlobalUnsafeStrMap = require("../Perspectives.GlobalUnsafeStrMap");
var Perspectives_TripleAdministration = require("../Perspectives.TripleAdministration");
var Prelude = require("../Prelude");
var runMonadPerspectivesQuery = function (a) {
    return function (f) {
        var tripleGetter = function (id) {
            return Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Perspectives_TripleAdministration.addToTripleIndex(id)("model:Perspectives$start")([ a ])([  ])([  ])(tripleGetter))));
        };
        var tref = {
            subject: a,
            predicate: "model:Perspectives$start"
        };
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Trans_Class.lift(Control_Monad_Reader_Trans.monadTransReaderT)(Control_Monad_Aff.monadAff)(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Perspectives_TripleAdministration.addToTripleIndex(a)("model:Perspectives$start")([ a ])([  ])([  ])(tripleGetter)))))(function (v) {
            return Control_Monad_State_Trans.evalStateT(Control_Monad_Reader_Trans.functorReaderT(Control_Monad_Aff.functorAff))(f(a))(Data_StrMap.singleton("#start")(tref));
        });
    };
};
var runTypedTripleGetter = function (v) {
    return function (a) {
        return runMonadPerspectivesQuery(a)(v.value1);
    };
};
var runQuery = Data_Function.flip(runTypedTripleGetter);
var runTypedTripleGetterToMaybeObject = function (id) {
    return function (ttg) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(runTypedTripleGetter(ttg)(id))(function ($17) {
            return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Array.head(Perspectives_CoreTypes.tripleObjects($17)));
        });
    };
};
var runTypedTripleGetterToObject = function (id) {
    return function (v) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(runTypedTripleGetter(v)(id))(function (v1) {
            var v2 = Data_Array.head(v1.object);
            if (v2 instanceof Data_Maybe.Nothing) {
                return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("TypedTripleGetter '" + (v.value0 + ("' returns no values for '" + (id + "'.")))));
            };
            if (v2 instanceof Data_Maybe.Just) {
                return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(v2.value0);
            };
            throw new Error("Failed pattern match at Perspectives.RunMonadPerspectivesQuery line 74, column 3 - line 76, column 25: " + [ v2.constructor.name ]);
        });
    };
};
var runTypedTripleGetterToObjects = function (id) {
    return function (ttg) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(runTypedTripleGetter(ttg)(id))(function ($18) {
            return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Perspectives_CoreTypes.tripleObjects($18));
        });
    };
};
module.exports = {
    runMonadPerspectivesQuery: runMonadPerspectivesQuery,
    runTypedTripleGetter: runTypedTripleGetter,
    runQuery: runQuery,
    runTypedTripleGetterToObjects: runTypedTripleGetterToObjects,
    runTypedTripleGetterToMaybeObject: runTypedTripleGetterToMaybeObject,
    runTypedTripleGetterToObject: runTypedTripleGetterToObject
};
