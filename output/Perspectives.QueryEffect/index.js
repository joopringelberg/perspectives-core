// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Data_Function = require("../Data.Function");
var Data_Semigroup = require("../Data.Semigroup");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Perspectives_Effects = require("../Perspectives.Effects");
var Perspectives_TripleAdministration = require("../Perspectives.TripleAdministration");
var Prelude = require("../Prelude");
var pushesObjectsTo = function (v) {
    return function (v1) {
        var name = "(" + (v.value0 + (" ~> " + (v1.value0 + ")")));
        var effectFun = function (v2) {
            return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_Eff_Class.liftEff(Control_Monad_State_Trans.monadEffState(Control_Monad_Reader_Trans.monadEffReader(Control_Monad_Aff.monadEffAff)))(v1.value1(v2.object)))(function (v3) {
                return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))({
                    subject: v2.subject,
                    predicate: name,
                    object: v2.object,
                    dependencies: [  ],
                    supports: [ Perspectives_TripleAdministration.getRef(v2) ],
                    tripleGetter: Data_Function["const"](effectFun(v2))
                });
            });
        };
        var pushesObjectsTo$prime = function (id) {
            return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(v.value1(id))(function (v2) {
                return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(effectFun(v2))(function (v3) {
                    return Control_Monad_Eff_Class.liftEff(Control_Monad_State_Trans.monadEffState(Control_Monad_Reader_Trans.monadEffReader(Control_Monad_Aff.monadEffAff)))(Perspectives_TripleAdministration.registerTriple(v3));
                });
            });
        };
        return new Perspectives_CoreTypes.TypedTripleGetter(v1.value0, pushesObjectsTo$prime);
    };
};
module.exports = {
    pushesObjectsTo: pushesObjectsTo
};
