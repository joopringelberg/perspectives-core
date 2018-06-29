// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_State = require("../Control.Monad.State");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Newtype = require("../Data.Newtype");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Perspectives_DataTypeObjectGetters = require("../Perspectives.DataTypeObjectGetters");
var Perspectives_Effects = require("../Perspectives.Effects");
var Perspectives_EntiteitAndRDFAliases = require("../Perspectives.EntiteitAndRDFAliases");
var Perspectives_Identifiers = require("../Perspectives.Identifiers");
var Perspectives_ObjectGetterConstructors = require("../Perspectives.ObjectGetterConstructors");
var Perspectives_ObjectsGetterComposition = require("../Perspectives.ObjectsGetterComposition");
var Perspectives_TripleAdministration = require("../Perspectives.TripleAdministration");
var Prelude = require("../Prelude");
var constructTripleGetterWithArbitrarySupport = function (pn) {
    return function (objectsGetter) {
        return function (v) {
            var tripleGetter = function (id) {
                return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Perspectives_TripleAdministration.memorizeQueryResults)(function (v1) {
                    if (v1) {
                        return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Perspectives_TripleAdministration.lookupInTripleIndex(id)(pn)))))(function (v2) {
                            if (v2 instanceof Data_Maybe.Nothing) {
                                return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(objectsGetter(id))(function (v3) {
                                    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(v.value1(id))(function (v4) {
                                        return Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Perspectives_TripleAdministration.addToTripleIndex(id)(pn)(v3)([  ])([ {
                                            subject: v4.subject,
                                            predicate: v4.predicate
                                        } ])(tripleGetter))));
                                    });
                                });
                            };
                            if (v2 instanceof Data_Maybe.Just) {
                                return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(v2.value0);
                            };
                            throw new Error("Failed pattern match at Perspectives.TripleGetterConstructors line 59, column 9 - line 64, column 29: " + [ v2.constructor.name ]);
                        });
                    };
                    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(objectsGetter(id))(function (v2) {
                        return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))({
                            subject: id,
                            predicate: pn,
                            object: v2,
                            dependencies: [  ],
                            supports: [  ],
                            tripleGetter: tripleGetter
                        });
                    });
                });
            };
            return new Perspectives_CoreTypes.TypedTripleGetter(pn, tripleGetter);
        };
    };
};
var constructTripleGetterFromEffectExpression = function (pn) {
    return function (objectsGetter) {
        var tripleGetter = function (id) {
            return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Perspectives_TripleAdministration.memorizeQueryResults)(function (v) {
                if (v) {
                    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Perspectives_TripleAdministration.lookupInTripleIndex(id)(pn)))))(function (v1) {
                        if (v1 instanceof Data_Maybe.Nothing) {
                            return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(objectsGetter(id))(function (v2) {
                                return Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Perspectives_TripleAdministration.addToTripleIndex(id)(pn)(v2)([  ])([  ])(tripleGetter))));
                            });
                        };
                        if (v1 instanceof Data_Maybe.Just) {
                            return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(v1.value0);
                        };
                        throw new Error("Failed pattern match at Perspectives.TripleGetterConstructors line 32, column 9 - line 36, column 29: " + [ v1.constructor.name ]);
                    });
                };
                return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(objectsGetter(id))(function (v1) {
                    return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))({
                        subject: id,
                        predicate: pn,
                        object: v1,
                        dependencies: [  ],
                        supports: [  ],
                        tripleGetter: tripleGetter
                    });
                });
            });
        };
        return new Perspectives_CoreTypes.TypedTripleGetter(pn, tripleGetter);
    };
};
var constructTripleGetterFromObjectsGetter = function (pn) {
    return function (objGetter) {
        return constructTripleGetterFromEffectExpression(pn)(function ($34) {
            return Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(objGetter($34));
        });
    };
};
var rolHasType = function (typeId) {
    return constructTripleGetterFromObjectsGetter("model:Perspectives$rolHasType" + ("_" + typeId))(Control_Bind.composeKleisli(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_DataTypeObjectGetters.rolType)(function (v) {
        return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Maybe.maybe([ "false" ])(Data_Function["const"]([ "true" ]))(Data_Array.elemIndex(Data_Eq.eqString)(typeId)(v)));
    }));
};
var rolHasTypeWithLocalName = function (localName) {
    var f = Data_Newtype.alaF(Data_Functor.functorFn)(Data_Functor.functorFn)(Data_Monoid_Disj.newtypeDisj)(Data_Monoid_Disj.newtypeDisj)(Data_Monoid_Disj.Disj)(Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Monoid_Disj.monoidDisj(Data_HeytingAlgebra.heytingAlgebraBoolean)))(function ($35) {
        return Data_Maybe.maybe(false)(Data_Eq.eq(Data_Eq.eqString)(localName))(Perspectives_Identifiers.deconstructLocalNameFromDomeinURI($35));
    });
    return constructTripleGetterFromObjectsGetter("model:Perspectives$rolHasTypeWithLocalName" + ("_" + localName))(Perspectives_ObjectsGetterComposition.composeMonoidal(Data_Show.showBoolean)(Perspectives_DataTypeObjectGetters.rolType)(f));
};
var constructTripleGetter = function (objectsGetterConstructor) {
    return function (pn) {
        return constructTripleGetterFromObjectsGetter(pn)(objectsGetterConstructor(pn));
    };
};
var constructRolPropertyLookup = function (ln) {
    return constructTripleGetter(Perspectives_ObjectGetterConstructors.getPropertyFromRolTelescope)(ln);
};
var constructRolPropertyGetter = function (pn) {
    return constructTripleGetter(Perspectives_ObjectGetterConstructors.getProperty)(pn);
};
var constructRolLookup = function (rn) {
    return constructTripleGetter(Perspectives_ObjectGetterConstructors.getRolFromPrototypeHierarchy)(rn);
};
var constructRolGetter = function (rn) {
    return constructTripleGetter(Perspectives_ObjectGetterConstructors.getRol)(rn);
};
var constructInverseRolGetter = function (pn) {
    return constructTripleGetterFromObjectsGetter(pn + "_inverse")(Perspectives_ObjectGetterConstructors.getGebondenAls(pn));
};
var constructInternalPropertyLookup = function (ln) {
    return constructTripleGetter(Perspectives_ObjectGetterConstructors.lookupInternalProperty)(ln);
};
var constructInternalPropertyGetter = function (pn) {
    return constructTripleGetter(Perspectives_ObjectGetterConstructors.getInternalProperty)(pn);
};
var constructExternalPropertyLookup = function (ln) {
    return constructTripleGetter(Perspectives_ObjectGetterConstructors.lookupExternalProperty)(ln);
};
var constructExternalPropertyGetter = function (pn) {
    return constructTripleGetter(Perspectives_ObjectGetterConstructors.getExternalProperty)(pn);
};
module.exports = {
    constructTripleGetterFromEffectExpression: constructTripleGetterFromEffectExpression,
    constructTripleGetterWithArbitrarySupport: constructTripleGetterWithArbitrarySupport,
    constructTripleGetterFromObjectsGetter: constructTripleGetterFromObjectsGetter,
    constructTripleGetter: constructTripleGetter,
    constructExternalPropertyGetter: constructExternalPropertyGetter,
    constructExternalPropertyLookup: constructExternalPropertyLookup,
    constructInternalPropertyGetter: constructInternalPropertyGetter,
    constructInternalPropertyLookup: constructInternalPropertyLookup,
    constructRolPropertyGetter: constructRolPropertyGetter,
    constructRolPropertyLookup: constructRolPropertyLookup,
    constructRolGetter: constructRolGetter,
    constructRolLookup: constructRolLookup,
    constructInverseRolGetter: constructInverseRolGetter,
    rolHasType: rolHasType,
    rolHasTypeWithLocalName: rolHasTypeWithLocalName
};
