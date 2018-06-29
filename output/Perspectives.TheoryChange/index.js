"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Ordering = require("../Data.Ordering");
var Data_Traversable = require("../Data.Traversable");
var Partial_Unsafe = require("../Partial.Unsafe");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Perspectives_Effects = require("../Perspectives.Effects");
var Perspectives_EntiteitAndRDFAliases = require("../Perspectives.EntiteitAndRDFAliases");
var Perspectives_GlobalUnsafeStrMap = require("../Perspectives.GlobalUnsafeStrMap");
var Perspectives_RunMonadPerspectivesQuery = require("../Perspectives.RunMonadPerspectivesQuery");
var Perspectives_TripleAdministration = require("../Perspectives.TripleAdministration");
var Perspectives_TypesForDeltas = require("../Perspectives.TypesForDeltas");
var Prelude = require("../Prelude");
var updateDependencies = function (v) {
    return function (v1) {
        var remove = function (ref) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
                var v2 = Perspectives_TripleAdministration.getTriple(ref)();
                if (v2 instanceof Data_Maybe.Just) {
                    return Perspectives_TripleAdministration.removeDependency_(v2.value0)(Perspectives_TripleAdministration.getRef(v))();
                };
                if (v2 instanceof Data_Maybe.Nothing) {
                    return ref;
                };
                throw new Error("Failed pattern match at Perspectives.TheoryChange line 62, column 7 - line 64, column 28: " + [ v2.constructor.name ]);
            });
        };
        return Control_Monad_Eff.foreachE(Data_Array.difference(Perspectives_CoreTypes.eqTripleRef)(v.supports)(v1.supports))(remove);
    };
};

// Change the object of the triple to the array of IDs passed to the function.
var saveChangedObject = function (t) {
    return function (obj) {
        return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)($foreign.saveChangedObject_(t)(obj));
    };
};

// Destructively change the objects of an existing triple.
// Returns a Triple that can be used as a seed for delta propagation, i.e. (wrapped in an array) as argument to updateFromSeeds.
// NOTA BENE. This function is superceded by the functions in deltas.purs.
var setProperty = function (rid) {
    return function (pid) {
        return function (object) {
            return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Perspectives_TripleAdministration.lookupInTripleIndex(rid)(pid)))(function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(Data_Maybe.Nothing.value);
                };
                if (v instanceof Data_Maybe.Just) {
                    return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffAff)(Control_Bind.bind(Control_Monad_Aff.bindAff)(saveChangedObject(v.value0)(object))(function ($71) {
                        return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(Data_Maybe.Just.create($71));
                    }));
                };
                throw new Error("Failed pattern match at Perspectives.TheoryChange line 106, column 5 - line 108, column 71: " + [ v.constructor.name ]);
            });
        };
    };
};
var recompute = function (v) {
    return Perspectives_RunMonadPerspectivesQuery.runMonadPerspectivesQuery(v.subject)(v.tripleGetter);
};
var pushIntoQueue = Data_Array.snoc;
var popFromQueue = Data_Array.uncons;
var modifyTriple = function (v) {
    return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(Data_Maybe.fromJust()(v.value)))(function (v1) {
        return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Perspectives_TripleAdministration.lookupInTripleIndex(v.id)(v.memberName)))(function (v2) {
            if (v2 instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(Data_Maybe.Nothing.value);
            };
            if (v2 instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Control_Monad_Aff.bindAff)((function () {
                    if (v.deltaType instanceof Perspectives_TypesForDeltas.Add) {
                        return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(Data_Array.cons(v1)(v2.value0.object));
                    };
                    if (v.deltaType instanceof Perspectives_TypesForDeltas.Remove) {
                        return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(Data_Array["delete"](Data_Eq.eqString)(v1)(v2.value0.object));
                    };
                    if (v.deltaType instanceof Perspectives_TypesForDeltas.Change) {
                        return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)([ v1 ]);
                    };
                    throw new Error("Failed pattern match at Perspectives.TheoryChange line 91, column 26 - line 94, column 34: " + [ v.deltaType.constructor.name ]);
                })())(function (v3) {
                    return Control_Bind.bind(Control_Monad_Aff.bindAff)(saveChangedObject(v2.value0)(v3))(function (v4) {
                        return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(new Data_Maybe.Just(v4));
                    });
                });
            };
            throw new Error("Failed pattern match at Perspectives.TheoryChange line 88, column 5 - line 96, column 34: " + [ v2.constructor.name ]);
        });
    });
};
var getDependencies = function (v) {
    var lookupRef = function (v1) {
        return Perspectives_TripleAdministration.lookupInTripleIndex(v1.subject)(v1.predicate);
    };
    return function __do() {
        var v1 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Control_Monad_Eff.applicativeEff)(lookupRef)(v.dependencies)();
        return Data_Foldable.foldr(Data_Foldable.foldableArray)(Data_Maybe.maybe(Control_Category.id(Control_Category.categoryFn))(Data_Array.cons))([  ])(v1);
    };
};

// | dependsOn t1 t2 returns GT if t1 depends on t2, that is, t1 is one of t2's dependencies.
// | In the graph, draw t1 above t2 with an arrow pointing from t1 downwards to t2. Hence, t1 is GT than t2.
// | (dependsOn is the inverse of hasDependency, in other words, dependencies)
var dependsOn = function (v) {
    return function (v1) {
        var v2 = Data_Array.elemIndex(Perspectives_CoreTypes.eqTripleRef)({
            subject: v.subject,
            predicate: v.predicate
        })(v1.dependencies);
        if (v2 instanceof Data_Maybe.Nothing) {
            return Data_Ordering.EQ.value;
        };
        return Data_Ordering.GT.value;
    };
};
var addToQueue = function (q) {
    return function (triples) {
        return Data_Array.union(Perspectives_CoreTypes.eqTriple)(q)(Data_Array.sortBy(dependsOn)(Data_Array.difference(Perspectives_CoreTypes.eqTriple)(triples)(q)));
    };
};
var propagateTheoryDeltas = function (q) {
    var v = popFromQueue(q);
    if (v instanceof Data_Maybe.Nothing) {
        return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))([  ]);
    };
    if (v instanceof Data_Maybe.Just) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(recompute(v.value0.head))(function (v1) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(saveChangedObject(v.value0.head)(v1.object)))(function (v2) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(updateDependencies(v.value0.head)(v1))))(function (v3) {
                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Perspectives_TripleAdministration.setSupports_(v.value0.head)(v1.supports))))(function (v4) {
                        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Reader_Trans.monadEffReader(Control_Monad_Aff.monadEffAff))(getDependencies(v.value0.head)))(function (v5) {
                            return propagateTheoryDeltas(addToQueue(v.value0.tail)(v5));
                        });
                    });
                });
            });
        });
    };
    throw new Error("Failed pattern match at Perspectives.TheoryChange line 46, column 27 - line 54, column 49: " + [ v.constructor.name ]);
};
var updateFromSeeds = function (ts) {
    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Reader_Trans.monadEffReader(Control_Monad_Aff.monadEffAff))(Data_Traversable.traverse(Data_Traversable.traversableArray)(Control_Monad_Eff.applicativeEff)(getDependencies)(ts)))(function (v) {
        return propagateTheoryDeltas(Control_Bind.join(Control_Bind.bindArray)(v));
    });
};
module.exports = {
    updateFromSeeds: updateFromSeeds,
    modifyTriple: modifyTriple
};
