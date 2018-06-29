"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Now = require("../Control.Monad.Eff.Now");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_DateTime = require("../Data.DateTime");
var Data_DateTime_Instant = require("../Data.DateTime.Instant");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Foreign = require("../Data.Foreign");
var Data_Foreign_Class = require("../Data.Foreign.Class");
var Data_Foreign_Generic = require("../Data.Foreign.Generic");
var Data_Foreign_Generic_Class = require("../Data.Foreign.Generic.Class");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_JSDate = require("../Data.JSDate");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_StrMap = require("../Data.StrMap");
var Data_Symbol = require("../Data.Symbol");
var Data_Traversable = require("../Data.Traversable");
var Data_TraversableWithIndex = require("../Data.TraversableWithIndex");
var Data_Unit = require("../Data.Unit");
var Network_HTTP_Affjax = require("../Network.HTTP.Affjax");
var Network_HTTP_Affjax_Request = require("../Network.HTTP.Affjax.Request");
var Network_HTTP_Affjax_Response = require("../Network.HTTP.Affjax.Response");
var Network_HTTP_StatusCode = require("../Network.HTTP.StatusCode");
var Partial_Unsafe = require("../Partial.Unsafe");
var Perspectives_ContextAndRole = require("../Perspectives.ContextAndRole");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Perspectives_DataTypeObjectGetters = require("../Perspectives.DataTypeObjectGetters");
var Perspectives_DataTypeTripleGetters = require("../Perspectives.DataTypeTripleGetters");
var Perspectives_DomeinCache = require("../Perspectives.DomeinCache");
var Perspectives_Effects = require("../Perspectives.Effects");
var Perspectives_EntiteitAndRDFAliases = require("../Perspectives.EntiteitAndRDFAliases");
var Perspectives_Identifiers = require("../Perspectives.Identifiers");
var Perspectives_ModelBasedTripleGetters = require("../Perspectives.ModelBasedTripleGetters");
var Perspectives_PerspectEntiteit = require("../Perspectives.PerspectEntiteit");
var Perspectives_QueryCombinators = require("../Perspectives.QueryCombinators");
var Perspectives_Resource = require("../Perspectives.Resource");
var Perspectives_ResourceRetrieval = require("../Perspectives.ResourceRetrieval");
var Perspectives_RunMonadPerspectivesQuery = require("../Perspectives.RunMonadPerspectivesQuery");
var Perspectives_Syntax = require("../Perspectives.Syntax");
var Perspectives_TheoryChange = require("../Perspectives.TheoryChange");
var Perspectives_TripleGetterComposition = require("../Perspectives.TripleGetterComposition");
var Perspectives_TypesForDeltas = require("../Perspectives.TypesForDeltas");
var Perspectives_User = require("../Perspectives.User");
var Perspectives_Utilities = require("../Perspectives.Utilities");
var Prelude = require("../Prelude");

//---------------------------------------------------------
// DATETIME
// We need a newtype for DateTime in order to be able to serialize and show it.
//---------------------------------------------------------
var SerializableDateTime = function (x) {
    return x;
};

// instance showSerializableDateTime :: Show SerializableDateTime where
//   show (SerializableDateTime d) = runPure (catchException handleError (toISOString (fromDateTime d)))
//
// handleError :: forall eff. Error -> Eff eff String
// handleError e = pure "Could not serialize DateTime"
//---------------------------------------------------------
// TRANSACTIE
//---------------------------------------------------------
var Transactie = function (x) {
    return x;
};

// TODO. De verbinding tussen Actie en Rol is omgekeerd en is niet
// langer geregistreerd als een rol van de Actie, maar als rol van de Rol (objectRol en subjectRol).
// | The (IDs of the) users that play a role in, and have a relevant perspective on, the Context that is modified;
// | or the users that play a role in the context of the Role that is modified and have a relevant perspective on that Role.
var usersInvolvedInDelta = function (v) {
    
    // From the instance of the context, retrieve the instances of the users that play
    // a Rol in this context that have a subjectRol bound to an Actie that is bound as the
    // objectRol of the Rol of which memberName is an instance.
var usersInvolvedInContext = function (v1) {
        
        // All Rollen that are the subject of Acties that have the Rol as object.
        // `psp:Rol -> psp:Rol`
var actorsForObject = Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_ModelBasedTripleGetters.objectRollenDefM)(Perspectives_ModelBasedTripleGetters.inverse_subjectRollenDefM);
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_RunMonadPerspectivesQuery.runQuery(v1.id)(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_DataTypeTripleGetters.rolTypeM)(actorsForObject))(Perspectives_QueryCombinators.rolesOf(v1.id)))(Perspectives_ModelBasedTripleGetters.rolUserM)))(function (v2) {
            return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(v2.object);
        });
    };
    
    // Tests an Actie for having memberName in the view that is its objectView.
    // psp:Actie -> psp:Boolean
var hasRelevantView = function (id) {
        return Perspectives_QueryCombinators.contains(id)(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_ModelBasedTripleGetters.objectViewDefM)(Perspectives_ModelBasedTripleGetters.propertyReferentiesM))(Perspectives_ModelBasedTripleGetters.bindingDefM));
    };
    
    // From the instance of a Rol, retrieve the instances of users that play another Rol
    // in the same context, such that they have an Actie with an objectView that has the
    // changed property (as identified by memberName).
var usersInvolvedInRol = function (v1) {
        
        // acties that have an objectView with the memberName
        // psp:Rol -> psp:Actie
var relevantActies = Perspectives_QueryCombinators.filter(hasRelevantView(v1.memberName))(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_ModelBasedTripleGetters.rolInContextDefM)(Perspectives_ModelBasedTripleGetters.actiesInContextDefM));
        
        // roles in context that play the subjectRol in the relevant acties
        // psp:Rol -> psp:Rol
var subjectsOfRelevantActies = Perspectives_QueryCombinators.filter(Perspectives_QueryCombinators.notEmpty(Perspectives_QueryCombinators.intersect(Perspectives_ModelBasedTripleGetters.subjectRollenDefM)(relevantActies)))(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_ModelBasedTripleGetters.rolInContextDefM)(Perspectives_ModelBasedTripleGetters.ownRollenDefM));
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_CoreTypes.applyObjectsGetterToObject(v1.id)(Perspectives_DataTypeObjectGetters.context))(function (v2) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_RunMonadPerspectivesQuery.runQuery(v1.id)(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_TripleGetterComposition.composeTripleGetters(Perspectives_DataTypeTripleGetters.rolTypeM)(subjectsOfRelevantActies))(Perspectives_QueryCombinators.rolesOf(v2)))(Perspectives_ModelBasedTripleGetters.rolUserM)))(function (v3) {
                return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(v3.object);
            });
        });
    };
    if (v.isContext) {
        return usersInvolvedInContext(v);
    };
    return usersInvolvedInRol(v);
};
var updatePerspectEntiteitMember$prime = function (dictPerspectEntiteit) {
    return function (changeEntityMember) {
        return function (cid) {
            return function (memberName) {
                return function (value) {
                    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Perspectives_Resource.getPerspectEntiteit(dictPerspectEntiteit)(cid)))(function (v) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Functor["void"](Control_Monad_State_Trans.functorStateT(Control_Monad_Reader_Trans.functorReaderT(Control_Monad_Aff.functorAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Perspectives_PerspectEntiteit.cacheCachedEntiteit(dictPerspectEntiteit)(cid)(changeEntityMember(v)(memberName)(value)))))(function () {
                            return Data_Functor["void"](Control_Monad_State_Trans.functorStateT(Control_Monad_Reader_Trans.functorReaderT(Control_Monad_Aff.functorAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Perspectives_ResourceRetrieval.saveVersionedEntiteit(dictPerspectEntiteit)(cid)(v)));
                        });
                    });
                };
            };
        };
    };
};

/**
 * 
 * Bouw een transactie eerst op, splits hem dan in versies voor elke gebruiker.
 * Doorloop de verzameling deltas en bepaal per delta welke gebruikers betrokken zijn.
 * Bouw al doende een StrMap van userId en gespecialiseerde transacties op, waarbij je een transactie toevoegt voor een gebruiker die nog niet in de StrMap voorkomt.
 */
var transactieForEachUser = function (v) {
    var transactieCloneWithJustDelta = function (v1) {
        return function (d) {
            var $106 = {};
            for (var $107 in v1) {
                if ({}.hasOwnProperty.call(v1, $107)) {
                    $106[$107] = v1[$107];
                };
            };
            $106.deltas = [ d ];
            return $106;
        };
    };
    var transactieForEachUser$prime = function (d) {
        return function (users) {
            return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))))(function (v1) {
                return Data_Foldable.for_(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Foldable.foldableArray)(users)(function (user) {
                    var v2 = Data_StrMap.lookup(user)(v1);
                    if (v2 instanceof Data_Maybe.Nothing) {
                        return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_StrMap.insert(user)(transactieCloneWithJustDelta(v)(d))(v1));
                    };
                    if (v2 instanceof Data_Maybe.Just) {
                        return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_StrMap.insert(user)((function () {
                            var $111 = {};
                            for (var $112 in v2.value0) {
                                if ({}.hasOwnProperty.call(v2.value0, $112)) {
                                    $111[$112] = v2["value0"][$112];
                                };
                            };
                            $111.deltas = Data_Array.cons(d)(v2.value0.deltas);
                            return $111;
                        })())(v1));
                    };
                    throw new Error("Failed pattern match at Perspectives.Deltas line 304, column 19 - line 306, column 108: " + [ v2.constructor.name ]);
                });
            });
        };
    };
    return Control_Monad_State_Trans.execStateT(Control_Monad_Reader_Trans.functorReaderT(Control_Monad_Aff.functorAff))(Data_Foldable.for_(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Foldable.foldableArray)(v.deltas)(function (d) {
        return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(usersInvolvedInDelta(d)))(function (users) {
            return transactieForEachUser$prime(d)(users);
        });
    }))(Data_StrMap.empty);
};
var showSerializableDateTime = new Data_Show.Show(function (v) {
    return "todo";
});
var transactieID = function (v) {
    return v.author + ("_" + Data_Show.show(showSerializableDateTime)(v.timeStamp));
};
var onNothing$prime = function (dictMonadThrow) {
    return function ($269) {
        return Perspectives_Utilities["onNothing'"](dictMonadThrow)(Control_Monad_Eff_Exception.error($269));
    };
};
var onNothing = function (dictMonadThrow) {
    return function ($270) {
        return Perspectives_Utilities.onNothing(dictMonadThrow)(Control_Monad_Eff_Exception.error($270));
    };
};
var genericRepTransactie = new Data_Generic_Rep.Generic(function (x) {
    return new Data_Generic_Rep.Product(x.author, new Data_Generic_Rep.Product(x.changedDomeinFiles, new Data_Generic_Rep.Product(x.createdContexts, new Data_Generic_Rep.Product(x.createdRoles, new Data_Generic_Rep.Product(x.deletedContexts, new Data_Generic_Rep.Product(x.deletedRoles, new Data_Generic_Rep.Product(x.deltas, x.timeStamp)))))));
}, function (x) {
    return {
        author: x.value0,
        changedDomeinFiles: x.value1.value0,
        createdContexts: x.value1.value1.value0,
        createdRoles: x.value1.value1.value1.value0,
        deletedContexts: x.value1.value1.value1.value1.value0,
        deletedRoles: x.value1.value1.value1.value1.value1.value0,
        deltas: x.value1.value1.value1.value1.value1.value1.value0,
        timeStamp: x.value1.value1.value1.value1.value1.value1.value1
    };
});
var showTransactie = new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericRepTransactie)(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsRec(Data_Generic_Rep_Show.genericShowFieldsProduct(Data_Generic_Rep_Show.genericShowFieldsField(Data_Show.showString)(new Data_Symbol.IsSymbol(function () {
    return "author";
})))(Data_Generic_Rep_Show.genericShowFieldsProduct(Data_Generic_Rep_Show.genericShowFieldsField(Data_Show.showArray(Data_Show.showString))(new Data_Symbol.IsSymbol(function () {
    return "changedDomeinFiles";
})))(Data_Generic_Rep_Show.genericShowFieldsProduct(Data_Generic_Rep_Show.genericShowFieldsField(Data_Show.showArray(Perspectives_Syntax.showPerspectContext))(new Data_Symbol.IsSymbol(function () {
    return "createdContexts";
})))(Data_Generic_Rep_Show.genericShowFieldsProduct(Data_Generic_Rep_Show.genericShowFieldsField(Data_Show.showArray(Perspectives_Syntax.showPerspectRol))(new Data_Symbol.IsSymbol(function () {
    return "createdRoles";
})))(Data_Generic_Rep_Show.genericShowFieldsProduct(Data_Generic_Rep_Show.genericShowFieldsField(Data_Show.showArray(Data_Show.showString))(new Data_Symbol.IsSymbol(function () {
    return "deletedContexts";
})))(Data_Generic_Rep_Show.genericShowFieldsProduct(Data_Generic_Rep_Show.genericShowFieldsField(Data_Show.showArray(Data_Show.showString))(new Data_Symbol.IsSymbol(function () {
    return "deletedRoles";
})))(Data_Generic_Rep_Show.genericShowFieldsProduct(Data_Generic_Rep_Show.genericShowFieldsField(Data_Show.showArray(Perspectives_TypesForDeltas.showDelta))(new Data_Symbol.IsSymbol(function () {
    return "deltas";
})))(Data_Generic_Rep_Show.genericShowFieldsField(showSerializableDateTime)(new Data_Symbol.IsSymbol(function () {
    return "timeStamp";
})))))))))))(new Data_Symbol.IsSymbol(function () {
    return "Transactie";
}))));
var encodeSerializableDateTime = new Data_Foreign_Class.Encode(function (d) {
    return Data_Foreign.toForeign(Data_Show.show(showSerializableDateTime)(d));
});
var encodeTransactie = new Data_Foreign_Class.Encode(Perspectives_TypesForDeltas.encodeDefault(genericRepTransactie)(Data_Foreign_Generic_Class.genericEncodeConstructor(new Data_Symbol.IsSymbol(function () {
    return "Transactie";
}))(Data_Foreign_Generic_Class.genericEncodeArgsRec(Data_Foreign_Generic_Class.genericEncodeFieldsProduct(Data_Foreign_Generic_Class.genericEncodeFieldsField(new Data_Symbol.IsSymbol(function () {
    return "author";
}))(Data_Foreign_Class.stringEncode))(Data_Foreign_Generic_Class.genericEncodeFieldsProduct(Data_Foreign_Generic_Class.genericEncodeFieldsField(new Data_Symbol.IsSymbol(function () {
    return "changedDomeinFiles";
}))(Data_Foreign_Class.arrayEncode(Data_Foreign_Class.stringEncode)))(Data_Foreign_Generic_Class.genericEncodeFieldsProduct(Data_Foreign_Generic_Class.genericEncodeFieldsField(new Data_Symbol.IsSymbol(function () {
    return "createdContexts";
}))(Data_Foreign_Class.arrayEncode(Perspectives_Syntax.encodePerspectContext)))(Data_Foreign_Generic_Class.genericEncodeFieldsProduct(Data_Foreign_Generic_Class.genericEncodeFieldsField(new Data_Symbol.IsSymbol(function () {
    return "createdRoles";
}))(Data_Foreign_Class.arrayEncode(Perspectives_Syntax.encodePerspectRol)))(Data_Foreign_Generic_Class.genericEncodeFieldsProduct(Data_Foreign_Generic_Class.genericEncodeFieldsField(new Data_Symbol.IsSymbol(function () {
    return "deletedContexts";
}))(Data_Foreign_Class.arrayEncode(Data_Foreign_Class.stringEncode)))(Data_Foreign_Generic_Class.genericEncodeFieldsProduct(Data_Foreign_Generic_Class.genericEncodeFieldsField(new Data_Symbol.IsSymbol(function () {
    return "deletedRoles";
}))(Data_Foreign_Class.arrayEncode(Data_Foreign_Class.stringEncode)))(Data_Foreign_Generic_Class.genericEncodeFieldsProduct(Data_Foreign_Generic_Class.genericEncodeFieldsField(new Data_Symbol.IsSymbol(function () {
    return "deltas";
}))(Data_Foreign_Class.arrayEncode(Perspectives_TypesForDeltas.encodeDelta)))(Data_Foreign_Generic_Class.genericEncodeFieldsField(new Data_Symbol.IsSymbol(function () {
    return "timeStamp";
}))(encodeSerializableDateTime))))))))))));
var sendTransactieToUser = function (userId) {
    return function (t) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_RunMonadPerspectivesQuery.runTypedTripleGetterToMaybeObject(userId)(Perspectives_DataTypeTripleGetters.identityM))(function (v) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(onNothing$prime(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))("sendTransactieToUser: user has no IP: " + userId)(v))(function (v1) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Network_HTTP_Affjax.put(Network_HTTP_Affjax_Request.requestableString)(Network_HTTP_Affjax_Response.responsableString)(v1 + ("/" + (userId + ("_post/" + transactieID(t)))))(Data_Foreign_Generic.encodeJSON(encodeTransactie)(t))))(function (v2) {
                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(v2.status))(function (v3) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))((function () {
                            var v4 = v3 === 200 || v3 === 201;
                            if (v4) {
                                return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Unit.unit);
                            };
                            if (!v4) {
                                return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("sendTransactieToUser " + (transactieID(t) + (" fails: " + (Data_Show.show(Network_HTTP_StatusCode.showStatusCode)(v2.status) + ("(" + (Data_Show.show(Data_Show.showString)(v2.response) + ")")))))));
                            };
                            throw new Error("Failed pattern match at Perspectives.Deltas line 238, column 3 - line 240, column 146: " + [ v4.constructor.name ]);
                        })())(function () {
                            return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Unit.unit);
                        });
                    });
                });
            });
        });
    };
};
var distributeTransactie = function (t) {
    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(transactieForEachUser(t))(function (v) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Data_TraversableWithIndex.forWithIndex(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_StrMap.traversableWithIndexStrMap)(v)(sendTransactieToUser))(function (v1) {
            return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Unit.unit);
        });
    });
};
var deleteRolFromTransactie = function (v) {
    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))))(function (v1) {
        var v2 = Data_Array.findIndex(function (v3) {
            return v._id === v3._id;
        })(v1.createdRoles);
        if (v2 instanceof Data_Maybe.Nothing) {
            return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))((function () {
                var $156 = {};
                for (var $157 in v1) {
                    if ({}.hasOwnProperty.call(v1, $157)) {
                        $156[$157] = v1[$157];
                    };
                };
                $156.deletedRoles = Data_Array.cons(v._id)(v1.deletedRoles);
                return $156;
            })());
        };
        if (v2 instanceof Data_Maybe.Just) {
            return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))((function () {
                var $159 = {};
                for (var $160 in v1) {
                    if ({}.hasOwnProperty.call(v1, $160)) {
                        $159[$160] = v1[$160];
                    };
                };
                $159.createdRoles = Data_Maybe.fromJust()(Data_Array.deleteAt(v2.value0)(v1.createdRoles));
                return $159;
            })());
        };
        throw new Error("Failed pattern match at Perspectives.Deltas line 147, column 3 - line 149, column 103: " + [ v2.constructor.name ]);
    });
};
var deleteContextFromTransactie = function (v) {
    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))))(function (v1) {
        var v2 = Data_Array.findIndex(function (v3) {
            return v._id === v3._id;
        })(v1.createdContexts);
        if (v2 instanceof Data_Maybe.Nothing) {
            return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))((function () {
                var $171 = {};
                for (var $172 in v1) {
                    if ({}.hasOwnProperty.call(v1, $172)) {
                        $171[$172] = v1[$172];
                    };
                };
                $171.deletedContexts = Data_Array.cons(v._id)(v1.deletedContexts);
                return $171;
            })());
        };
        if (v2 instanceof Data_Maybe.Just) {
            return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))((function () {
                var $174 = {};
                for (var $175 in v1) {
                    if ({}.hasOwnProperty.call(v1, $175)) {
                        $174[$175] = v1[$175];
                    };
                };
                $174.createdContexts = Data_Maybe.fromJust()(Data_Array.deleteAt(v2.value0)(v1.createdContexts));
                return $174;
            })());
        };
        throw new Error("Failed pattern match at Perspectives.Deltas line 140, column 3 - line 142, column 109: " + [ v2.constructor.name ]);
    });
};
var createTransactie = function (author) {
    return function __do() {
        var v = Control_Monad_Eff_Now.now();
        return {
            author: author,
            timeStamp: Data_DateTime_Instant.toDateTime(v),
            deltas: [  ],
            createdContexts: [  ],
            createdRoles: [  ],
            deletedContexts: [  ],
            deletedRoles: [  ],
            changedDomeinFiles: [  ]
        };
    };
};

// TODO: kan het zo zijn dat er al een transactie loopt? En wat dan? Denk aan acties met effect.
var runInTransactie = function (m) {
    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_User.getUser)(function (v) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Reader_Trans.monadEffReader(Control_Monad_Aff.monadEffAff))(createTransactie(v)))(function (v1) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_State_Trans.execStateT(Control_Monad_Reader_Trans.functorReaderT(Control_Monad_Aff.functorAff))(m)(v1))(function (v2) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Data_Traversable.traverse(Data_Traversable.traversableArray)(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(function ($271) {
                    return Control_Monad_Trans_Class.lift(Control_Monad_Reader_Trans.monadTransReaderT)(Control_Monad_Aff.monadAff)(Perspectives_TheoryChange.modifyTriple($271));
                })(v2.deltas))(function (v3) {
                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Foldable.foldr(Data_Foldable.foldableArray)(function (mt) {
                        return function (a) {
                            return Data_Maybe.maybe(a)(Data_Function.flip(Data_Array.cons)(a))(mt);
                        };
                    })([  ])(v3)))(function (v4) {
                        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_TheoryChange.updateFromSeeds(v4))(function (v5) {
                            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Data_Foldable.for_(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Foldable.foldableArray)(v2.changedDomeinFiles)(Perspectives_DomeinCache.saveCachedDomeinFile))(function () {
                                return distributeTransactie(v2);
                            });
                        });
                    });
                });
            });
        });
    });
};
var addRolToTransactie = function (c) {
    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))))(function (v) {
        return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))((function () {
            var $190 = {};
            for (var $191 in v) {
                if ({}.hasOwnProperty.call(v, $191)) {
                    $190[$191] = v[$191];
                };
            };
            $190.createdRoles = Data_Array.cons(c)(v.createdRoles);
            return $190;
        })());
    });
};
var addDomeinFileToTransactie = function (dfId) {
    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))))(function (v) {
        var v1 = Data_Array.elemIndex(Data_Eq.eqString)(dfId)(v.changedDomeinFiles);
        if (v1 instanceof Data_Maybe.Nothing) {
            return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))((function () {
                var $196 = {};
                for (var $197 in v) {
                    if ({}.hasOwnProperty.call(v, $197)) {
                        $196[$197] = v[$197];
                    };
                };
                $196.changedDomeinFiles = Data_Array.cons(dfId)(v.changedDomeinFiles);
                return $196;
            })());
        };
        return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Unit.unit);
    });
};
var updatePerspectEntiteit$prime = function (dictPerspectEntiteit) {
    return function (changeEntity) {
        return function (cid) {
            return function (value) {
                return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Perspectives_Resource.getPerspectEntiteit(dictPerspectEntiteit)(cid)))(function (v) {
                    var $201 = Perspectives_Identifiers.isUserEntiteitID(cid);
                    if ($201) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Functor["void"](Control_Monad_State_Trans.functorStateT(Control_Monad_Reader_Trans.functorReaderT(Control_Monad_Aff.functorAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Perspectives_PerspectEntiteit.cacheCachedEntiteit(dictPerspectEntiteit)(cid)(changeEntity(value)(v)))))(function () {
                            return Data_Functor["void"](Control_Monad_State_Trans.functorStateT(Control_Monad_Reader_Trans.functorReaderT(Control_Monad_Aff.functorAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Perspectives_ResourceRetrieval.saveVersionedEntiteit(dictPerspectEntiteit)(cid)(v)));
                        });
                    };
                    var dfId = Data_Maybe.fromJust()(Perspectives_Identifiers.deconstructModelName(cid));
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(addDomeinFileToTransactie(dfId))(function () {
                        return Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Perspectives_PerspectEntiteit.cacheInDomeinFile(dictPerspectEntiteit)(dfId)(changeEntity(value)(v)));
                    });
                });
            };
        };
    };
};

/**
 * 
 * 1. Bepaal of de delta precies zo voorkomt in de lijst in de transactie. Zo ja, negeer dan de nieuwe delta.
 * 2. Bepaal of de rol functioneel is.
 * ZO JA:
 * 3. Zoek een delta waarvan de id, rolName en DeltaType gelijk zijn aan die van de nieuwe.
 * 	Indien gevonden: vervang die door de nieuwe.
 * 	Indien niet gevonden: zoek een delta waarvan id en rolName overeenkomen.
 * 		Indien gevonden, als geldt:
 * 			het ene DeltaType is Add en het andere Remove, verwijder dan de oude.
 * 			het oude DeltaType is Change en het nieuwe Remove, vervang de oude dan door de nieuwe
 * 			het oude DeltaType is Add en het nieuwe is Change, vervang dan in de oude de rolID door die van de nieuwe.
 * 		Indien niet gevonden: voeg de nieuwe toe.
 * ZO NEE:
 * 4. zoek een delta waarvan id, rolName en rolID gelijk zijn aan die van de nieuwe en het ene DeltaType Add is en het andere Remove.
 * 	Indien gevonden: verwijder de oude.
 * 	Anders: voeg de nieuwe toe.
 */
var addDelta = function (v) {
    var replace = function (i) {
        return function (delta) {
            return function (v1) {
                var $206 = {};
                for (var $207 in v1) {
                    if ({}.hasOwnProperty.call(v1, $207)) {
                        $206[$207] = v1[$207];
                    };
                };
                $206.deltas = Data_Array.cons(v)(Data_Maybe.maybe(v1.deltas)(Control_Category.id(Control_Category.categoryFn))(Data_Array.deleteAt(i)(v1.deltas)));
                return $206;
            };
        };
    };
    var remove = function (i) {
        return function (v1) {
            var $212 = {};
            for (var $213 in v1) {
                if ({}.hasOwnProperty.call(v1, $213)) {
                    $212[$213] = v1[$213];
                };
            };
            $212.deltas = Data_Array["delete"](Perspectives_TypesForDeltas.eqDelta)(i)(v1.deltas);
            return $212;
        };
    };
    var equalIdRolName = function (v1) {
        return v.id === v1.id && v.memberName === v1.memberName;
    };
    var equalExceptRolID = function (v1) {
        return v.id === v1.id && (v.memberName === v1.memberName && Data_Eq.eq(Perspectives_TypesForDeltas.eqDeltaType)(v.deltaType)(v1.deltaType));
    };
    var equalExceptDeltaType = function (v1) {
        return v.id === v1.id && (v.memberName === v1.memberName && (Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqString))(v.value)(v1.value) && (Data_Eq.eq(Perspectives_TypesForDeltas.eqDeltaType)(v.deltaType)(Perspectives_TypesForDeltas.Add.value) && Data_Eq.eq(Perspectives_TypesForDeltas.eqDeltaType)(v1.deltaType)(Perspectives_TypesForDeltas.Remove.value) || Data_Eq.eq(Perspectives_TypesForDeltas.eqDeltaType)(v.deltaType)(Perspectives_TypesForDeltas.Remove.value) && Data_Eq.eq(Perspectives_TypesForDeltas.eqDeltaType)(v1.deltaType)(Perspectives_TypesForDeltas.Add.value))));
    };
    var add = function (delta) {
        return function (v1) {
            var $230 = {};
            for (var $231 in v1) {
                if ({}.hasOwnProperty.call(v1, $231)) {
                    $230[$231] = v1[$231];
                };
            };
            $230.deltas = Data_Array.cons(delta)(v1.deltas);
            return $230;
        };
    };
    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))))(function (v1) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))((function () {
            var v2 = Data_Array.elemIndex(Perspectives_TypesForDeltas.eqDelta)(v)(v1.deltas);
            if (v2 instanceof Data_Maybe.Just) {
                return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Unit.unit);
            };
            if (v2 instanceof Data_Maybe.Nothing) {
                return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Perspectives_RunMonadPerspectivesQuery.runMonadPerspectivesQuery(v.memberName)(Perspectives_QueryCombinators.toBoolean((function () {
                    if (v.isContext) {
                        return Perspectives_ModelBasedTripleGetters.rolIsFunctioneelM;
                    };
                    return Perspectives_ModelBasedTripleGetters.propertyIsFunctioneelM;
                })()))))(function (v3) {
                    if (v3) {
                        return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Array.findIndex(equalExceptRolID)(v1.deltas)))(function (v4) {
                            if (v4 instanceof Data_Maybe.Just) {
                                return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(replace(v4.value0)(v)(v1));
                            };
                            if (v4 instanceof Data_Maybe.Nothing) {
                                return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Foldable.find(Data_Foldable.foldableArray)(equalIdRolName)(v1.deltas)))(function (v5) {
                                    if (v5 instanceof Data_Maybe.Nothing) {
                                        return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(add(v)(v1));
                                    };
                                    if (v5 instanceof Data_Maybe.Just) {
                                        return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Maybe.fromJust()(Data_Array.elemIndex(Perspectives_TypesForDeltas.eqDelta)(v5.value0)(v1.deltas))))(function (v6) {
                                            if (v5.value0.deltaType instanceof Perspectives_TypesForDeltas.Add && Data_Eq.eq(Perspectives_TypesForDeltas.eqDeltaType)(v.deltaType)(Perspectives_TypesForDeltas.Remove.value)) {
                                                return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(remove(v5.value0)(v1));
                                            };
                                            if (v5.value0.deltaType instanceof Perspectives_TypesForDeltas.Remove && Data_Eq.eq(Perspectives_TypesForDeltas.eqDeltaType)(v.deltaType)(Perspectives_TypesForDeltas.Add.value)) {
                                                return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(remove(v5.value0)(v1));
                                            };
                                            if (v5.value0.deltaType instanceof Perspectives_TypesForDeltas.Change && Data_Eq.eq(Perspectives_TypesForDeltas.eqDeltaType)(v.deltaType)(Perspectives_TypesForDeltas.Remove.value)) {
                                                return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(replace(v6)(v)(v1));
                                            };
                                            if (v5.value0.deltaType instanceof Perspectives_TypesForDeltas.Add && Data_Eq.eq(Perspectives_TypesForDeltas.eqDeltaType)(v.deltaType)(Perspectives_TypesForDeltas.Change.value)) {
                                                return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(replace(v6)((function () {
                                                    var $247 = {};
                                                    for (var $248 in v5.value0) {
                                                        if ({}.hasOwnProperty.call(v5.value0, $248)) {
                                                            $247[$248] = v5["value0"][$248];
                                                        };
                                                    };
                                                    $247.value = v.value;
                                                    return $247;
                                                })())(v1));
                                            };
                                            return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(add(v)(v1));
                                        });
                                    };
                                    throw new Error("Failed pattern match at Perspectives.Deltas line 189, column 15 - line 198, column 51: " + [ v5.constructor.name ]);
                                });
                            };
                            throw new Error("Failed pattern match at Perspectives.Deltas line 185, column 11 - line 198, column 51: " + [ v4.constructor.name ]);
                        });
                    };
                    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Array.findIndex(equalExceptDeltaType)(v1.deltas)))(function (v4) {
                        if (v4 instanceof Data_Maybe.Nothing) {
                            return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(add(v)(v1));
                        };
                        if (v4 instanceof Data_Maybe.Just) {
                            return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(replace(v4.value0)(v)(v1));
                        };
                        throw new Error("Failed pattern match at Perspectives.Deltas line 201, column 11 - line 203, column 48: " + [ v4.constructor.name ]);
                    });
                });
            };
            throw new Error("Failed pattern match at Perspectives.Deltas line 178, column 3 - line 203, column 48: " + [ v2.constructor.name ]);
        })())(function () {
            return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Unit.unit);
        });
    });
};
var setBinding = function (rid) {
    return function (boundRol) {
        return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(Perspectives_DataTypeObjectGetters.binding(rid)))(function (v) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(updatePerspectEntiteit$prime(Perspectives_PerspectEntiteit.perspectEntiteitRol)(Perspectives_ContextAndRole.changeRol_binding)(rid)(boundRol))(function () {
                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))((function () {
                    var v1 = Data_Array.head(v);
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Data_Unit.unit);
                    };
                    if (v1 instanceof Data_Maybe.Just) {
                        return updatePerspectEntiteitMember$prime(Perspectives_PerspectEntiteit.perspectEntiteitRol)(Perspectives_ContextAndRole.removeRol_gevuldeRollen)(v1.value0)("model:Perspectives$binding")(rid);
                    };
                    throw new Error("Failed pattern match at Perspectives.Deltas line 413, column 3 - line 415, column 107: " + [ v1.constructor.name ]);
                })())(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(updatePerspectEntiteitMember$prime(Perspectives_PerspectEntiteit.perspectEntiteitRol)(Perspectives_ContextAndRole.addRol_gevuldeRollen)(boundRol)("model:Perspectives$binding")(rid))(function () {
                        return addDelta({
                            id: rid,
                            memberName: "model:Perspectives$binding",
                            deltaType: Perspectives_TypesForDeltas.Change.value,
                            value: new Data_Maybe.Just(boundRol),
                            isContext: false
                        });
                    });
                });
            });
        });
    };
};

//---------------------------------------------------------
// UPDATEPERSPECTENTITEIT
//---------------------------------------------------------
/**
 * 
 * Om een door de gebruiker aangebrachte wijziging door te voeren, moet je:
 *   - een Delta maken;
 *   - die versturen aan alle betrokkenen.
 *     - wat is het type van de context?
 *     - wie zijn de betrokkenen?
 *     - welke betrokkenen hebben een Actie met als lijdend voorwerp de entiteit?
 *     - heeft die Actie een view met de betreffende property?
 *   - de wijziging doorvoeren op de interne representatie;
 *   - de consequenties doorvoeren in de triple administratie;
 *   - de gewijzigde context opslaan;
 * 
 * Om een door een andere gebruiker aangebrachte wijziging door te voeren, moet je:
 *   - controleren of de author wel gerechtigd is tot de wijziging;
 *     - in welke rol is de author betrokken bij de context (van de rol)?
 *     - heeft die rol een actie die de betreffende delta oplevert?
 *       - past het werkwoord bij de DeltaType?
 *       - is het lijdend voorwerp de betreffende rol of context?
 *       - heeft de view op het lijdend voorwerp de relevante property (indien het gaat om een property delta)?
 *   - de wijziging doorvoeren op de interne representatie;
 *   - de consequenties doorvoeren in de triple administratie;
 *   - de gewijzigde context opslaan;
 */
// | Create update functions on PerspectContext or PerspectRol.
var updatePerspectEntiteit = function (dictPerspectEntiteit) {
    return function (changeEntity) {
        return function (createDelta) {
            return function (cid) {
                return function (value) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(updatePerspectEntiteit$prime(dictPerspectEntiteit)(changeEntity)(cid)(value))(function () {
                        return addDelta(createDelta(cid)(value));
                    });
                };
            };
        };
    };
};
var setContext = updatePerspectEntiteit(Perspectives_PerspectEntiteit.perspectEntiteitRol)(Perspectives_ContextAndRole.changeRol_context)(function (cid) {
    return function (rol) {
        return {
            id: cid,
            memberName: "model:Perspectives$context",
            deltaType: Perspectives_TypesForDeltas.Change.value,
            value: new Data_Maybe.Just(rol),
            isContext: false
        };
    };
});
var setContextDisplayName = updatePerspectEntiteit(Perspectives_PerspectEntiteit.perspectEntiteitContext)(Perspectives_ContextAndRole.changeContext_displayName)(function (cid) {
    return function (displayName) {
        return {
            id: cid,
            memberName: "model:Perspectives$label",
            deltaType: Perspectives_TypesForDeltas.Change.value,
            value: new Data_Maybe.Just(displayName),
            isContext: true
        };
    };
});

// rev <- lift $ onNothing' ("updatePerspectEntiteit: context has no revision, deltas are impossible: " <> cid) (getRevision' context)
// -- Store the changed entity in couchdb.
// newRev <- lift $ modifyResourceInCouchdb cid rev (encode context)
// -- Set the new revision in the entity.
// lift $ cacheCachedEntiteit cid (setRevision newRev context)
var setContextType = updatePerspectEntiteit(Perspectives_PerspectEntiteit.perspectEntiteitContext)(Perspectives_ContextAndRole.changeContext_type)(function (cid) {
    return function (theType) {
        return {
            id: cid,
            memberName: "model:Perspectives$type",
            deltaType: Perspectives_TypesForDeltas.Change.value,
            value: new Data_Maybe.Just(theType),
            isContext: true
        };
    };
});
var setRolType = updatePerspectEntiteit(Perspectives_PerspectEntiteit.perspectEntiteitRol)(Perspectives_ContextAndRole.changeRol_type)(function (cid) {
    return function (theType) {
        return {
            id: cid,
            memberName: "model:Perspectives$type",
            deltaType: Perspectives_TypesForDeltas.Change.value,
            value: new Data_Maybe.Just(theType),
            isContext: false
        };
    };
});

//---------------------------------------------------------
// UPDATEPERSPECTENTITEITMEMBER
//---------------------------------------------------------
var updatePerspectEntiteitMember = function (dictPerspectEntiteit) {
    return function (changeEntityMember) {
        return function (createDelta) {
            return function (cid) {
                return function (memberName) {
                    return function (value) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(updatePerspectEntiteitMember$prime(dictPerspectEntiteit)(changeEntityMember)(cid)(memberName)(value))(function () {
                            return addDelta(createDelta(cid)(memberName)(value));
                        });
                    };
                };
            };
        };
    };
};
var addProperty = updatePerspectEntiteitMember(Perspectives_PerspectEntiteit.perspectEntiteitRol)(Perspectives_ContextAndRole.addRol_property)(function (rid) {
    return function (propertyName) {
        return function (value) {
            return {
                id: rid,
                memberName: propertyName,
                deltaType: Perspectives_TypesForDeltas.Add.value,
                value: new Data_Maybe.Just(value),
                isContext: false
            };
        };
    };
});

// rev <- lift $ onNothing' ("updateRoleProperty: context has no revision, deltas are impossible: " <> cid) (un(getRevision' context))
// -- Store the changed entity in couchdb.
// newRev <- lift $ modifyResourceInCouchdb cid rev (encode context)
// -- Set the new revision in the entity.
// lift $ cacheCachedEntiteit cid (setRevision newRev context)
// | Add a rol to a context (and inversely register the context with the rol)
// | In a functional rol, remove an old
var addRol = updatePerspectEntiteitMember(Perspectives_PerspectEntiteit.perspectEntiteitContext)(Perspectives_ContextAndRole.addContext_rolInContext)(function (cid) {
    return function (rolName) {
        return function (rolId) {
            return {
                id: cid,
                memberName: rolName,
                deltaType: Perspectives_TypesForDeltas.Add.value,
                value: new Data_Maybe.Just(rolId),
                isContext: true
            };
        };
    };
});
var removeProperty = updatePerspectEntiteitMember(Perspectives_PerspectEntiteit.perspectEntiteitRol)(Perspectives_ContextAndRole.removeRol_property)(function (rid) {
    return function (propertyName) {
        return function (value) {
            return {
                id: rid,
                memberName: propertyName,
                deltaType: Perspectives_TypesForDeltas.Remove.value,
                value: new Data_Maybe.Just(value),
                isContext: false
            };
        };
    };
});
var removeRol = updatePerspectEntiteitMember(Perspectives_PerspectEntiteit.perspectEntiteitContext)(Perspectives_ContextAndRole.removeContext_rolInContext)(function (cid) {
    return function (rolName) {
        return function (rolId) {
            return {
                id: cid,
                memberName: rolName,
                deltaType: Perspectives_TypesForDeltas.Remove.value,
                value: new Data_Maybe.Just(rolId),
                isContext: true
            };
        };
    };
});
var setProperty = updatePerspectEntiteitMember(Perspectives_PerspectEntiteit.perspectEntiteitRol)(Perspectives_ContextAndRole.setRol_property)(function (rid) {
    return function (propertyName) {
        return function (value) {
            return {
                id: rid,
                memberName: propertyName,
                deltaType: Perspectives_TypesForDeltas.Change.value,
                value: new Data_Maybe.Just(value),
                isContext: false
            };
        };
    };
});
var setRol = updatePerspectEntiteitMember(Perspectives_PerspectEntiteit.perspectEntiteitContext)(Perspectives_ContextAndRole.setContext_rolInContext)(function (cid) {
    return function (rolName) {
        return function (rolId) {
            return {
                id: cid,
                memberName: rolName,
                deltaType: Perspectives_TypesForDeltas.Change.value,
                value: new Data_Maybe.Just(rolId),
                isContext: true
            };
        };
    };
});
var addContextToTransactie = function (c) {
    return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))))(function (v) {
        return Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff)))((function () {
            var $265 = {};
            for (var $266 in v) {
                if ({}.hasOwnProperty.call(v, $266)) {
                    $265[$266] = v[$266];
                };
            };
            $265.createdContexts = Data_Array.cons(c)(v.createdContexts);
            return $265;
        })());
    });
};
module.exports = {
    SerializableDateTime: SerializableDateTime,
    Transactie: Transactie,
    createTransactie: createTransactie,
    transactieID: transactieID,
    runInTransactie: runInTransactie,
    distributeTransactie: distributeTransactie,
    addContextToTransactie: addContextToTransactie,
    addRolToTransactie: addRolToTransactie,
    deleteContextFromTransactie: deleteContextFromTransactie,
    deleteRolFromTransactie: deleteRolFromTransactie,
    addDomeinFileToTransactie: addDomeinFileToTransactie,
    addDelta: addDelta,
    sendTransactieToUser: sendTransactieToUser,
    usersInvolvedInDelta: usersInvolvedInDelta,
    transactieForEachUser: transactieForEachUser,
    updatePerspectEntiteit: updatePerspectEntiteit,
    "updatePerspectEntiteit'": updatePerspectEntiteit$prime,
    setContextType: setContextType,
    setRolType: setRolType,
    setContextDisplayName: setContextDisplayName,
    setContext: setContext,
    setBinding: setBinding,
    updatePerspectEntiteitMember: updatePerspectEntiteitMember,
    "updatePerspectEntiteitMember'": updatePerspectEntiteitMember$prime,
    addRol: addRol,
    removeRol: removeRol,
    setRol: setRol,
    addProperty: addProperty,
    removeProperty: removeProperty,
    setProperty: setProperty,
    onNothing: onNothing,
    "onNothing'": onNothing$prime,
    encodeSerializableDateTime: encodeSerializableDateTime,
    showSerializableDateTime: showSerializableDateTime,
    genericRepTransactie: genericRepTransactie,
    showTransactie: showTransactie,
    encodeTransactie: encodeTransactie
};
