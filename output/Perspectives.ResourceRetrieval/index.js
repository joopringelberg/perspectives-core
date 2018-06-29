"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("../Control.Monad.Aff.AVar");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Except = require("../Control.Monad.Except");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HTTP_Method = require("../Data.HTTP.Method");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Semigroup = require("../Data.Semigroup");
var Network_HTTP_Affjax = require("../Network.HTTP.Affjax");
var Network_HTTP_Affjax_Request = require("../Network.HTTP.Affjax.Request");
var Network_HTTP_StatusCode = require("../Network.HTTP.StatusCode");
var Partial_Unsafe = require("../Partial.Unsafe");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Perspectives_Couchdb = require("../Perspectives.Couchdb");
var Perspectives_Couchdb_Databases = require("../Perspectives.Couchdb.Databases");
var Perspectives_Effects = require("../Perspectives.Effects");
var Perspectives_EntiteitAndRDFAliases = require("../Perspectives.EntiteitAndRDFAliases");
var Perspectives_Identifiers = require("../Perspectives.Identifiers");
var Perspectives_PerspectEntiteit = require("../Perspectives.PerspectEntiteit");
var Perspectives_User = require("../Perspectives.User");
var Prelude = require("../Prelude");
var saveVersionedEntiteit = function (dictPerspectEntiteit) {
    return function (entId) {
        return function (entiteit) {
            return Perspectives_Couchdb_Databases.ensureAuthentication((function () {
                var v = Perspectives_PerspectEntiteit["getRevision'"](dictPerspectEntiteit)(entiteit);
                if (v instanceof Data_Maybe.Nothing) {
                    return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("saveVersionedEntiteit: entiteit has no revision, deltas are impossible: " + entId));
                };
                if (v instanceof Data_Maybe.Just) {
                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_User.entitiesDatabase)(function (v1) {
                        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.defaultPerspectRequest)(function (v2) {
                            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Network_HTTP_Affjax.affjax(Network_HTTP_Affjax_Request.requestableString)(Perspectives_Couchdb.respondablePutCouchdbDocument)((function () {
                                var $32 = {};
                                for (var $33 in v2) {
                                    if ({}.hasOwnProperty.call(v2, $33)) {
                                        $32[$33] = v2[$33];
                                    };
                                };
                                $32.method = new Data_Either.Left(Data_HTTP_Method.PUT.value);
                                $32.url = v1 + (entId + ("?_rev=" + v.value0));
                                $32.content = new Data_Maybe.Just(Perspectives_PerspectEntiteit.encode(dictPerspectEntiteit)(entiteit));
                                return $32;
                            })())))(function (v3) {
                                var $36 = Data_Eq.eq(Network_HTTP_StatusCode.eqStatusCode)(v3.status)(409);
                                if ($36) {
                                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.retrieveDocumentVersion(entId))(function ($64) {
                                        return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Function.flip(Perspectives_PerspectEntiteit.setRevision(dictPerspectEntiteit))(entiteit)($64));
                                    }))(saveVersionedEntiteit(dictPerspectEntiteit)(entId));
                                };
                                return Perspectives_Couchdb.onAccepted(Control_Monad_Reader_Trans.monadErrorReaderT(Control_Monad_Aff.monadErrorAff))(v3.status)([ 200, 201 ])("saveVersionedEntiteit")(Perspectives_PerspectEntiteit.cacheCachedEntiteit(dictPerspectEntiteit)(entId)(Perspectives_PerspectEntiteit.setRevision(dictPerspectEntiteit)(Data_Maybe.fromJust()((Data_Newtype.unwrap(Perspectives_Couchdb.newtypePutCouchdbDocument)(v3.response)).rev))(entiteit)));
                            });
                        });
                    });
                };
                throw new Error("Failed pattern match at Perspectives.ResourceRetrieval line 94, column 3 - line 102, column 173: " + [ v.constructor.name ]);
            })());
        };
    };
};

// | A Resource may be created and stored locally, but not sent to the couchdb. Send such resources to
// | couchdb with this function.
var saveUnversionedEntiteit = function (dictPerspectEntiteit) {
    return function (id) {
        return Perspectives_Couchdb_Databases.ensureAuthentication(Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectEntiteit.retrieveInternally(dictPerspectEntiteit)(id))(function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("saveUnversionedEntiteit needs a locally stored resource for " + id));
            };
            if (v instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.takeVar(v.value0)))(function (v1) {
                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_User.entitiesDatabase)(function (v2) {
                        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.defaultPerspectRequest)(function (v3) {
                            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Network_HTTP_Affjax.affjax(Network_HTTP_Affjax_Request.requestableString)(Perspectives_Couchdb.respondablePutCouchdbDocument)((function () {
                                var $43 = {};
                                for (var $44 in v3) {
                                    if ({}.hasOwnProperty.call(v3, $44)) {
                                        $43[$44] = v3[$44];
                                    };
                                };
                                $43.method = new Data_Either.Left(Data_HTTP_Method.PUT.value);
                                $43.url = v2 + id;
                                $43.content = new Data_Maybe.Just(Perspectives_PerspectEntiteit.encode(dictPerspectEntiteit)(v1));
                                return $43;
                            })())))(function (v4) {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))((function () {
                                    var $47 = Data_Eq.eq(Network_HTTP_StatusCode.eqStatusCode)(v4.status)(409);
                                    if ($47) {
                                        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.retrieveDocumentVersion(v2 + id))(function ($65) {
                                            return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Function.flip(Perspectives_PerspectEntiteit.setRevision(dictPerspectEntiteit))(v1)($65));
                                        }))(function ($66) {
                                            return Data_Functor["void"](Control_Monad_Reader_Trans.functorReaderT(Control_Monad_Aff.functorAff))(saveVersionedEntiteit(dictPerspectEntiteit)(id)($66));
                                        });
                                    };
                                    return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Perspectives_Couchdb.onAccepted(Control_Monad_Aff.monadErrorAff)(v4.status)([ 200, 201 ])("saveUnversionedEntiteit")(Control_Monad_Aff_AVar.putVar(Perspectives_PerspectEntiteit.setRevision(dictPerspectEntiteit)(Data_Maybe.fromJust()((Data_Newtype.unwrap(Perspectives_Couchdb.newtypePutCouchdbDocument)(v4.response)).rev))(v1))(v.value0)));
                                })())(function () {
                                    return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(v1);
                                });
                            });
                        });
                    });
                });
            };
            throw new Error("Failed pattern match at Perspectives.ResourceRetrieval line 79, column 3 - line 90, column 14: " + [ v.constructor.name ]);
        }));
    };
};
var saveEntiteit = function (dictPerspectEntiteit) {
    return function (id) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectEntiteit.readEntiteitFromCache(dictPerspectEntiteit)(id))(function (v) {
            var v1 = Perspectives_PerspectEntiteit["getRevision'"](dictPerspectEntiteit)(v);
            if (v1 instanceof Data_Maybe.Nothing) {
                return saveUnversionedEntiteit(dictPerspectEntiteit)(id);
            };
            return saveVersionedEntiteit(dictPerspectEntiteit)(id)(v);
        });
    };
};
var saveEntiteitPreservingVersion = function (dictPerspectEntiteit) {
    return saveEntiteit(dictPerspectEntiteit);
};

// | Fetch the definition of a resource asynchronously.
var fetchEntiteit = function (dictPerspectEntiteit) {
    return function (id) {
        return Perspectives_Couchdb_Databases.ensureAuthentication(Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectEntiteit.representInternally(dictPerspectEntiteit)(id))(function (v) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_User.entitiesDatabase)(function (v2) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.defaultPerspectRequest)(function (v3) {
                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Network_HTTP_Affjax.affjax(Network_HTTP_Affjax_Request.requestableUnit)(dictPerspectEntiteit.Respondable2())((function () {
                        var $54 = {};
                        for (var $55 in v3) {
                            if ({}.hasOwnProperty.call(v3, $55)) {
                                $54[$55] = v3[$55];
                            };
                        };
                        $54.url = v2 + id;
                        return $54;
                    })())))(function (v4) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Perspectives_Couchdb.onAccepted(Control_Monad_Aff.monadErrorAff)(v4.status)([ 200, 304 ])("fetchEntiteit")(Control_Monad_Aff_AVar.putVar(v4.response)(v))))(function () {
                            return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.readVar(v));
                        });
                    });
                });
            });
        }));
    };
};

// | Fetch the definition of the resource asynchronously, either from a Domein file or from the user database.
// TODO rename
var fetchPerspectEntiteitFromCouchdb = function (dictPerspectEntiteit) {
    return function (id) {
        var $58 = Perspectives_Identifiers.isUserURI(id);
        if ($58) {
            return fetchEntiteit(dictPerspectEntiteit)(id);
        };
        var $59 = Perspectives_Identifiers.isQualifiedWithDomein(id);
        if ($59) {
            var v = Perspectives_Identifiers.deconstructModelName(id);
            if (v instanceof Data_Maybe.Nothing) {
                return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("fetchPerspectEntiteitFromCouchdb: Cannot construct namespace out of id " + id));
            };
            if (v instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectEntiteit.retrieveFromDomein(dictPerspectEntiteit)(id)(v.value0))(function (v1) {
                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectEntiteit.representInternally(dictPerspectEntiteit)(id))(function (v2) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.putVar(v1)(v2)))(function () {
                            return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(v1);
                        });
                    });
                });
            };
            throw new Error("Failed pattern match at Perspectives.ResourceRetrieval line 40, column 10 - line 46, column 17: " + [ v.constructor.name ]);
        };
        return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("fetchPerspectEntiteitFromCouchdb: Unknown URI structure for " + id));
    };
};
module.exports = {
    fetchPerspectEntiteitFromCouchdb: fetchPerspectEntiteitFromCouchdb,
    saveUnversionedEntiteit: saveUnversionedEntiteit,
    saveVersionedEntiteit: saveVersionedEntiteit,
    saveEntiteit: saveEntiteit,
    saveEntiteitPreservingVersion: saveEntiteitPreservingVersion,
    fetchEntiteit: fetchEntiteit
};
