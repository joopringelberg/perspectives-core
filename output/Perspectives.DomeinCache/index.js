"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("../Control.Monad.Aff.AVar");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Except = require("../Control.Monad.Except");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foreign_Generic = require("../Data.Foreign.Generic");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HTTP_Method = require("../Data.HTTP.Method");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Semigroup = require("../Data.Semigroup");
var Data_StrMap = require("../Data.StrMap");
var Network_HTTP_Affjax = require("../Network.HTTP.Affjax");
var Network_HTTP_Affjax_Request = require("../Network.HTTP.Affjax.Request");
var Network_HTTP_StatusCode = require("../Network.HTTP.StatusCode");
var Partial_Unsafe = require("../Partial.Unsafe");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Perspectives_Couchdb = require("../Perspectives.Couchdb");
var Perspectives_Couchdb_Databases = require("../Perspectives.Couchdb.Databases");
var Perspectives_DomeinFile = require("../Perspectives.DomeinFile");
var Perspectives_Effects = require("../Perspectives.Effects");
var Perspectives_EntiteitAndRDFAliases = require("../Perspectives.EntiteitAndRDFAliases");
var Perspectives_GlobalUnsafeStrMap = require("../Perspectives.GlobalUnsafeStrMap");
var Perspectives_Identifiers = require("../Perspectives.Identifiers");
var Perspectives_PerspectivesState = require("../Perspectives.PerspectivesState");
var Perspectives_Syntax = require("../Perspectives.Syntax");
var Prelude = require("../Prelude");
var storeDomeinFileInCache = function (ns) {
    return function (df) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectivesState.domeinCache)(function (v) {
            return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Perspectives_GlobalUnsafeStrMap.poke(v)(ns)(df))(Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(df))));
        });
    };
};

// | Change the domeinfile in cache. NOTA BENE: does not store the modified file in Couchdb!
var modifyDomeinFileInCache = function (ns) {
    return function (modifier) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectivesState.domeinCacheLookup(ns))(function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("modifyDomeinFileInCache cannot find domeinfile in cache: " + ns));
            };
            if (v instanceof Data_Maybe.Just) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.takeVar(v.value0)))(function (v1) {
                    return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.putVar(modifier(v1))(v.value0));
                });
            };
            throw new Error("Failed pattern match at Perspectives.DomeinCache line 44, column 3 - line 48, column 42: " + [ v.constructor.name ]);
        });
    };
};
var modelsURL = "http://localhost:5984/perspect_models/";
var modifyDomeinFileInCouchdb = function (v) {
    return function (av) {
        var setRevision = function (s) {
            return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.putVar((function () {
                var $44 = {};
                for (var $45 in v) {
                    if ({}.hasOwnProperty.call(v, $45)) {
                        $44[$45] = v[$45];
                    };
                };
                $44._rev = Perspectives_Syntax.revision(s);
                return $44;
            })())(av));
        };
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.readVar(av)))(function (v1) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Maybe.fromJust()(v1._rev)))(function (v2) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.takeVar(av)))(function (v3) {
                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Network_HTTP_Affjax.put(Network_HTTP_Affjax_Request.requestableString)(Perspectives_Couchdb.respondablePutCouchdbDocument)(modelsURL + (Perspectives_Identifiers.escapeCouchdbDocumentName(v._id) + ("?_rev=" + v2)))(Data_Foreign_Generic.encodeJSON(Perspectives_DomeinFile.encodeDomeinFile)((function () {
                        var $50 = {};
                        for (var $51 in v) {
                            if ({}.hasOwnProperty.call(v, $51)) {
                                $50[$51] = v[$51];
                            };
                        };
                        $50._rev = v1._rev;
                        return $50;
                    })()))))(function (v4) {
                        var $54 = Data_Eq.eq(Network_HTTP_StatusCode.eqStatusCode)(v4.status)(409);
                        if ($54) {
                            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.retrieveDocumentVersion(modelsURL + Perspectives_Identifiers.escapeCouchdbDocumentName(v._id)))(function (v5) {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(setRevision(v5))(function () {
                                    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.readVar(av)))(function (v6) {
                                        return modifyDomeinFileInCouchdb(v6)(av);
                                    });
                                });
                            });
                        };
                        return Perspectives_Couchdb.onAccepted(Control_Monad_Reader_Trans.monadErrorReaderT(Control_Monad_Aff.monadErrorAff))(v4.status)([ 200, 201 ])("modifyDomeinFileInCouchdb")(setRevision(Data_Maybe.fromJust()((Data_Newtype.unwrap(Perspectives_Couchdb.newtypePutCouchdbDocument)(v4.response)).rev)));
                    });
                });
            });
        });
    };
};
var saveCachedDomeinFile = function (ns) {
    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectivesState.domeinCacheLookup(ns))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("saveCachedDomeinFile: cannot find domeinfile in cache: " + ns));
        };
        if (v instanceof Data_Maybe.Just) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.readVar(v.value0)))(function (v1) {
                return modifyDomeinFileInCouchdb(v1)(v.value0);
            });
        };
        throw new Error("Failed pattern match at Perspectives.DomeinCache line 102, column 3 - line 106, column 40: " + [ v.constructor.name ]);
    });
};
var domeinRequest = {
    method: new Data_Either.Left(Data_HTTP_Method.GET.value),
    url: "http://localhost:5984/models2model_SysteemDomein_",
    headers: [  ],
    content: Data_Maybe.Nothing.value,
    username: new Data_Maybe.Just("cor"),
    password: new Data_Maybe.Just("geheim"),
    withCredentials: true
};
var retrieveDomeinFile = function (ns) {
    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectivesState.domeinCacheLookup(ns))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.makeEmptyVar))(Perspectives_PerspectivesState.domeinCacheInsert(ns)))(function (v1) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Network_HTTP_Affjax.affjax(Network_HTTP_Affjax_Request.requestableUnit)(Perspectives_DomeinFile.respondableDomeinFile)((function () {
                    var $66 = {};
                    for (var $67 in domeinRequest) {
                        if ({}.hasOwnProperty.call(domeinRequest, $67)) {
                            $66[$67] = domeinRequest[$67];
                        };
                    };
                    $66.url = modelsURL + Perspectives_Identifiers.escapeCouchdbDocumentName(ns);
                    return $66;
                })())))(function (v2) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Perspectives_Couchdb.onAccepted(Control_Monad_Aff.monadErrorAff)(v2.status)([ 200, 304 ])("retrieveDomeinFile")(Control_Monad_Aff_AVar.putVar(v2.response)(v1))))(function () {
                        return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.readVar(v1));
                    });
                });
            });
        };
        if (v instanceof Data_Maybe.Just) {
            return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.readVar(v.value0));
        };
        throw new Error("Failed pattern match at Perspectives.DomeinCache line 75, column 3 - line 83, column 42: " + [ v.constructor.name ]);
    });
};

// | Fetch a PerspectContext asynchronously from its Domein, loading the Domein file if necessary.
var retrieveContextFromDomein = function (id) {
    return function (ns) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(retrieveDomeinFile(ns))(function (v) {
            var v1 = Data_StrMap.lookup(id)(v.contexts);
            if (v1 instanceof Data_Maybe.Nothing) {
                return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("retrieveContextFromDomein: cannot find definition of " + (id + (" in retrieveContextFromDomein for " + ns))));
            };
            if (v1 instanceof Data_Maybe.Just) {
                return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(v1.value0);
            };
            throw new Error("Failed pattern match at Perspectives.DomeinCache line 57, column 3 - line 59, column 35: " + [ v1.constructor.name ]);
        });
    };
};

// | Fetch a PerspectRol asynchronously from its Domein, loading the Domein file if necessary.
var retrieveRolFromDomein = function (id) {
    return function (ns) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(retrieveDomeinFile(ns))(function (v) {
            var v1 = Data_StrMap.lookup(id)(v.roles);
            if (v1 instanceof Data_Maybe.Nothing) {
                return Control_Monad_Error_Class.throwError(Control_Monad_Reader_Trans.monadThrowReaderT(Control_Monad_Aff.monadThrowAff))(Control_Monad_Eff_Exception.error("retrieveRolFromDomein: cannot find definition of " + (id + (" in retrieveRolFromDomein for " + ns))));
            };
            if (v1 instanceof Data_Maybe.Just) {
                return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(v1.value0);
            };
            throw new Error("Failed pattern match at Perspectives.DomeinCache line 68, column 3 - line 70, column 27: " + [ v1.constructor.name ]);
        });
    };
};
var createDomeinFileInCouchdb = function (v) {
    var setRevision = function (s) {
        return function (av) {
            return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.putVar((function () {
                var $80 = {};
                for (var $81 in v) {
                    if ({}.hasOwnProperty.call(v, $81)) {
                        $80[$81] = v[$81];
                    };
                };
                $80._rev = Perspectives_Syntax.revision(s);
                return $80;
            })())(av));
        };
    };
    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.makeEmptyVar))(Perspectives_PerspectivesState.domeinCacheInsert(v._id)))(function (v1) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Network_HTTP_Affjax.put(Network_HTTP_Affjax_Request.requestableString)(Perspectives_Couchdb.respondablePutCouchdbDocument)(modelsURL + Perspectives_Identifiers.escapeCouchdbDocumentName(v._id))(Data_Foreign_Generic.encodeJSON(Perspectives_DomeinFile.encodeDomeinFile)(v))))(function (v2) {
            var $85 = Data_Eq.eq(Network_HTTP_StatusCode.eqStatusCode)(v2.status)(409);
            if ($85) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.retrieveDocumentVersion(modelsURL + Perspectives_Identifiers.escapeCouchdbDocumentName(v._id)))(function (v3) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(setRevision(v3)(v1))(function () {
                        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Control_Monad_Aff_AVar.readVar(v1)))(function (v4) {
                            return modifyDomeinFileInCouchdb(v4)(v1);
                        });
                    });
                });
            };
            return Perspectives_Couchdb.onAccepted(Control_Monad_Reader_Trans.monadErrorReaderT(Control_Monad_Aff.monadErrorAff))(v2.status)([ 200, 201 ])("createDomeinFileInCouchdb")(setRevision(Data_Maybe.fromJust()((Data_Newtype.unwrap(Perspectives_Couchdb.newtypePutCouchdbDocument)(v2.response)).rev))(v1));
        });
    });
};

// | Either create or modify the DomeinFile in couchdb. Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
var storeDomeinFileInCouchdb = function (v) {
    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_PerspectivesState.domeinCacheLookup(v._id))(function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
            return createDomeinFileInCouchdb(v);
        };
        if (v1 instanceof Data_Maybe.Just) {
            return modifyDomeinFileInCouchdb(v)(v1.value0);
        };
        throw new Error("Failed pattern match at Perspectives.DomeinCache line 113, column 3 - line 115, column 53: " + [ v1.constructor.name ]);
    });
};
var baseURL = "http://localhost:5984/";
var documentsInDatabase = function (database) {
    return Control_Bind.bind(Control_Monad_Aff.bindAff)(Network_HTTP_Affjax.affjax(Network_HTTP_Affjax_Request.requestableUnit)(Perspectives_Couchdb.respondableGetCouchdbAllDocs)((function () {
        var $95 = {};
        for (var $96 in domeinRequest) {
            if ({}.hasOwnProperty.call(domeinRequest, $96)) {
                $95[$96] = domeinRequest[$96];
            };
        };
        $95.url = baseURL + (Perspectives_Identifiers.escapeCouchdbDocumentName(database) + "/_all_docs");
        return $95;
    })()))(function (v) {
        return Perspectives_Couchdb.onAccepted(Control_Monad_Aff.monadErrorAff)(v.status)([ 200 ])("documentsInDatabase")(Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v.response));
    });
};
var documentNamesInDatabase = function (database) {
    return Control_Bind.bind(Control_Monad_Aff.bindAff)(documentsInDatabase(database))(function (v) {
        return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(Data_Functor.map(Data_Functor.functorArray)(function (v1) {
            return v1.id;
        })(v.rows));
    });
};
module.exports = {
    storeDomeinFileInCache: storeDomeinFileInCache,
    modifyDomeinFileInCache: modifyDomeinFileInCache,
    retrieveContextFromDomein: retrieveContextFromDomein,
    retrieveRolFromDomein: retrieveRolFromDomein,
    retrieveDomeinFile: retrieveDomeinFile,
    documentsInDatabase: documentsInDatabase,
    documentNamesInDatabase: documentNamesInDatabase,
    saveCachedDomeinFile: saveCachedDomeinFile,
    storeDomeinFileInCouchdb: storeDomeinFileInCouchdb,
    createDomeinFileInCouchdb: createDomeinFileInCouchdb,
    modifyDomeinFileInCouchdb: modifyDomeinFileInCouchdb,
    modelsURL: modelsURL,
    baseURL: baseURL,
    domeinRequest: domeinRequest
};
