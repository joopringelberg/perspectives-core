// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Argonaut = require("../Data.Argonaut");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_HTTP_Method = require("../Data.HTTP.Method");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Unit = require("../Data.Unit");
var Network_HTTP_Affjax = require("../Network.HTTP.Affjax");
var Network_HTTP_Affjax_Request = require("../Network.HTTP.Affjax.Request");
var Network_HTTP_Affjax_Response = require("../Network.HTTP.Affjax.Response");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Perspectives_Couchdb = require("../Perspectives.Couchdb");
var Perspectives_Couchdb_Databases = require("../Perspectives.Couchdb.Databases");
var Perspectives_Effects = require("../Perspectives.Effects");
var Perspectives_User = require("../Perspectives.User");
var Prelude = require("../Prelude");
var partyMode = Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.allDbs)(function ($18) {
    return Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Array["null"]($18));
});
var createUserDatabases = function (user) {
    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.createDatabase("user_" + (user + "_entities")))(function () {
        return Perspectives_Couchdb_Databases.createDatabase("user_" + (user + "_post"));
    });
};
var createSystemDatabases = Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.createDatabase("_users"))(function () {
    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.createDatabase("_replicator"))(function () {
        return Perspectives_Couchdb_Databases.createDatabase("_global_changes");
    });
});
var createFirstAdmin = function (user) {
    return function (password) {
        return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_User.getCouchdbBaseURL)(function (v) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Network_HTTP_Affjax.put(Network_HTTP_Affjax_Request.requestableJson)(Network_HTTP_Affjax_Response.responsableString)(v + ("_node/couchdb@localhost/_config/admins/" + user))(Data_Argonaut_Core.fromString(password))))(function (v1) {
                return Perspectives_Couchdb.onAccepted(Control_Monad_Reader_Trans.monadErrorReaderT(Control_Monad_Aff.monadErrorAff))(v1.status)([ 200 ])("createFirstAdmin")(Control_Applicative.pure(Control_Monad_Reader_Trans.applicativeReaderT(Control_Monad_Aff.applicativeAff))(Data_Unit.unit));
            });
        });
    };
};
var setupCouchdb = Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_User.getUser)(function (v) {
    return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_User.getCouchdbPassword)(function (v1) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(createFirstAdmin(v)(v1))(function () {
            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(createSystemDatabases)(function () {
                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(createUserDatabases(v))(function () {
                    return Perspectives_Couchdb_Databases.createDatabase("perspect_models");
                });
            });
        });
    });
});
var createAnotherAdmin = function (user) {
    return function (password) {
        return Perspectives_Couchdb_Databases.ensureAuthentication(Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_User.getCouchdbBaseURL)(function (v) {
            return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Perspectives_Couchdb_Databases.defaultPerspectRequest)(function (v1) {
                return Control_Bind.bind(Control_Monad_Reader_Trans.bindReaderT(Control_Monad_Aff.bindAff))(Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Network_HTTP_Affjax.affjax(Network_HTTP_Affjax_Request.requestableJson)(Network_HTTP_Affjax_Response.responsableString)((function () {
                    var $14 = {};
                    for (var $15 in v1) {
                        if ({}.hasOwnProperty.call(v1, $15)) {
                            $14[$15] = v1[$15];
                        };
                    };
                    $14.method = new Data_Either.Left(Data_HTTP_Method.PUT.value);
                    $14.url = v + ("_node/couchdb@localhost/_config/admins/" + user);
                    $14.content = new Data_Maybe.Just(Data_Argonaut_Core.fromString(password));
                    return $14;
                })())))(function (v2) {
                    return Control_Monad_Aff_Class.liftAff(Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(Perspectives_Couchdb.onAccepted(Control_Monad_Aff.monadErrorAff)(v2.status)([ 200 ])("createAnotherAdmin")(Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(Data_Unit.unit)));
                });
            });
        }));
    };
};
module.exports = {
    setupCouchdb: setupCouchdb,
    createFirstAdmin: createFirstAdmin,
    createAnotherAdmin: createAnotherAdmin,
    createSystemDatabases: createSystemDatabases,
    createUserDatabases: createUserDatabases,
    partyMode: partyMode
};
