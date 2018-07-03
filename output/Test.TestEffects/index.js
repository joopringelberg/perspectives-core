"use strict";
var Control_Monad_Aff_Console = require("../Control.Monad.Aff.Console");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Now = require("../Control.Monad.Eff.Now");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Perspectives_Effects = require("../Perspectives.Effects");
var Prelude = require("../Prelude");
var handleSuccess = function (dictShow) {
    return function (a) {
        return Control_Monad_Eff_Console.log("Success in test: " + Data_Show.show(dictShow)(a));
    };
};
var handleError = function (dictShow) {
    return function (v) {
        if (v instanceof Data_Either.Left) {
            return Control_Monad_Eff_Console.log("An error caught in test: " + Data_Show.show(Control_Monad_Eff_Exception.showError)(v.value0));
        };
        if (v instanceof Data_Either.Right) {
            return Control_Monad_Eff_Console.log("Success in test: " + Data_Show.show(dictShow)(v.value0));
        };
        throw new Error("Failed pattern match at Test.TestEffects line 17, column 1 - line 17, column 91: " + [ v.constructor.name ]);
    };
};
module.exports = {
    handleError: handleError,
    handleSuccess: handleSuccess
};
