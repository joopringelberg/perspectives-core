"use strict";
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Show = require("../Data.Show");
var Node_FS = require("../Node.FS");
var Perspectives_PerspectivesState = require("../Perspectives.PerspectivesState");
var Prelude = require("../Prelude");
var Test_Coroutines = require("../Test.Coroutines");
var Test_TestEffects = require("../Test.TestEffects");

// import Test.BoundContexts
var main = Control_Monad_Aff.runAff(Test_TestEffects.handleError(Data_Show.showString))(Perspectives_PerspectivesState.runPerspectives("cor")("geheim")(Test_Coroutines.test));
module.exports = {
    main: main
};
