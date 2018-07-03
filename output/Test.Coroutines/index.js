"use strict";
var Control_Bind = require("../Control.Bind");
var Control_Coroutine = require("../Control.Coroutine");
var Control_Coroutine_Aff = require("../Control.Coroutine.Aff");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("../Control.Monad.Aff.AVar");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Parallel_Class = require("../Control.Parallel.Class");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Time_Duration = require("../Data.Time.Duration");
var Perspectives_CoreTypes = require("../Perspectives.CoreTypes");
var Prelude = require("../Prelude");
var q = Control_Coroutine_Aff["produce'"](Control_Monad_Aff_Class.monadAffReader(Control_Monad_Aff_Class.monadAffAff))(function (emit) {
    return function __do() {
        Control_Monad_Eff_Console.log("Working...")();
        emit(new Data_Either.Left("progress"))();
        Control_Monad_Eff_Console.log("Done!")();
        return emit(new Data_Either.Right("finished"))();
    };
});
var p = Control_Coroutine_Aff.produceAff(function (emit) {
    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Aff.delay(Data_Newtype.wrap(Data_Time_Duration.newtypeMilliseconds)(1000.0)))(function () {
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(emit(new Data_Either.Left("Working...")))(function () {
            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Aff.delay(Data_Newtype.wrap(Data_Time_Duration.newtypeMilliseconds)(1000.0)))(function () {
                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(emit(new Data_Either.Left("Working...")))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Aff.delay(Data_Newtype.wrap(Data_Time_Duration.newtypeMilliseconds)(1000.0)))(function () {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(emit(new Data_Either.Left("Working...")))(function () {
                            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Aff.delay(Data_Newtype.wrap(Data_Time_Duration.newtypeMilliseconds)(1000.0)))(function () {
                                return emit(new Data_Either.Right("Done!"));
                            });
                        });
                    });
                });
            });
        });
    });
});
var c = Control_Coroutine.consumer(Control_Monad_Reader_Trans.monadReaderT(Control_Monad_Aff.monadAff))(function (s) {
    return Data_Functor.voidLeft(Control_Monad_Reader_Trans.functorReaderT(Control_Monad_Aff.functorAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Reader_Trans.monadEffReader(Control_Monad_Aff.monadEffAff))(Control_Monad_Eff_Console.log(s)))(Data_Maybe.Nothing.value);
});

// test :: forall eff. Eff (console :: CONSOLE, avar :: AVAR, err :: EXCEPTION | eff) Unit
// test = void $ runAff (either logShow log) $ runProcess (q $$ c)
var test = Control_Coroutine.runProcess(Control_Monad_Reader_Trans.monadRecReaderT(Control_Monad_Aff.monadRecAff))(Control_Coroutine.connect(Control_Monad_Reader_Trans.monadRecReaderT(Control_Monad_Aff.monadRecAff))(Control_Parallel_Class.monadParReaderT(Control_Monad_Aff.parallelAff))(q)(c));
module.exports = {
    p: p,
    q: q,
    c: c,
    test: test
};
