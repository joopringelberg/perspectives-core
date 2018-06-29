// Generated by purs version 0.11.7
"use strict";
var $foreign = require("./foreign");
var Control_Bind = require("../Control.Bind");
var Control_Coroutine = require("../Control.Coroutine");
var Control_Coroutine_Aff = require("../Control.Coroutine.Aff");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_AVar = require("../Control.Monad.Eff.AVar");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Control_Monad_Except = require("../Control.Monad.Except");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class");
var Control_Parallel = require("../Control.Parallel");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foreign = require("../Data.Foreign");
var Data_Foreign_Class = require("../Data.Foreign.Class");
var Data_Foreign_Generic = require("../Data.Foreign.Generic");
var Data_Function = require("../Data.Function");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var writeMessage = function (dictMonadAff) {
    return function (c) {
        return function (m) {
            return Control_Monad_Eff_Class.liftEff(dictMonadAff.MonadEff0())($foreign.writeMessageImpl(c, m));
        };
    };
};
var writeData = function (dictEncode) {
    return function (dictMonadAff) {
        return function (c) {
            return function (d) {
                return Control_Monad_Eff_Class.liftEff(dictMonadAff.MonadEff0())($foreign.writeMessageImpl(c, Data_Foreign_Generic.encodeJSON(dictEncode)(d)));
            };
        };
    };
};
var messageConsumer = function (dictMonadAff) {
    return function (connection) {
        return Control_Monad_Rec_Class.forever(Control_Monad_Free_Trans.monadRecFreeT(Control_Coroutine.functorAwait)((dictMonadAff.MonadEff0()).Monad0()))(Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)((dictMonadAff.MonadEff0()).Monad0()))(Control_Coroutine["await"]((dictMonadAff.MonadEff0()).Monad0()))(function (v) {
            return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorAwait)(((((dictMonadAff.MonadEff0()).Monad0()).Bind1()).Apply0()).Functor0()))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))((dictMonadAff.MonadEff0()).Monad0())(writeMessage(dictMonadAff)(connection)(v)));
        }));
    };
};
var defaultTCPOptions = {
    port: 7777,
    host: "localhost",
    allowHalfOpen: false
};
var dataConsumer = function (dictEncode) {
    return function (dictMonadAff) {
        return function (connection) {
            return Control_Monad_Rec_Class.forever(Control_Monad_Free_Trans.monadRecFreeT(Control_Coroutine.functorAwait)((dictMonadAff.MonadEff0()).Monad0()))(Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)((dictMonadAff.MonadEff0()).Monad0()))(Control_Coroutine["await"]((dictMonadAff.MonadEff0()).Monad0()))(function (v) {
                return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorAwait)(((((dictMonadAff.MonadEff0()).Monad0()).Bind1()).Apply0()).Functor0()))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))((dictMonadAff.MonadEff0()).Monad0())(writeData(dictEncode)(dictMonadAff)(connection)(v)));
            }));
        };
    };
};
var createMessageEmitter = function (connection) {
    return function (emitfunction) {
        var cb = function (s) {
            var $25 = s === "shutdown";
            if ($25) {
                return emitfunction(new Data_Either.Right(Data_Unit.unit));
            };
            return emitfunction(new Data_Either.Left(s));
        };
        return Control_Monad_Eff_Uncurried.runEffFn2($foreign.createMessageEmitterImpl)(connection)(cb);
    };
};
var messageProducer = function (dictMonadAff) {
    return function (connection) {
        return Control_Coroutine_Aff["produce'"](dictMonadAff)(createMessageEmitter(connection));
    };
};
var dataProducer = function (dictDecode) {
    return function (dictMonadAff) {
        return function (dictMonadRec) {
            return function (dictParallel) {
                return function (connection) {
                    return Control_Coroutine.transformProducer(dictMonadRec)(dictParallel)(messageProducer(dictMonadAff)(connection))(Control_Coroutine.transform(dictMonadRec.Monad0())(function ($27) {
                        return Control_Monad_Except.runExcept(Data_Foreign_Generic.decodeJSON(dictDecode)($27));
                    }));
                };
            };
        };
    };
};
var dataProducer_ = function (dictDecode) {
    return function (dictMonadAff) {
        return function (dictMonadRec) {
            return function (dictParallel) {
                return function (connection) {
                    return Control_Coroutine.transformProducer(dictMonadRec)(dictParallel)(messageProducer(dictMonadAff)(connection))(Control_Coroutine.transform(dictMonadRec.Monad0())(function ($28) {
                        return Data_Either.fromRight()(Control_Monad_Except.runExcept(Data_Foreign_Generic.decodeJSON(dictDecode)($28)));
                    }));
                };
            };
        };
    };
};
var createConnectionEmitter = Control_Monad_Eff_Uncurried.runEffFn4($foreign.createConnectionEmitterImpl)(Data_Either.Left.create)(Data_Either.Right.create);
var connectionProducer = function (dictMonadAff) {
    return function (options) {
        return Control_Coroutine_Aff["produce'"](dictMonadAff)(createConnectionEmitter(options));
    };
};
var connectionConsumer = function (dictMonadAff) {
    return function (dictMonadRec) {
        return function (process) {
            return Control_Monad_Rec_Class.forever(Control_Monad_Free_Trans.monadRecFreeT(Control_Coroutine.functorAwait)(dictMonadRec.Monad0()))(Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(dictMonadRec.Monad0()))(Control_Coroutine["await"](dictMonadRec.Monad0()))(function (v) {
                return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorAwait)((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0()))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(dictMonadRec.Monad0())(Control_Coroutine.runProcess(dictMonadRec)(process(v))));
            }));
        };
    };
};
var connectToServer = function (dictMonadAff) {
    return function ($29) {
        return Control_Monad_Eff_Class.liftEff(dictMonadAff.MonadEff0())(Control_Monad_Eff_Uncurried.runEffFn1($foreign.connectToServerImpl)($29));
    };
};
module.exports = {
    defaultTCPOptions: defaultTCPOptions,
    createConnectionEmitter: createConnectionEmitter,
    connectionProducer: connectionProducer,
    connectToServer: connectToServer,
    createMessageEmitter: createMessageEmitter,
    messageProducer: messageProducer,
    writeMessage: writeMessage,
    messageConsumer: messageConsumer,
    connectionConsumer: connectionConsumer,
    dataProducer: dataProducer,
    writeData: writeData,
    dataConsumer: dataConsumer,
    dataProducer_: dataProducer_,
    createConnectionEmitterImpl: $foreign.createConnectionEmitterImpl,
    connectToServerImpl: $foreign.connectToServerImpl,
    createMessageEmitterImpl: $foreign.createMessageEmitterImpl,
    writeMessageImpl: $foreign.writeMessageImpl
};
