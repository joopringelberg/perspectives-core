// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Data_Either = require("../Data.Either");
var Data_Identity = require("../Data.Identity");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_These = require("../Data.These");
var Prelude = require("../Prelude");
var Text_Parsing_Parser = require("../Text.Parsing.Parser");
var Text_Parsing_Parser_Combinators = require("../Text.Parsing.Parser.Combinators");
var URI_Common = require("../URI.Common");
var URI_Host = require("../URI.Host");
var URI_Port = require("../URI.Port");
var print = function (printHost) {
    return function (printPort) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return "";
            };
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_These.This) {
                return URI_Host.print(printHost(v.value0.value0));
            };
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_These.That) {
                return URI_Port.print(printPort(v.value0.value0));
            };
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_These.Both) {
                return URI_Host.print(printHost(v.value0.value0)) + URI_Port.print(printPort(v.value0.value1));
            };
            throw new Error("Failed pattern match at URI.HostPortPair line 58, column 29 - line 66, column 62: " + [ v.constructor.name ]);
        };
    };
};
var parser = function (parseHost) {
    return function (parsePort) {
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(URI_Common.wrapParser(Data_Identity.monadIdentity)(parseHost)(URI_Host.parser)))(function (v) {
            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(URI_Common.wrapParser(Data_Identity.monadIdentity)(parsePort)(URI_Port.parser)))(function (v1) {
                return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))((function () {
                    if (v instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Nothing) {
                        return new Data_Maybe.Just(new Data_These.This(v.value0));
                    };
                    if (v instanceof Data_Maybe.Nothing && v1 instanceof Data_Maybe.Just) {
                        return new Data_Maybe.Just(new Data_These.That(v1.value0));
                    };
                    if (v instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Just) {
                        return new Data_Maybe.Just(new Data_These.Both(v.value0, v1.value0));
                    };
                    if (v instanceof Data_Maybe.Nothing && v1 instanceof Data_Maybe.Nothing) {
                        return Data_Maybe.Nothing.value;
                    };
                    throw new Error("Failed pattern match at URI.HostPortPair line 41, column 8 - line 45, column 31: " + [ v.constructor.name, v1.constructor.name ]);
                })());
            });
        });
    };
};
module.exports = {
    parser: parser,
    print: print
};
