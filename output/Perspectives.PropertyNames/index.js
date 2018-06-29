// Generated by purs version 0.11.7
"use strict";
var Control_Category = require("../Control.Category");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_String = require("../Data.String");
var Data_String_Regex = require("../Data.String.Regex");
var Data_String_Regex_Flags = require("../Data.String.Regex.Flags");
var Data_String_Regex_Unsafe = require("../Data.String.Regex.Unsafe");
var Perspectives_Identifiers = require("../Perspectives.Identifiers");
var Prelude = require("../Prelude");
var domainRangeRuleRegex = Data_String_Regex_Unsafe.unsafeRegex("(.+?)\\_(.+)")(Data_String_Regex_Flags.noFlags);
var decapitalizeWord = Data_String.toLower;
var conformsToDomainRangeRule = function (s) {
    return Data_String_Regex.test(domainRangeRuleRegex)(s);
};
var capitalizeWord = function (word) {
    return Data_String.toUpper(Data_String.take(1)(word)) + Data_String.drop(1)(word);
};
var invertDomainAndRange = function (pn) {
    var namespace = Perspectives_Identifiers.deconstructNamespace(pn);
    var v = Perspectives_Identifiers.deconstructLocalNameFromDomeinURI(pn);
    if (v instanceof Data_Maybe.Just) {
        var $2 = conformsToDomainRangeRule(v.value0);
        if ($2) {
            var part2 = capitalizeWord(Data_Maybe.maybe("")(Control_Category.id(Control_Category.categoryFn))(Perspectives_Identifiers.getFirstMatch(domainRangeRuleRegex)(v.value0)));
            var part1 = decapitalizeWord(Data_Maybe.maybe("")(Control_Category.id(Control_Category.categoryFn))(Perspectives_Identifiers.getSecondMatch(domainRangeRuleRegex)(v.value0)));
            return Data_Maybe.Just.create(Data_Maybe.maybe("")(Control_Category.id(Control_Category.categoryFn))(namespace) + (part1 + ("_" + part2)));
        };
        return Data_Maybe.Just.create(Data_Maybe.maybe("")(Control_Category.id(Control_Category.categoryFn))(namespace) + ("inverse_" + v.value0));
    };
    if (v instanceof Data_Maybe.Nothing) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Perspectives.PropertyNames line 41, column 10 - line 50, column 20: " + [ v.constructor.name ]);
};
var getInversePropertyName = invertDomainAndRange;
module.exports = {
    getInversePropertyName: getInversePropertyName,
    capitalizeWord: capitalizeWord,
    decapitalizeWord: decapitalizeWord,
    domainRangeRuleRegex: domainRangeRuleRegex,
    conformsToDomainRangeRule: conformsToDomainRangeRule,
    invertDomainAndRange: invertDomainAndRange
};
