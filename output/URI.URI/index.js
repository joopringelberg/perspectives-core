// Generated by purs version 0.11.7
"use strict";
var Control_Apply = require("../Control.Apply");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Identity = require("../Data.Identity");
var Data_Lens = require("../Data.Lens");
var Data_Lens_Lens = require("../Data.Lens.Lens");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_Symbol = require("../Data.Symbol");
var Prelude = require("../Prelude");
var Text_Parsing_Parser = require("../Text.Parsing.Parser");
var Text_Parsing_Parser_Combinators = require("../Text.Parsing.Parser.Combinators");
var Text_Parsing_Parser_String = require("../Text.Parsing.Parser.String");
var URI_Common = require("../URI.Common");
var URI_Fragment = require("../URI.Fragment");
var URI_HierarchicalPart = require("../URI.HierarchicalPart");
var URI_Query = require("../URI.Query");
var URI_Scheme = require("../URI.Scheme");
var URI = (function () {
    function URI(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    URI.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new URI(value0, value1, value2, value3);
                };
            };
        };
    };
    return URI;
})();
var print = function (opts) {
    return function (v) {
        return Data_String.joinWith("")(Data_Array.catMaybes([ new Data_Maybe.Just(URI_Scheme.print(v.value0)), new Data_Maybe.Just(URI_HierarchicalPart.print(opts)(v.value1)), Data_Functor.map(Data_Maybe.functorMaybe)(function ($143) {
            return URI_Query.print(opts.printQuery($143));
        })(v.value2), Data_Functor.map(Data_Maybe.functorMaybe)(function ($144) {
            return URI_Fragment.print(opts.printFragment($144));
        })(v.value3) ]));
    };
};
var parser = function (opts) {
    return Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Apply.apply(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Apply.apply(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Apply.apply(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(URI.create)(URI_Scheme.parser))(URI_HierarchicalPart.parser(opts)))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(URI_Common.wrapParser(Data_Identity.monadIdentity)(opts.parseQuery)(URI_Query.parser))))(Text_Parsing_Parser_Combinators.optionMaybe(Data_Identity.monadIdentity)(URI_Common.wrapParser(Data_Identity.monadIdentity)(opts.parseFragment)(URI_Fragment.parser))))(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity));
};
var genericURI = new Data_Generic_Rep.Generic(function (x) {
    return new Data_Generic_Rep.Product(x.value0, new Data_Generic_Rep.Product(x.value1, new Data_Generic_Rep.Product(x.value2, x.value3)));
}, function (x) {
    return new URI(x.value0, x.value1.value0, x.value1.value1.value0, x.value1.value1.value1);
});
var showURI = function (dictShow) {
    return function (dictShow1) {
        return function (dictShow2) {
            return function (dictShow3) {
                return function (dictShow4) {
                    return function (dictShow5) {
                        return new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericURI)(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsProduct(Data_Generic_Rep_Show.genericShowArgsArgument(URI_Scheme.showScheme))(Data_Generic_Rep_Show.genericShowArgsProduct(Data_Generic_Rep_Show.genericShowArgsArgument(URI_HierarchicalPart.showHierarchicalPart(dictShow)(dictShow1)(dictShow2)(dictShow3)))(Data_Generic_Rep_Show.genericShowArgsProduct(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Maybe.showMaybe(dictShow4)))(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Maybe.showMaybe(dictShow5))))))(new Data_Symbol.IsSymbol(function () {
                            return "URI";
                        }))));
                    };
                };
            };
        };
    };
};
var eqURI = function (dictEq) {
    return function (dictEq1) {
        return function (dictEq2) {
            return function (dictEq3) {
                return function (dictEq4) {
                    return function (dictEq5) {
                        return new Data_Eq.Eq(function (x) {
                            return function (y) {
                                return Data_Eq.eq(URI_Scheme.eqScheme)(x.value0)(y.value0) && Data_Eq.eq(URI_HierarchicalPart.eqHierarchicalPart(dictEq)(dictEq1)(dictEq2)(dictEq3))(x.value1)(y.value1) && Data_Eq.eq(Data_Maybe.eqMaybe(dictEq4))(x.value2)(y.value2) && Data_Eq.eq(Data_Maybe.eqMaybe(dictEq5))(x.value3)(y.value3);
                            };
                        });
                    };
                };
            };
        };
    };
};
var ordURI = function (dictOrd) {
    return function (dictOrd1) {
        return function (dictOrd2) {
            return function (dictOrd3) {
                return function (dictOrd4) {
                    return function (dictOrd5) {
                        return new Data_Ord.Ord(function () {
                            return eqURI(dictOrd.Eq0())(dictOrd1.Eq0())(dictOrd2.Eq0())(dictOrd3.Eq0())(dictOrd4.Eq0())(dictOrd5.Eq0());
                        }, function (x) {
                            return function (y) {
                                var v = Data_Ord.compare(URI_Scheme.ordScheme)(x.value0)(y.value0);
                                if (v instanceof Data_Ordering.LT) {
                                    return Data_Ordering.LT.value;
                                };
                                if (v instanceof Data_Ordering.GT) {
                                    return Data_Ordering.GT.value;
                                };
                                var v1 = Data_Ord.compare(URI_HierarchicalPart.ordHierarchicalPart(dictOrd)(dictOrd1)(dictOrd2)(dictOrd3))(x.value1)(y.value1);
                                if (v1 instanceof Data_Ordering.LT) {
                                    return Data_Ordering.LT.value;
                                };
                                if (v1 instanceof Data_Ordering.GT) {
                                    return Data_Ordering.GT.value;
                                };
                                var v2 = Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd4))(x.value2)(y.value2);
                                if (v2 instanceof Data_Ordering.LT) {
                                    return Data_Ordering.LT.value;
                                };
                                if (v2 instanceof Data_Ordering.GT) {
                                    return Data_Ordering.GT.value;
                                };
                                return Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd5))(x.value3)(y.value3);
                            };
                        });
                    };
                };
            };
        };
    };
};
var _scheme = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value0;
    })(function (v) {
        return function (s) {
            return new URI(s, v.value1, v.value2, v.value3);
        };
    })(dictStrong);
};
var _query = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value2;
    })(function (v) {
        return function (q) {
            return new URI(v.value0, v.value1, q, v.value3);
        };
    })(dictStrong);
};
var _hierPart = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value1;
    })(function (v) {
        return function (h) {
            return new URI(v.value0, h, v.value2, v.value3);
        };
    })(dictStrong);
};
var _fragment = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value3;
    })(function (v) {
        return function (f) {
            return new URI(v.value0, v.value1, v.value2, f);
        };
    })(dictStrong);
};
module.exports = {
    URI: URI,
    parser: parser,
    print: print,
    _scheme: _scheme,
    _hierPart: _hierPart,
    _query: _query,
    _fragment: _fragment,
    eqURI: eqURI,
    ordURI: ordURI,
    genericURI: genericURI,
    showURI: showURI
};
