define( function( require, exports, module ) {
	"use strict";

	var oop = require( "../lib/oop" );
	var TextHighlightRules = require( "./text_highlight_rules" ).TextHighlightRules;
	var uriAndCurie = require("./perspectives/uriAndCurie");

	var PerspectivesHighlightRules = function() {

		// regexp must not have capturing parentheses. Use (?:) instead.
		// regexps are ordered -> the first match is used
		this.$rules = {
			"start": [
				/////////////// BOOLEAN, STRING, NUMBER
				{
					token: "constant.language.boolean",
					regex: "(?:true|false)\\b"
				}, {
					token : "constant.numeric", // float
					regex : "[+-]?\\d+(?:(?:\\.\\d*)?(?:[eE][+-]?\\d+)?)?\\b"
				}, {
					token : "string", // single line
					regex : '"',
					next  : "string"
				},
				{
					token: "constant.language",
					regex: /(Number|String|Date|Boolean|SimpleValue)/
				},
				// TODO: date.
				/////////////// KEYWORDS
				// TODO: echte resource identifier match!
				{
					// public :urgentie = 7;
					token: ["keyword", "variable"],
					regex: uriAndCurie.composeRegExp( /(public\b\s+)/, uriAndCurie.propertyName )
				},
				{
					// private :urgentie = 7;
					token: ["keyword", "variable"],
					regex: uriAndCurie.composeRegExp( /(private\b\s+)/, uriAndCurie.propertyName )
				},
				{
					// query :aangever binding
					token: ["keyword"],
					regex: /query\b/
				},
				{
					// Context
					token: ["keyword"],
					regex: /^Context\b/
				},
				{
					// import
					token: ["keyword"],
					regex: /^import\b/
				},
				{
					// Section
					token: ["keyword"],
					regex: /^Section\b/
				},
				{
					// als
					token: ["keyword"],
					regex: /als\b/
				},
				/////////////// ROLPROPERTY ASSIGNMENT
				{
					token: ["variable", "text"],
					regex: uriAndCurie.composeRegExp( uriAndCurie.propertyName, /(\s*=)/ )
				},
				/////////////// TYPEDECLARATION, BINDING
				{
					// contextDeclaration: :Aangifte :a1
					token: ["type", "text", "text"],
					regex: new RegExp( uriAndCurie.regExpToString( uriAndCurie.contextName ) + "(\\s+)" + uriAndCurie.regExpToString( uriAndCurie.contextName ) )
				},
				{
					// rolebinding: :role => :Binding
					token: ["variable", "text"],
					regex: uriAndCurie.composeRegExp( uriAndCurie.propertyName, /(\s+=>)/)
				},
				/////////////// COMMENT
				{
					token : "comment",
					regex : /(--.*$)/
				},
				{
					token: "comment",
					regex: /({-.*)/,
					next: "multiLineComment"
				}
			],
			"string" : [
				{
					token : "constant.language.escape",
					regex : /\\(?:x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|["\\\/bfnrt])/
				}, {
					token : "string",
					regex : '"|$',
					next  : "start"
				}, {
					defaultToken : "string"
				}
			],
			"multiLineComment": [
				{
					token: "comment",
					regex: /(.*-})/,
					next: "start"
				},
				{
					token: "comment",
					regex: /(.*)/
				}
			]
		};
	};

	oop.inherits( PerspectivesHighlightRules, TextHighlightRules );

	exports.PerspectivesHighlightRules = PerspectivesHighlightRules;

	exports.LispHighlightRules = PerspectivesHighlightRules;

} );