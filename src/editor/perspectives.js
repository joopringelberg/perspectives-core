define(function(require, exports, module) {
	"use strict";

	var oop = require("../lib/oop");
	// defines the parent mode
	var TextMode = require("ace/mode/text").Mode;
	var Tokenizer = require("ace/tokenizer").Tokenizer;
	var MatchingBraceOutdent = require("ace/mode/matching_brace_outdent").MatchingBraceOutdent;

	// defines the language specific highlighters and folding rules
	var PerspectivesHighlightRules = require("./perspectives_highlight_rules").PerspectivesHighlightRules;
	var PerspectivesFoldMode = require("./folding/perspectives_folding").PerspectivesFoldMode;
	var uriAndCurie = require("./perspectives/uriAndCurie");

	var Mode = function() {
		// set everything up
		this.HighlightRules = PerspectivesHighlightRules;
		this.$outdent = new MatchingBraceOutdent();
		this.foldingRules = new PerspectivesFoldMode();
	};
	var typeDeclarationRegExp = new RegExp( uriAndCurie.regExpToString( uriAndCurie.resourceName ) + "(\\s+)" + uriAndCurie.regExpToString( uriAndCurie.resourceName ) );
	var roleBindingRegExp = uriAndCurie.composeRegExp( uriAndCurie.propertyName, /(\s+=>)/);

	oop.inherits(Mode, TextMode);


	(function() {
		// configure comment start/end characters
		this.lineCommentStart = "--";
		this.blockComment = {start: "{-", end: "-}"};

		// special logic for indent/outdent.
		// By default ace keeps indentation of previous line
		this.getNextLineIndent = function(state, line, tab)
		{
			/* We indent on:
				- type declaration
				- roleBinding as start of inline type declaration
			 */
			if ( line.match(typeDeclarationRegExp))
			{
				return "\t" + this.$getIndent(line);
			}
			else if (line.match(roleBindingRegExp))
			{
				return "\t" + this.$getIndent(line);
			}
			else {
				return this.$getIndent(line);
			}
		};

		this.checkOutdent = function(state, line, input) {
			return this.$outdent.checkOutdent(line, input);
		};

		this.autoOutdent = function(state, doc, row) {
			this.$outdent.autoOutdent(doc, row);
		};

	}).call(Mode.prototype);

	exports.Mode = Mode;
});