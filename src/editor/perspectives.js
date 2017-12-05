define(function(require, exports, module) {
	"use strict";

	var oop = require("../lib/oop");
	// defines the parent mode
	var TextMode = require("ace/mode/text").Mode;
	var Tokenizer = require("ace/tokenizer").Tokenizer;
	var MatchingBraceOutdent = require("ace/mode/matching_brace_outdent").MatchingBraceOutdent;
	var WorkerClient = require("ace/worker/worker_client").WorkerClient;

	// defines the language specific highlighters and folding rules
	var PerspectivesHighlightRules = require("./perspectives_highlight_rules").PerspectivesHighlightRules;
	var PerspectivesFoldMode = require("./folding/perspectives_folding").PerspectivesFoldMode;

	var Mode = function() {
		// set everything up
		this.HighlightRules = PerspectivesHighlightRules;
		this.$outdent = new MatchingBraceOutdent();
		this.foldingRules = new PerspectivesFoldMode();
	};
	oop.inherits(Mode, TextMode);

	(function() {
		// configure comment start/end characters
		this.lineCommentStart = "--";
		this.blockComment = {start: "{-", end: "-}"};

		// special logic for indent/outdent.
		// By default ace keeps indentation of previous line
		this.getNextLineIndent = function(state, line, tab) {
			var indent = this.$getIndent(line);
			return indent;
		};

		this.checkOutdent = function(state, line, input) {
			return this.$outdent.checkOutdent(line, input);
		};

		this.autoOutdent = function(state, doc, row) {
			this.$outdent.autoOutdent(doc, row);
		};

		// create worker for live syntax checking
		this.createWorker = function(session) {
			var worker = new WorkerClient(["ace"], "ace/mode/perspectives_worker", "PerspectivesWorker");
			worker.attachToDocument(session.getDocument());
			worker.on("errors", function(e) {
				session.setAnnotations(e.data);
			});
			return worker;
		};

	}).call(Mode.prototype);

	exports.Mode = Mode;
});