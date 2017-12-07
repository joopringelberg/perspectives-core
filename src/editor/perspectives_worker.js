define(function(require, exports, module) {
	"use strict";

	var oop = require("../lib/oop");
	var Mirror = require("../worker/mirror").Mirror;
	var parse = require("./parser").parse;

	var PerspectivesWorker = exports.PerspectivesWorker = function(sender) {
		Mirror.call(this, sender);
		this.setTimeout(200);
	};

	oop.inherits(PerspectivesWorker, Mirror);

	(function() {

		this.onUpdate = function() {
			var perspectText = this.doc.getValue();
			var errors = parse( perspectText );
			console.log(errors)
			// errors.push({
             //        row: loc.first_line,
             //        column: loc.first_column,
             //        endRow: loc.last_line,
             //        endColumn: loc.last_column,
             //        text: e.message,
             //        type: "error"
             //    });
			this.sender.emit("annotate", errors);
		};

	}).call(PerspectivesWorker.prototype);

});
