define(function(require, exports, module) {
	"use strict";

	var oop = require("../lib/oop");
	var Mirror = require("../worker/mirror").Mirror;

	var PerspectivesWorker = exports.PerspectivesWorker = function(sender) {
		Mirror.call(this, sender);
		this.setTimeout(200);
	};

	oop.inherits(PerspectivesWorker, Mirror);

	(function() {

		this.onUpdate = function() {
			var value = this.doc.getValue();
			var errors = [];
			this.sender.emit("annotate", errors);
		};

	}).call(PerspectivesWorker.prototype);

});
