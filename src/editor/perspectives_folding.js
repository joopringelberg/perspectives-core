define(function(require, exports, module) {
	"use strict";

	var oop = require("../../lib/oop");
	var Range = require("../../range").Range;
	var BaseFoldMode = require("./fold_mode").FoldMode;

	var FoldMode = exports.FoldMode = function() {};
	oop.inherits(FoldMode, BaseFoldMode);

	(function() {

		// regular expressions that identify starting and stopping points
		this.foldingStartMarker = /^(\{)\-/;
		this.foldingStopMarker = /^\-(\})/;

		this.getFoldWidgetRange = function(session, foldStyle, row) {
			var line = session.getLine(row);

			// test each line, and return a range of segments to collapse
			var match = line.match(this.foldingStartMarker);
			if (match) {
				var i = match.index;

				if (match[1])
				{
					return this.openingBracketBlock(session, match[1], row, i);
				}

				var range = session.getCommentFoldRange(row, i + match[0].length);
				range.end.column -= 2;
				return range;
			}
		};

	}).call(FoldMode.prototype);

	exports.PerspectivesFoldMode = exports.FoldMode;
});