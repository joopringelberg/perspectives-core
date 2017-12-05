define(function(require, exports, module) {
	"use strict";

	var oop = require("../../lib/oop");
	var Range = require("../../range").Range;
	var BaseFoldMode = require("./fold_mode").FoldMode;

	var FoldMode = exports.FoldMode = function() {};
	var uriAndCurie = require("../perspectives/uriAndCurie");

	var typeDeclarationRegExp = new RegExp( uriAndCurie.regExpToString( uriAndCurie.resourceName ) + "(\\s+)" + uriAndCurie.regExpToString( uriAndCurie.resourceName ) );
	var multiLineCommentStartRegExp = /^({)-/


			oop.inherits(FoldMode, BaseFoldMode);

	(function() {

		// regular expressions that identify starting and stopping points
		this.foldingStartMarker = uriAndCurie.disjunctiveRegExp( multiLineCommentStartRegExp, typeDeclarationRegExp);
		this.foldingStopMarker = /^-(})/;

		this.getFoldWidgetRange = function(session, foldStyle, row) {
			var line = session.getLine(row),
				range,
				i;

			// We fold multiline comments
			var match = line.match(multiLineCommentStartRegExp);
			if (match) {
				i = match.index;

				if (match[1])
				{
					return this.openingBracketBlock(session, match[1], row, i);
				}

				range = session.getCommentFoldRange(row, i + match[0].length);
				range.end.column -= 2;
				return range;
			}
			// We also fold on type declarations.
			else if ( line.match( typeDeclarationRegExp ))
			{
				// Find the next line that outdents. By providing zero as starting column, the function indentationBlock will take the line length as start column.
				return this.indentationBlock(session, row, 0 );
			}
		};

	}).call(FoldMode.prototype);

	exports.PerspectivesFoldMode = exports.FoldMode;
});