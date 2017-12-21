define( function( require, exports, module ) {
	"use strict";

	// RegExp -> RegExp -> RegExp
	function composeRegExp(r1, r2)
	{

		return new RegExp( regExpToString(r1) + regExpToString(r2) );
	}

	function catchingRegexp(r)
	{
		return new RegExp( "(" + regExpToString(r) + ")" );
	}

	function disjunctiveRegExp(r1, r2)
	{
		return new RegExp( regExpToString(r1) + "|" + regExpToString(r2));
	}

	// RegExp -> String
	// Returns all between //, ignoring flags.
	function regExpToString(r)
	{
		return r.toString().match(/\/(.+)(?=\/)/)[1];
	}

	var uncapitalizedString = new RegExp(/[a-z]\w*\b/);

	var capitalizedString = new RegExp(/[A-Z]\w*\b/);

	var localResourceName = capitalizedString;

	var localPropertyName = uncapitalizedString;

	var domeinName = new RegExp( "model:" + regExpToString(capitalizedString) + "\\$" );

	var prefix = new RegExp(/[a-z0-9]*:/);

	var prefixedResourceName = composeRegExp( prefix, localResourceName );

	var prefixedPropertyName = composeRegExp( prefix, localPropertyName );

	var qualifiedResourceName = composeRegExp( domeinName, localResourceName );

	var qualifiedPropertyName = composeRegExp( domeinName, localPropertyName );

	var resourceName = catchingRegexp( disjunctiveRegExp( qualifiedResourceName, prefixedResourceName ) );

	var propertyName = catchingRegexp( disjunctiveRegExp( qualifiedPropertyName, prefixedPropertyName ) );

	exports.resourceName = resourceName;

	exports.propertyName = propertyName;

	exports.composeRegExp = composeRegExp;

	exports.regExpToString = regExpToString;

	exports.disjunctiveRegExp = disjunctiveRegExp;
});
