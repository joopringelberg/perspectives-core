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

	function nonCatchingGroup(r)
	{
		return new RegExp( "(?:" + regExpToString(r) + ")" );
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

	var defaultEmbedded = new RegExp(/\$/);

	var localContextName = new RegExp( regExpToString(capitalizedString) + "(?:\\$" + regExpToString(capitalizedString) + ")*" );

	var localPropertyName = uncapitalizedString;

	var domeinName = new RegExp( "model:" + regExpToString(capitalizedString) + "\\$" );

	var prefix = new RegExp(/[a-z0-9]+:/);

	var prefixedContextName = composeRegExp( prefix, localContextName );

	var prefixedPropertyName = composeRegExp( prefix, localPropertyName );

	var qualifiedContextName = composeRegExp( domeinName, localContextName );

	var qualifiedPropertyName = composeRegExp( domeinName, localPropertyName );

	var contextName = catchingRegexp( disjunctiveRegExp( qualifiedContextName, disjunctiveRegExp( prefixedContextName, new RegExp( "\\$" + regExpToString( localContextName ) ) ) ) );

	var propertyName = catchingRegexp( disjunctiveRegExp( qualifiedPropertyName, disjunctiveRegExp(prefixedPropertyName, new RegExp( "\\$" + regExpToString( localPropertyName ) ) ) ) );

	exports.contextName = contextName;

	exports.propertyName = propertyName;

	exports.composeRegExp = composeRegExp;

	exports.regExpToString = regExpToString;

	exports.disjunctiveRegExp = disjunctiveRegExp;
});
