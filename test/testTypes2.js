"use strict";

exports.parseInt = parseInt;
exports.stringifyInt = function( i )
{
	return i + "";
};
exports.sumIntStrings = function( s1 )
{
	return function( s2 )
	{
		return parseInt( s1 ) + parseInt( s2 ) + ""
	};
};
exports.andBoolStrings = function( s1 )
{
	return function( s2 )
	{
		return (
			   s1 === "true" && s2 === "true"
			   ) + "";
	};
};

function stringToBool( b )
{
	return b === "true";
}

exports.literalMap = function( f )
{
	return function( vs )
	{
		return function( lt )
		{
			toString( f( parse( vs, lt)));
		};
	};
};

function parse( v, lt )
{
	switch( lt.constructor.name )
	{
		case "IntL":
		case "FloatL":
		case "BoolL":
			return JSON.parse( v );
		case "DateL":
			return new Date(v);
		default:
			return v;
	}
}

function toString( v, lt )
{
	switch( lt.constructor.name )
	{
		case "IntL":
		case "FloatL":
		case "BoolL":
			return JSON.stringify( v );
		case "DateL":
			return v.toISOString()
		default:
			return v;
	}
}