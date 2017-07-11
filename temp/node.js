"use strict";
var next = (
	function( count )
	{
		return function next()
		{
			count = count + 1;
			return count;
		};
	}
)( -1 );

exports.getIndex = function( node )
{
	return node.index;
};

exports.createNode = function()
{
	var n = {
		dependents: new Map(),
		supports: [],
		index: next()
	};
	return n;
};

exports.setNode = function(n)
{
	return function(v)
	{
		return function()
		{
			n.location.value0 = v;
			return n;
		}
	}
}

exports.recomputeNode = function(n)
{
	return function()
	{
		var fn, value;
		if ( n.supports.length === 1 )
		{
			fn = n.fn;
			value = n.supports[ 0 ].location.value0
		}
		else
		{
			fn = supports[ 0 ].location.value0;
			value = supports[ 1 ].location.value0;
		}
		// value0 represents value in (Location value node)
		n.location.value0 = fn( value );
		return target;
	}
}

exports.linkNode = function( origins )
{
	return function( fn )
	{
		return function( target )
		{
			target.supports = origins;
			target.fn = fn;
			origins.forEach( function( origin )
							 {
								 origin.dependents.set( fn.toString(), target );
							 } );
			return target;
		};
	};
};

exports.isUndefined = function( v )
{
	return v === undefined;
};

exports.fromUndefined = function( v )
{
	return v;
};

exports.equalNodes = function( n1, n2 )
{
	return n1 === n2;
};
