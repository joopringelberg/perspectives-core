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
		set: function( v )
		{
			n.location.value0 = v;
			return n;
		},
		index: next()
	};
	return n;
};

exports.linkNode = function( origin )
{
	return function( fn )
	{
		return function( target )
		{
			origin.dependents.set( fn.toString(), target );
			target.supports.push( origin );
			target.recompute = function()
			{
				// value0 represents value in (Location value node)
				target.location.value0 = fn( origin.location.value0 );
				return target;
			};
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
