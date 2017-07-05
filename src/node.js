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

exports.linkNode = function( origins )
{
	return function( fn )
	{
		return function( target )
		{
			target.supports = origins;
			origins.forEach( function( origin )
							 {
								 origin.dependents.set( fn.toString(), target );
							 } );
			if( origins.length === 1 )
			{
				target.recompute = function()
				{
					// value0 represents value in (Location value node)
					target.location.value0 = fn( origins[ 0 ].location.value );
					return target;
				};
			}
			else
			{
				target.recompute = function()
				{
					var fn = origins[ 0 ].location.value0;
					var value = origins[ 1 ].location.value0;
					target.location.value0 = fn( value );
					return target;
				};
			}
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
