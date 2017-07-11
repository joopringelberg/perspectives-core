// module Location
"use strict";

function constructLocation( initial )
{
	var subs = [];
	var val = initial;
	return {
		subscribe: function( sub )
		{
			subs.push( sub );
			sub( val );
		},
		get: function()
		{
			return val;
		},
		set: function( newval )
		{
			val = newval;
			subs.forEach( function( sub )
						  {
							  sub( newval );
						  } );
		}
	};
}

exports.locate = constructLocation;

exports.mapLoc = function( fun )
{
	return function( loc )
	{
		var out = constructLocation( fun( loc.get() ) );
		loc.subscribe( function( val )
					   {
						   out.set( fun( val ) );
					   } );
		return out;
	};
};

exports.applyLoc = function( fun )
{
	return function( loc )
	{
		var out = constructLocation( fun.get()( loc.get() ) );
		var produce = function()
		{
			out.set( fun.get()( loc.get() ) );
		};
		fun.subscribe( produce );
		loc.subscribe( produce );
		return out;
	};
};

exports.runLocation =
	function runLocation( loc )
	{
		return function()
		{
			loc.subscribe( function( val )
						   {
							   val();
						   } );
			return {};
		};
	};

exports.setLocationValue =
	function setLocationValue( loc )
	{
		return function( v )
		{
			return function()
			{
				loc.set( v );
			};
		};
	};
