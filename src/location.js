// module Location
"use strict";

var queue = [];

/**
 *
 * @param {Iterator} candidates
 */
function addToQueue( queue, candidates )
{
	candidates
		.filter(
			function( c )
			{
				return queue.indexOf( c ) < 0;
			} )
		.sort( compareLocations )
		.forEach(
			function( c )
			{
				queue.push( c );
			}
		);
}

exports.propagateTheoryDeltas = function()
{
	var q = queue, next;
	queue = [];
	while( q.length > 0 )
	{
		next = q.shift();
		next.recomputeNode();
		addToQueue( q, next.dependents );
	}
};

function compareLocations( l1, l2 )
{
	if( l1.dependents.indexOf( l2 ) >= 0 )
	{
		return -1;
	}
	else if( l2.dependents.indexOf( l1 ) >= 0 )
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

function Location( fun )
{
	this._value = fun();
	this.fun = fun;
	this._dependents = new Map();
	// Holds the function if rightSupport holds the value to which it should be applied.
	this.leftSupport = undefined;
	this.rightSupport = undefined;
}

Location.prototype = Object.create(
	{},
	{
		dependents: {
			get: function()
			{
				return Array.from( this._dependents.values() );
			}
		}
	} );

Location.prototype.getDependent = function( linkName )
{
	return this._dependents.get( linkName );
};

Location.prototype.addLeftLeaningDependent = function( linkName, dep )
{
	this._dependents.set( linkName, dep );
	dep.leftSupport = this;
};

Location.prototype.addRightLeaningDependent = function( linkName, dep )
{
	this._dependents.set( linkName, dep );
	dep.rightSupport = dep;
};

Location.prototype.onChange = function( handler )
{
	if( !this.handlers )
	{
		this.handlers = [];
	}
	this.handlers.push( handler );
};

Location.prototype.get = function()
{
	return this._value;
};

Location.prototype.set = function( nv )
{
	this._value = nv;
	addToQueue( queue, this.dependents );
	this.handlers.forEach(
		function( h )
		{
			h( nv );
		} );
};

Location.prototype.updateValue = function( nv )
{
	this._value = nv;
	if ( this.handlers )
	{
		this.handlers.forEach(
			function( h )
			{
				h( nv );
			} );
	}
};

Location.prototype.recomputeNode = function()
{
	this.updateValue( this.fun() );
};

exports.locate = function( v )
{
	function constant()
	{
		return v;
	}
	return new Location( constant );
};

exports.mapLoc = function( fun )
{
	return function( loc )
	{
		// Note: toString does not give a unique value for each function!
		var linkName = fun.toString();
		var out = loc.getDependent( linkName );
		var produce = function()
		{
			return fun( loc.get() );
		};
		if( !out )
		{
			out = new Location( produce );
			/*
			loc ---dependent--> out
			loc <--leftSupport--- out
			 */
			loc.addLeftLeaningDependent( linkName, out );
		}
		return out;
	};
};

exports.applyLoc = function( fun )
{
	return function( loc )
	{
		// Note: toString does not give a unique value for each function!
		var linkName = fun.get().toString();
		var out = fun.getDependent( linkName );
		var produce = function()
		{
			return fun.get()( loc.get() );
		};
		if( !out )
		{
			out = new Location( produce );
			/*
			fun ---dependent--> out
			fun <--leftSupport--- out

			loc ---dependent--> out
			loc <--rightSupport--- out
			 */
			fun.addRightLeaningDependent( linkName, out, loc );
		}
		return out;
	};
};

exports.getLoc = function( loc )
{
	return loc.get();
};

exports.runLocation =
	function runLocation( loc )
	{
		return function()
		{
			/*
			 'loc' will contain effectful computations. As soon as a new such computation is set in 'loc', we run it.
			 */
			loc.onChange( function( val )
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

exports.runTHEORYDELTA = function( f )
{
	return f;
};
