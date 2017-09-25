// module Location
"use strict";

/**
 *
 * @param {Iterator} candidates
 */
function addToQueue( queue, candidates )
{
	candidates
		.filter(
			function( c ) {
				return queue.indexOf( c ) < 0;
			} )
		.sort( compareLocations )
		.forEach(
			function( c ) {
				queue.push( c );
			}
		);
}

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

function Location( fun, name )
{
	this._value = fun();
	this.fun = fun;
	this.locName = name;
	this._dependents = {};
	this._supports = [];
}

Location.prototype = Object.create(
	{},
	{
		dependents: {
			get: function() {
				var r = [];
				var dependents = this._dependents;
				dependents.keys().forEach(
					function( key ) {
						r.push( dependents[ key ] );
					}
				);
				return r;
			}
		}
	} );

Location.prototype.getDependent = function( linkName ) {
	return this._dependents[ linkName ];
};

Location.prototype.addDependent = function( linkName, dep ) {
	this._dependents[ linkName ] = dep;
	dep._supports.push( this );
};

// NOTE: add dependents first, then remove. On removing all dependents, the location will self-destruct!
Location.prototype.removeDependent = function( linkName ) {
	var self = this;
	var dep = this.getDependent( linkName );
	delete this._dependents[ linkName ];
	dep.removeSupport( this );
	if( this._dependents.keys().length === 0 )
	{
		// This location has no further use. Disconnect from supports.
		this._supports.forEach( function( s ) {
			s.removeDependent( self );
		} );
	}
};

Location.prototype.removeSupport = function( support ) {
	this.supports.splice( this.supports.indexOf( support ), 1 );
};

Location.prototype.onChange = function( handler ) {
	if( !this.handlers )
	{
		this.handlers = [];
	}
	this.handlers.push( handler );
};

Location.prototype.get = function() {
	return this._value;
};

Location.prototype.updateValue = function( nv ) {
	this._value = nv;
	if( this.handlers )
	{
		this.handlers.forEach(
			function( h ) {
				h( nv );
			} );
	}
};

Location.prototype.recomputeNode = function() {
	this.updateValue( this.fun() );
};

Location.prototype.toJSON = function(){
	return this.id;
};

var propagateTheoryDeltas;

(
	function( queue ) {
		propagateTheoryDeltas = function() {
			var q = queue;
			var next;
			queue = [];
			while( q.length > 0 )
			{
				next = q.shift();
				next.recomputeNode();
				addToQueue( q, next.dependents );
			}
		};

		Location.prototype.set = function( nv ) {
			this._value = nv;
			addToQueue( queue, this.dependents );
			if( this.handlers )
			{
				this.handlers.forEach(
					function( h ) {
						h( nv );
					} );
			}
		};
	}( [] )
);

exports.locate = function( v ) {
	return new Location( function() {
		return v;
	}, valueName( v ) );
};

/*
 * Note that this function depends on the representation of ADT's by the purescript compiler.
 */
function valueName( v )
{
	if( Array.isArray( v ) )
	{
		return v.map( valueName ).toString();
	}
	else
	{
		switch( typeof v )
		{
			case "object":
				return JSON.stringify(v, ["value0", "value1", "value2", "value3", "id"]);
			case "function":
				return v.name;
			default:
				return v.toString();
		}
	}
}

exports.connectLocations = function( loc ) {
	return function( fun ) {
		return function( dependent ) {
			var linkName = fun.name + "_" + loc.locName;
			loc.addDependent( linkName, dependent );
			return dependent;
		};
	};
};

exports.mapLoc = function( fun ) {
	return function( loc ) {
		// A function can only be applied once to a location.
		var linkName = fun.name + "<$>" + loc.locName;
		var dependent = loc.getDependent( linkName );
		if( !dependent )
		{
			dependent = new Location(
				nameFunction( fun.name, function() {
					return fun( loc.get() );
				}),
				linkName );
			loc.addDependent( linkName, dependent );
		}
		return dependent;
	};
};

exports.applyLoc = function( funLoc ) {
	return function( loc ) {
		// Apply can be executed only once for the combination of funLoc and loc.
		var linkName = funLoc.locName + "<*>" + loc.locName;
		var dependent = funLoc.getDependent( linkName );
		if( !dependent )
		{
			dependent = new Location(
				nameFunction( funLoc.locName, function() {
					return funLoc.get()( loc.get() );
				}),
				linkName );
			funLoc.addDependent( linkName, dependent );
			loc.addDependent( linkName, dependent );
		}
		return dependent;
	};
};

exports.bindLoc = function( loc ) {
	return function( fun ) {
		var linkName = loc.locName + ">>=" + fun.name;
		var dependent = loc.getDependent( linkName );
		if( !dependent )
		{
			// Dependent will be produced by fun:
			dependent = fun( loc.get() );
			// The function set in dependent will perform the update necessary for bind.
			dependent.fun = nameFunction( fun.name, function() {
				var newLocWithValue = fun( loc.get() );
				// Move the dependents of dependent to newLocWithValue.
				dependent._dependents.keys().forEach(
					function( linkName ) {
						newLocWithValue.addDependent( linkName, dependent.getDependent( linkName ) );
						dependent.removeDependent( linkName );
					}
				);
				// Return the content of the new location; it will be inserted into dependent by the recomputeNode function.
				return newLocWithValue.get();
			} );
			loc.addDependent( linkName, dependent );
		}
		return dependent;
	};
};

exports.connectLocationsAsInBind = function( loc ) {
	return function( fun ) {
		return function( dependent ) {
			var linkName = loc.locName + ">>=" + fun.name;
			loc.addDependent( linkName, dependent );
			// The function set in dependent will perform the update necessary for bind.
			dependent.fun = nameFunction( fun.name, function() {
				var newLocWithValue = fun( loc.get() );
				// Move the dependents of dependent to newLocWithValue.
				dependent._dependents.keys().forEach(
					function( linkName ) {
						newLocWithValue.addDependent( linkName, dependent.getDependent( linkName ) );
						dependent.removeDependent( linkName );
					}
				);
				// Return the content of the new location; it will be inserted into dependent by the recomputeNode function.
				return newLocWithValue.get();
			} );
			return dependent;
		};
	};
};

exports.locationValue = function( loc ) {
	return loc.get();
};

exports.locationDependentAux = function( f ) {
	return function( loc ) {
		return loc.getDependent( f.name );
	};
};

exports.nameFunction = function( name ) {
	return function( f ) {
		return nameFunction( name, f );
	};
};

function nameFunction( name, f )
{
	Object.defineProperty( f, 'name', { value: name } );
	return f;
}

exports.functionName = function( f ) {
	return f.name;
};

exports.runLocation =
	function runLocation( loc ) {
		return function() {
			/*
			 'loc' will contain effectful computations. As soon as a new such computation is set in 'loc', we run it.
			 */
			loc.onChange( function( val ) {
				val();
			} );
			return {};
		};
	};

exports.setLocationValue =
	function setLocationValue( loc ) {
		return function( v ) {
			return function() {
				loc.set( v );
			};
		};
	};

exports.runTHEORYDELTA = function( f ) {
	return function() {
		f();
		propagateTheoryDeltas();
	};
};
