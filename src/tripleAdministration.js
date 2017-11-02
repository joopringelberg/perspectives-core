"use strict";

exports.addDependency = function( triple ) {
	return function( tripleRef ) {
		return function() {
			if( !triple.dependencies.find( function( ref ) {
					return ref.subject === tripleRef.subject && ref.predicate === tripleRef.predicate;
				} ) )
			{
				triple.dependencies.push( tripleRef );
			}
			return tripleRef;
		};
	};
};

exports.removeDependency = function( triple ) {
	return function( tripleRef ) {
		return function() {
			var index = triple.dependencies.findIndex( function( ref ) {
					return ref.subject === tripleRef.subject && ref.predicate === tripleRef.predicate;
				} );
			triple.dependencies.splice( index, 1 );
			return tripleRef;
		};
	};
};

exports.setSupports = function( triple )
{
	return function(supports)
	{
		return function()
		{
			triple.supports = supports;
			return {}
		}
	}
};
