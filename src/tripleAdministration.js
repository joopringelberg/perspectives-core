"use strict";

exports.addDependency = function( triple ) {
	return function( tripleRef ) {
		return function() {
			// TODO: alleen toevoegen indien niet aanwezig!
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
