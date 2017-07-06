/**
 * Created by joopringelberg on 05-07-17.
 */
"use strict";

function Queue()
{
	this.queue = [];
	this.index = -1;
}

Queue.prototype = Object.create( {}, {} );

Queue.prototype.appendToEnd = function( a )
{
	a.forEach(
		function( x )
		{
			this.queue.push( x );
			return this.queue;
		}
	);
};

Queue.prototype.popFromFront = function()
{
	if ( this.index < this.queue.length )
	{
		this.index = this.index + 1;
		return this.queue[ this.index ];
	}
};

Queue.prototype.empty = function()
{
	this.index >= this.queue.length;
};

exports.queue = function()
{
	return new Queue();
};

exports.appendToEnd = function(q)
{
	return function(x)
	{
		q.appendToEnd(x);
	};
};

exports.popFromFront = function(q)
{
	return q.popFromFront();
};

exports.empty = function(q)
{
	return q.empty();
};
