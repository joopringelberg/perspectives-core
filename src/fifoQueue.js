/**
 * Created by joopringelberg on 05-07-17.
 */
"use strict";

function Queue(arr)
{
	this.queue = arr;
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

Queue.prototype.next = function()
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

exports.queue = function(arr)
{
	return new Queue(arr);
};

exports.appendToEnd = function(q)
{
	return function(x)
	{
		q.appendToEnd(x);
	};
};

exports.next = function(q)
{
	return q.next();
};

exports.empty = function(q)
{
	return q.empty();
};

exports.cumulator = function(q)
{
	return q.queue;
};