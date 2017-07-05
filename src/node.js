
var next = (function(count)
{
  return function next()
  {
    count = count + 1;
    return count;
  };
})(-1)


exports.getIndex = function( node )
{
  return node.index;
}

exports.createNode = function( location )
{
	return {
		location: location,
		dependents: new Map(),
		origins: [],
    index: next()
	};
};

exports.linkNode = function( origin )
{
  return function( fn )
  {
    var target = { dependents: new Map(), supports: [ origin ], index: next() };
    origin.dependents.set( fn.toString(), target );
    return target;
  };
};

exports.isUndefined = function(v)
{
  return v === undefined;
}

exports.fromUndefined = function(v)
{
  return v;
}
