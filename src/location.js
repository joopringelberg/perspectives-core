exports.saveLocation = function(location)
{
  return function(node)
  {
    node.location = location;
    return location;
  };
};

exports.retrieveLocation = function(node)
{
  return function(fn)
  {
    const n = node.dependents.get(fn.toString());
    if ( n )
    {
      return n.location;
    }
  };
}
