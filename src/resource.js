'use strict';
function id(obj)
{
  return obj;
}

exports.castPerspectRol = id
exports.castPerspectContext = id;
exports.unwrapPerspectRol = id;
exports.unwrapPerspectContext = id;

exports.saveRevision = function(rev)
{
  return function(cdbr)
  {
    cdbr._rev = rev;
  };
};
