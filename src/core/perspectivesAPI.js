if (exports === undefined)
{
  exports = module.exports;
}

exports.connect = require("client/perspectivesApiProxy.js").connect;

exports.createRequestEmitterImpl = require("client/perspectivesApiProxy.js").createRequestEmitterImpl
