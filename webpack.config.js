const path = require("path");

module.exports = {
  entry: path.join(__dirname, "output/Main/index.js" ),
  output: {
    library: "perspectives-core",
    libraryTarget: "commonjs2",
    filename: "perspectives-core.js",
    path: path.join(__dirname, "dist")
  },
  watch: false,
  mode: "development",
  target: "electron-renderer",
  module: {
    rules: []
  },
  externals: {
    "perspectives-proxy": {
      commonjs: 'perspectives-proxy',
      commonjs2: 'perspectives-proxy',
      amd: 'perspectives-proxy',
      root: "perspectivesProxy"
    }
  }
};
