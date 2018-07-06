const path = require("path");

module.exports = {
  entry: path.join(__dirname, "output/Main/index.js" ),
  output: {
    library: "perspectives-core",
    libraryTarget: "umd",
    filename: "perspectives-core.js",
    path: path.join(__dirname, "dist")
  },
  watch: true,
  mode: "development",
  target: "electron-renderer",
  module: {
    rules: []
  }
};
