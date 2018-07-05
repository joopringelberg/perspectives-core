const path = require("path");

module.exports = {
  entry: path.join(__dirname, "output/Main/index.js" ),
  output: {path: path.join(__dirname, "dist"), filename: "perspectives-core.js"},
  watch: true,
  mode: "development",
  target: "electron-renderer",
  module: {
    rules: []
  }
};
