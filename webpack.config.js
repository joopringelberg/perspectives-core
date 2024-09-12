// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

const path = require("path");
const webpack = require('webpack');

module.exports = function(env)
  {
    const {target} = env;
    return {
    entry: path.join(__dirname, "output/Main/index.js" ),
    output: {
      library: "perspectives-core",
      libraryTarget: "umd",
      filename: "perspectives-core.js",
      path: path.join(__dirname, "dist")
    },
    resolve: {
      extensions: ['.js']
    },
    watch: false,
    mode: env.target,
    target: "webworker",
    module: {
      rules: [
        {
          test: /\.arc/,
          type: 'asset/source'
        }        
      ]
    },
    plugins: [
      new webpack.DefinePlugin({
        __PDRVersion__: JSON.stringify(require("./package.json").version),
        __MYCONTEXTS__: "https://mycontexts.com/"
      })        
      ],
    externals: {
      "perspectives-proxy": {
        commonjs: 'perspectives-proxy',
        commonjs2: 'perspectives-proxy',
        amd: 'perspectives-proxy',
        root: 'perspectives-proxy'
      },
      // These are Affjax dependencies when running on node.
      "xhr2-cookies": {
        commonjs: "xhr2-cookies",
        commonjs2: "xhr2-cookies",
        amd: "xhr2-cookies",
        root: "xhr2-cookies"
      },
      "url": {
        commonjs: "url",
        commonjs2: "url",
        amd: "url",
        root: "url"
      },
      "pouchdb-browser": {
        commonjs: "pouchdb-browser",
        commonjs2: "pouchdb-browser",
        amd: "pouchdb-browser",
        root: "pouchdb-browser"
      }
    }
  }
};
