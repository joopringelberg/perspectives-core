// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import webpack from "webpack";
let packageJson;
import { promises as fs } from 'fs';

export default async function(env) {
  packageJson = JSON.parse(await fs.readFile(new URL('./package.json', import.meta.url)));
  const { target } = env;
  return {
    entry: new URL('./output/Main/index.js', import.meta.url).pathname,
    output: {
      library: {
        type: "module"
      },
      chunkFormat: "module",
      filename: "perspectives-core.js",
      path: new URL('./dist', import.meta.url).pathname,
    },
    experiments: {
      outputModule: true // Enable output as a module
    },
    resolve: {
      extensions: ['.js']
    },
    watch: false,
    mode: env.target,
    target: "webworker",
    devtool: 'source-map',
    module: {
      rules: [
      {
        test: /\.arc/,
        type: 'asset/source'
      },
      {
        test: /\.js$/,
        enforce: 'pre',
        use: ['source-map-loader'],
        include: new URL('./output', import.meta.url).pathname,
        exclude: /node_modules/ // Exclude node_modules from source map processing
        }
      ]
    },
    plugins: [
      new webpack.DefinePlugin({
        __PDRVersion__: packageJson.version ? `"${packageJson.version}"` : '"no version"',
        __MYCONTEXTS__: '"https://mycontexts.com/"'
      })
    ],
    externals: {
      "eventsource": 'commonjs eventsource',
      }
  };
};