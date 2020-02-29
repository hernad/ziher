//@ts-check

'use strict';

const path = require('path');
const CopyPlugin = require('copy-webpack-plugin');

/**@type {import('webpack').Configuration}*/
const config = {
  target: 'node', 
  entry: {
    "zh_server": './src/server/main.ts'
  },
  resolve: {
    // support reading TypeScript and JavaScript files, 📖 -> https://github.com/TypeStrong/ts-loader
    extensions: ['.ts', '.js']
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        use: [
          {
            loader: 'ts-loader',
            options: {
                //compilerOptions: {
                //    "module": "es6" // override `tsconfig.json` so that TypeScript emits native JavaScript modules.
                //}
            }
          }

        ]
      },
      {
        test: /\.js$/,
        exclude: /node_modules/
      }
    ]
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'zh_server.js'
  },
  devtool: 'source-map',
  node: {
    __dirname: false
  },
  plugins: [
    new CopyPlugin([
      {from: "ziher/zh_docs.*", flatten: true}
    ])
  ]
};
module.exports = config;