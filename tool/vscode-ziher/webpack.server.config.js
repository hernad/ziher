//@ts-check

'use strict';

const path = require('path');
const CopyPlugin = require('copy-webpack-plugin');

/**@type {import('webpack').Configuration}*/
const config = {
  target: 'node', 
  entry: './src/server/main.js',
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