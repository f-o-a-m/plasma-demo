"use strict";
/* eslint-env node */

var fs = require("fs");
var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var UglifyJSPlugin = require("uglifyjs-webpack-plugin");
var MiniCssExtractPlugin = require("mini-css-extract-plugin");
var AutoDllPlugin = require("autodll-webpack-plugin");


var IS_PROD = process.env.NODE_ENV === "production";
console.log(IS_PROD ? "PRODUCTION build..." : "DEVELOPMENT build...");  // eslint-disable-line no-console

var nodeEnvPlugin = new webpack.DefinePlugin({
  "process.env.NODE_ENV": JSON.stringify(process.env.NODE_ENV),
});

var minifyJSPlugin = new UglifyJSPlugin({
  sourceMap: false,
  uglifyOptions: {
    parallel: true,
    sourceMap: false,
    compress: {
      warnings: false
    },
    ecma: 6,
  }
});

var commonPlugins = [nodeEnvPlugin].concat(IS_PROD ? [ minifyJSPlugin ] : []);

module.exports = function(/*env*/) {
  return {
    mode: IS_PROD ? "production" : "development",
    entry: {
      "styles": "./frontend/styles/index.scss",
      "index": "./frontend/index.js"
    },
    output: {
      path: path.join(__dirname, "frontend/dist"),
      filename: IS_PROD ? "[name].[hash].js": "[name].js",
    },
    devtool: IS_PROD ? false : "eval",
    devServer: {
      contentBase: path.join(__dirname, "frontend/dist"),
    },
    watch: false,
    module: {
      rules: [
        {
          test: /\.(css|sass|scss)$/,
          use: [
            IS_PROD ? MiniCssExtractPlugin.loader : "style-loader",
            "css-loader",
            {
              loader: "postcss-loader",
              options: {
                plugins: function () { return IS_PROD ? [require("autoprefixer"), require("cssnano")] : []; }
              }
            },
            "sass-loader"
          ],
        }
      ]
    },
    plugins: commonPlugins.concat([
      new HtmlWebpackPlugin({
        inject: true,
        title: "Plasma Demo",
        chunks: ["index"],
        template: "./frontend/index.ejs",
        filename: "index.html",
      }),
      new webpack.DefinePlugin({
        "process.env.API_BASE_URL": JSON.stringify(process.env.API_BASE_URL),
      }),
      new MiniCssExtractPlugin({
        filename: IS_PROD ? "[name].[hash].css": "[name].css",
        chunkFilename: IS_PROD ? "[id].[hash].css": "[id].css",
      }),
      new AutoDllPlugin({
        inject: true, // Will inject the DLL bundles into html files
        filename: IS_PROD ? "[name].[hash].js": "[name].js",
        debug: true,
        entry: {
          vendor:
            // It's supposed to be same as `Object.keys(require('./package.json').dependencies)` but instead if that,
            // we type all deps here, so it's more explicit and we don't accidentally include something undesired in bundle.
            []
        },
        config: { mode: "none" }, // https://github.com/asfktz/autodll-webpack-plugin/issues/115
        plugins: commonPlugins,
      }),
    ])
  };

};
