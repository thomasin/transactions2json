const path = require("path")
const MiniCssExtractPlugin = require("mini-css-extract-plugin")
const autoprefixer = require("autoprefixer")

const elmSource = path.resolve(__dirname, 'client/elm-app')

const proxy = require('http-proxy-middleware')
const convert = require('koa-connect')
const Router = require('koa-router')
// const chokidar = require('chokidar');
// const stringify = require('json-stringify-safe');
// const WebSocket = require('ws');

// const router = new Router();

// const proxyOptions = {
//   target: 'http://localhost:8000',
//   changeOrigin: true,
//   // ... see: https://github.com/chimurai/http-proxy-middleware#options
// }

// router.get('*', convert(proxy(proxyOptions)))

module.exports = {  
  entry: "./client/index.js",

  mode: "development",

  output: {
      filename: "bundle.js",
      path: path.resolve(__dirname, "client/static")
  },

  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: [/node_modules/],
        loader: 'babel-loader'
      },
      {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: {
              loader: "elm-webpack-loader?cwd=" + elmSource,
              options: {
                debug: true
              }
          }
      },
      {
        test: /\.(scss|css)$/,
        use: [
          // MiniCssExtractPlugin.loader,
          "style-loader",
          {
            loader: "css-loader",
            options: {
                minimize: {
                    safe: true
                }
            }
          },
          {
            loader: "postcss-loader",
            options: {
                autoprefixer: {
                    browsers: ["last 2 versions"]
                },
                plugins: () => [
                    autoprefixer
                ]
            },
          },
          {
            loader: "sass-loader",
            options: {}
          }
        ]
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        use: [{
            loader: 'file-loader',
            options: {
                name: '[name].[ext]',
                outputPath: 'fonts/'
            }
        }]
      }
    ]
  },

  resolve: {
      extensions: [".js", ".elm"]
  },

  plugins: [
    new MiniCssExtractPlugin({
      // Options similar to the same options in webpackOptions.output
      // both options are optional
      filename: "app.css"
    })
  ]
}