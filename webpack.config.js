const path = require("path")
const MiniCssExtractPlugin = require("mini-css-extract-plugin")
const autoprefixer = require("autoprefixer")

const elmSource = path.resolve(__dirname, 'client/elm-app')

const proxy = require('http-proxy-middleware')
const convert = require('koa-connect')
const Router = require('koa-router')

const router = new Router();

const proxyOptions = {
  target: 'http://localhost:8000',
  changeOrigin: true,
  // ... see: https://github.com/chimurai/http-proxy-middleware#options
}

router.get('*', convert(proxy(proxyOptions)))

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

module.exports.serve = {
  content: [__dirname],
  add: (app, middleware, options) => {
    // since we're manipulating the order of middleware added, we need to handle
    // adding these two internal middleware functions.
    middleware.webpack()
    middleware.content()

    // router *must* be the last middleware added
    app.use(router.routes())
  },
}