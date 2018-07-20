const path = require('path')
const express = require('express')
const bodyParser = require('body-parser')
var serveStatic = require('serve-static')

const routes = require('./routes')


module.exports = (connection) => {
  var app = express()
  app.use(serveStatic(path.join(__dirname, '../client/static')))
  app.set('connection', connection)

  app.use(bodyParser.json())
  app.use(bodyParser.urlencoded({ extended: false }))

  app.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*")
    res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
    next()
  })

  app.use('/', routes)

  app.get('*', function(req, res) {
      res.sendFile(path.resolve(__dirname + '/../client/static/index.html'))
  })

  return app
}