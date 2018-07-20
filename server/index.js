var createServer = require('./server')

var environment = process.env.NODE_ENV || 'development'
// var config = require('../knexfile')[environment]
// var connection = require('knex')(config)

var server = createServer()

var PORT = process.env.PORT || 8000

if (require.main === module) {
  server.listen(PORT, function () {
    console.log(`Listening on port ${PORT}`)
  })
}