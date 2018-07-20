const express = require('express')
const router = express.Router()

// const users = require('./users')

// Routes

router.get('/pdfs', (req, res) => {
  res.sendStatus(200)
  // users.all(req.app.get('connection'))
  //   .then((users) => {
  //     res.json(users)
  //     res.end()
  //   })
  //   .catch((err) => {
  //     console.log(err)
  //     res.sendStatus(500)
  //   })
})

module.exports = router