'use strict'

const bodyParser = require('body-parser')
const express = require('express')


const familyStore = require('./app/family/FamilyStore.js')

const familyEndpointModule = require('./app/family/FamilyEndpoint.js')


const appPort = process.env.PORT || 8888

const app = express()
app.use(bodyParser.urlencoded({ extended: true }))
app.use(bodyParser.json())


familyStore.init()

const familyEndpoint = familyEndpointModule(familyStore)
familyEndpoint.register(app)

app.listen(appPort, () => {})

