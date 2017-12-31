'use strict'

const _ = require('lodash')
const fs = require('fs')


function FamilyStore() {

  let families = []
  const pathToFamilyData = './data/family.json'


  function init() {
    return new Promise((resolve) => {
      const familyData = JSON.parse(fs.readFileSync(pathToFamilyData, 'utf8'))
      families = _.cloneDeep(familyData)
      resolve()
    })
  }

  function list() {
    return _
      .cloneDeep(families)
      .filter(family => family.free)
      .map(family => {
        return {
          id: family.id,
          free: family.free,
          children: family.children
        }
      })
  }

  function exportList() {
    return _.cloneDeep(families)
  }

  function update(family) {
    const familyToUpdate = _.find(families, f => family.id == f.id)
    if (!familyToUpdate) {
      console.error('No family to update found', family)
      return false
    }
    familyToUpdate.free = false
    familyToUpdate.contact = family.contact
    const fileContent = JSON.stringify(families)
    fs.writeFileSync(pathToFamilyData, fileContent)

    return true
  }


  const api = {
    exportList: exportList,
    init: init,
    list: list,
    update: update
  }

  return api

}


const familyStore = FamilyStore()
module.exports = familyStore

