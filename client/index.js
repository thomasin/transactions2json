import { Elm } from "./elm-app/Main"
import scss from "./scss/app.scss"
import { saveAs } from 'file-saver/FileSaver'
var pdfjs = require("pdfjs-dist")

var app = undefined

document.addEventListener("DOMContentLoaded", contentLoaded)

function contentLoaded() {
  app = Elm.Main.init()

  app.ports.domLoaded.subscribe(createDragzone)
  app.ports.downloadJson.subscribe(downloadJSON)
  app.ports.downloadCsv.subscribe(downloadCSV)
}

function downloadJSON({ fileName, transactions }) {
  var blob = new Blob([JSON.stringify({ transactions })], {type: "text/plain;charset=utf-8"})
  saveAs(blob, `${fileName || 'transactions'}.json`)
}

function downloadCSV({ fileName, transactions }) {
  var blob = new Blob([transactions], {type: "text/plain;charset=utf-8"})
  saveAs(blob, `${fileName || 'transactions'}.csv`)
}

function createDragzone() {
  let dropArea = document.getElementById('dropzone')

  window.addEventListener('drop', (e) => e.preventDefault(), false)
  window.addEventListener('dragover', (e) => e.preventDefault(), false)

  dropArea.addEventListener('dragenter', dragEnter, false)
  dropArea.addEventListener('dragleave', dragLeave, false)
  dropArea.addEventListener('dragover', dragOver, false)
  dropArea.addEventListener('drop', dragDrop, false)
}
 
function dragEnter() {}
function dragLeave() {}
function dragOver() {}
function dragDrop (e) {
  e.preventDefault()
  e.stopPropagation()

  Promise.all([].map.call(e.dataTransfer.files, readFile))
    .then((pdfs) => Promise.all(extractTextItems(pdfs)))
    .then(app.ports.pdfsLoaded.send)
}

function readFile (file) {
  return new Promise((resolve, reject) => {
    var reader = new FileReader()

    reader.onloadend = (event) => {
      resolve({ fileName: file.name, buffer: event.target.result })
    }

    reader.readAsArrayBuffer(file)
  })  
}

function extractTextItems(files) {
  return files.map((file) => {
    return new Promise((resolve, reject) => getPdf(file, resolve, reject))
  })
}

function getPdf({ fileName, buffer }, resolve, reject) {
  pdfjs.getDocument(buffer)
    .then((pdf) => Promise.all(getPages(pdf)))
    .then((pages) => Promise.all(pages.map(page => page.getTextContent())))
    .then((pages) => pages.reduce(transformPage, {}))
    .then((pages) => resolve({ fileName, pages }))
    .catch(reject)
}

function getPages(pdf) {
  return new Array(pdf.numPages).fill(null)
    .map((x, i) => pdf.getPage(i + 1))  
}

function transformPage(acc, page, i) {
  return {
    ...acc,
    [`${i}`]: page.items.map((item) => {
      const transform = item.transform
      return {
        text: item.str,
        x: transform[4],
        y: transform[5],
        width: item.width,
        height: item.height
      }
    })
  }
}
