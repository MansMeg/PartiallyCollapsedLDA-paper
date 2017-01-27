chunk <- readLines("/Users/manma97/Downloads/enwiki-20160113-pages-articles.xml", 1000)
chunk <- readLines("/Users/manma97/Downloads/enwiki-20160113-pages-articles1.xml-p000000010p000010000", 1000)
chunk[1:100]

install.packages("XML")
library(XML)

file <- "/Users/manma97/Downloads/enwiki-20160113-pages-articles1.xml-p000000010p000010000"

test <- character(10000)
i <- 1

MedlineCitation = function(x, ...) {
  #This is a "branch" function
  #x is a XML node - everything inside element <MedlineCitation>
  # find element <ArticleTitle> inside and print it:
  ns <- getNodeSet(x, path = "//text")
  value <- xmlValue(ns[[1]])
  test[i] <<- value
  i <<- i + 1
}

test2 <- xmlEventParse(
  file = file, #"http://www.nlm.nih.gov/databases/dtd/medsamp2015.xml", 
  handlers = NULL, 
  branches = list(MedlineCitation = MedlineCitation)
)

filename = system.file("exampleData", "branch.xml", package="XML")
b.counter = function() {
  nodes <- character()
  f = function(node) { nodes <<- c(nodes, xmlGetAttr(node, "id"))}
  list(b = f, nodes = function() nodes)
}

b = b.counter()
invisible(xmlEventParse(filename, branches = b["b"]))
b$nodes()

readLines(filename)
"Transcription regulation of the nir gene cluster encoding nitrite reductase of Paracoccus denitrificans involves NNR and NirI, a novel type of membrane protein."

medlineRaw <- readLines("http://www.nlm.nih.gov/databases/dtd/medsamp2015.xml")
medlineRaw[1:100]


parse_chunk <-function(chunk){
  chunk <- readLines("/Users/manma97/Downloads/enwiki-20160113-pages-articles.xml", 2000)
  chunk <- stringr::str_c(chunk, collapse = " ")
  part <- stringr::str_detect(chunk, pattern = "<text.+>")
  part2 <- stringr::str_detect(part, pattern = "</text>")
  part[[1]][2]
  chunk3 <- stringr::str_split(string = chunk, pattern = "</page>")[[1]]
  str(chunk3[1])
}

