library(XML)

# Parameters
# Wiki XML file
xml_file_to_parse <- "/Users/manma97/Downloads/enwiki-20160113-pages-articles1.xml-p000000010p000010000"

page = function(x, ...) {
  # This is a "branch" function to handle Wikipedia text
  # x is a XML node - everything inside element <page>
  # Get element <revision>/<text> inside and return content as txt
  ns <- getNodeSet(x, path = "//revision//text")
  txt <- xmlValue(ns[[1]])
  
  # A predefined parse function to handle Wiki text objects
  parse_function(txt)
}

# Create params object to use in parse_function
params <- list()
params$connection <- file("test.txt", "a", encoding = "UTF-8")
params$i <- 1

text_example <- character(10000)
#nchar(text_example[124])
#save(text_example, file="text.Rdata")
load("text.Rdata")
txt <- text_example[184]


# Parse function to handle txt data
parse_function <- function(txt){
  print(params$i)
  text_example[params$i] <<- txt
  params$i <<- params$i + 1
  
  if(nchar(txt) < 2000){
    return(invisible(TRUE))
  }

  # Preprocess txt

  # Remove space characters
  txt <- stringr::str_replace_all(txt, "\\s", " ")

  # Remove all <!--  --> comments
#  tmp <- stringr::str_extract_all(txt, "<!--.+?-->")[[1]]
  txt <- stringr::str_replace_all(txt, "<!--.+?-->"," ")
  
  # Remove [[File:...]] [[Image:]]Images with text
#  tmp <- stringr::str_extract_all(txt, "(\\[\\[File:|\\[\\[Image:)([^\\[\\]]*|\\[\\[.*?\\]\\])*\\]\\]")[[1]]
  txt <- stringr::str_replace_all(txt, "(\\[\\[File:|\\[\\[Image:)([^\\[\\]]*|\\[\\[.*?\\]\\]|\\[.*?\\])*\\]\\]"," ")

  # Remove single refernce tags <ref name="AAA"/> 
  #  tmp <- stringr::str_extract_all(txt, "<ref[^>]*?\\/>")[[1]]
  txt <- stringr::str_replace_all(txt, "<ref[^>]*?\\/>"," ")  
  
  # Remove all references <ref> </ref> 
  #  tmp <- stringr::str_extract_all(txt, "<ref.*?>.+?<\\/ref>")[[1]]
  txt <- stringr::str_replace_all(txt, "<ref.*?>.+?<\\/ref>"," ")
  
  # Choose the first element in internal links [[...|...]]
  #  tmp <- stringr::str_extract_all(txt, "(\\[\\[)([^\\]]+?)(\\|.+?)(\\]\\])")[[1]]
  #  tmp2 <- stringr::str_replace_all(tmp, "(\\[\\[)([^\\]]+?)(\\|.+?)(\\]\\])","\\1\\2\\4")
  txt <- stringr::str_replace_all(txt, "(\\[\\[)([^\\]]+?)(\\|.+?)(\\]\\])","\\1\\2\\4")
  
  # Remove [[Category:...]], [[:Category:...]], [[File:...]]
  #  tmp <- stringr::str_extract_all(txt, "(\\[\\[)(Category|:Category)(.+?)(\\]\\])")[[1]]
  txt <- stringr::str_replace_all(txt, "(\\[\\[)(Category|:Category)(.+?)(\\]\\])"," ")
  
  # Remove [[wikt:...]]
  #  tmp <- stringr::str_extract_all(txt, "(\\[\\[)[wW]ikt:")[[1]]
  txt <- stringr::str_replace_all(txt, "(\\[\\[)[wW]ikt:","\\1")
  
  # Remove all tags <u>
  #  tmp <- stringr::str_extract_all(txt, "<.{1,2}>")[[1]]
  txt <- stringr::str_replace_all(txt, "<.{1,2}>"," ")  
    
  # Remove all {{ }} (templates) and two level nested 
  txt <- stringr::str_replace_all(txt, "\\{\\{[^{}]+\\}\\}"," ")
  txt <- stringr::str_replace_all(txt, "\\{\\{[^{}]+\\}\\}"," ")

  # Remove inner link tags [[...]]
#  tmp <- stringr::str_extract_all(txt, "(\\[\\[)(.*?)(\\]\\])")[[1]]
#  tmp2 <- stringr::str_replace_all(tmp, "(\\[\\[)(.*?)(\\]\\])","\\2")
  txt <- stringr::str_replace_all(txt, "(\\[\\[)(.*?)(\\]\\])","\\2")

  # Remove web site adresses [http]
#  tmp <- stringr::str_extract_all(txt, "\\[http.*?\\]")[[1]]
  txt <- stringr::str_replace_all(txt, "\\[http.*?\\]"," ")
  
  # Remove symbols
#  tmp <- stringr::str_extract_all(txt, "\\&nbsp;")[[1]]
  txt <- stringr::str_replace_all(txt, "\\&nbsp;"," ")

  # Remove headings
#  tmp <- stringr::str_extract_all(txt, "={1,6}[^=]*?={1,6}")[[1]]
  txt <- stringr::str_replace_all(txt, "={1,6}[^=]*?={1,6}"," ")
    
  # Remove punctuation
#  tmp <- stringr::str_extract_all(txt, "[*.;',\":!)(#\\[\\]//]+")[[1]]
  txt <- stringr::str_replace_all(txt, "[*.;',\":!)(#\\[\\]//]+", " ")
  
  # Remove many white-spaces
#  tmp <- stringr::str_extract_all(txt, "[ ]{2,}")[[1]]
  txt <- stringr::str_replace_all(txt, "[ ]{2,}", " ")
  
  # Remove many remove - when one side is space
#  tmp <- stringr::str_extract_all(txt, "( [-–])(\\w)")[[1]]
#  tmp2 <- stringr::str_replace_all(tmp, "( [-–])(\\w)", " \\2")
  txt <- stringr::str_replace_all(txt, "( [-–])(\\w)", " \\2")
#  tmp <- stringr::str_extract_all(txt, "(\\w)([-–] )")[[1]]
#  tmp2 <- stringr::str_replace_all(tmp, "(\\w)([-–] )", "\\1 ")
  txt <- stringr::str_replace_all(txt, "(\\w)([-–] )", "\\1 ")
#  tmp <- stringr::str_extract_all(txt, " [-–] ")[[1]]
  txt <- stringr::str_replace_all(txt, " [-–] ", " ")
  
  # All to lower
  txt <- tolower(txt)
  txt <- stringr::str_trim(txt)
  
  if(nchar(txt) >= 2000){
    writeLines(txt, params$connection)
  }
}

# Parse the XML file
xmlEventParse(
  file = xml_file_to_parse, 
  handlers = NULL, 
  branches = list(page = page)
)

close(params$connection)




txt <- "{{Infobox scientist
| name       = Albert Einstein
| image       = Einstein 1921 by F Schmutzer - restoration.jpg
| caption     = Albert Einstein in 1921
| birth_date  = {{Birth date|df=yes|1879|3|14}}
| birth_place = [[Ulm]], [[Kingdom of Württemberg]], [[German Empire]]
| death_date  = {{Death date and age|df=yes|1955|4|18|1879|3|14}}
| death_place = {{nowrap|[[Princeton, New Jersey]], U.S.}}
| children    = [[Lieserl Einstein|Lieserl]] (1902–1903?)<br />[[Hans Albert Einstein|Hans Albert]] (1904–1973)<br />[[Eduard Einstein|Eduard \"Tete\"]] (1910–1965)
| spouse      = [[Mileva Marić]]&nbsp;(1903–1919)<br />{{nowrap|[[Elsa Löwenthal]]&nbsp;(1919–1936)}}
| residence   = Germany, Italy, Switzerland, Austria (today: [[Czech Republic]]), Belgium, United States
| citizenship = {{Plainlist|
* [[Kingdom of Württemberg]] (1879–1896)
* [[Statelessness|Stateless]] (1896–1901)
* [[Switzerland]] (1901–1955)
* Austria of the [[Austro-Hungarian Empire]] (1911–1912)
* Germany (1914–1933)
* United States (1940–1955)
}}
| ethnicity  = Jewish
| fields    = [[Physics]], [[philosophy]]
| workplaces = {{Plainlist|
* [[Swiss Patent Office]] ([[Bern]]) (1902–1909)
* [[University of Bern]] (1908–1909)
* [[University of Zurich]] (1909–1911)
* [[Karl-Ferdinands-Universität|Charles University in Prague]] (1911–1912)
* [[ETH Zurich]] (1912–1914)
* [[Prussian Academy of Sciences]] (1914–1933)
* [[Humboldt University of Berlin]] (1914–1917)
* [[Kaiser Wilhelm Institute]] (director, 1917–1933)
* [[German Physical Society]] (president, 1916–1918)
* [[Leiden University]] (visits, 1920)
* [[Institute for Advanced Study]] (1933–1955)
* [[Caltech]] (visits, 1931–1933)
}}
| alma_mater = {{Plainlist|
* [[ETH Zurich|Swiss Federal Polytechnic]] (1896–1900; B.A., 1900)
* [[University of Zurich]] (Ph.D., 1905)
}}
| doctoral_advisor  = [[Alfred Kleiner]]
| thesis_title      = Eine neue Bestimmung der Moleküldimensionen (A New Determination of Molecular Dimensions)
| thesis_url        = http://e-collection.library.ethz.ch/eserv/eth:30378/eth-30378-01.pdf
| thesis_year       = 1905
| academic_advisors = [[Heinrich Friedrich Weber]]
| influenced  = {{Plainlist|
* [[Ernst G. Straus]]
* [[Nathan Rosen]]
* [[Leó Szilárd]]
}}
| known_for = {{Plainlist|
* [[General relativity]] and [[special relativity]]
* [[Photoelectric effect]]
* ''[[Mass–energy equivalence|E=mc<sup>2</sup>]]''
* Theory of [[Brownian motion]]
* [[Einstein field equations]]
* [[Bose–Einstein statistics]]
* [[Bose–Einstein condensate]]
* [[Gravitational wave]]
* [[Cosmological constant]]
* [[Classical unified field theories|Unified field theory]]
* [[EPR paradox]]
}}
| awards = {{Plainlist|
* [[Barnard Medal for Meritorious Service to Science|Barnard Medal]] (1920)
* [[Nobel Prize in Physics]] (1921)
* [[Matteucci Medal]] (1921)
* [[ForMemRS]] (1921)<ref name=\"frs\" />
* [[Copley Medal]] (1925)<ref name=\"frs\" />
* [[Max Planck Medal]] (1929)
* [[Time 100: The Most Important People of the Century|''Time'' Person of the Century]] (1999)
}}
| signature = Albert Einstein signature 1934.svg
}}"