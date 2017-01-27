# Read in library
library(XML)

corpus_dir <- "datasets/nyt_corpus/"
parsed_files <- "datasets/nyt_corpus/parsed/"
final_file <- "datasets/nyt_corpus/nyt.txt"
final_file_random <- "datasets/nyt_corpus/nyt_random.txt"
character_limit <- 100

#' Function to parse text data
#'
parse_function <-function(txt){
  # Remove LEAD paragraph
  # txt <- stringr::str_extract(txt, "LEAD:")
  txt <- stringr::str_replace_all(txt, "LEAD:", " ")
  
  # Remove \n
  txt <- stringr::str_replace_all(txt, "\\s", " ")
  
  # Remove thousands decimal
  no_chr1 <- nchar(txt)
  txt <- stringr::str_replace_all(txt, "([0-9]+)(,)([0-9]+)", "\\1\\3")
  no_chr2 <- nchar(txt)
  while(no_chr1 != no_chr2){
    no_chr1 <- no_chr2
    txt <- stringr::str_replace_all(txt, "([0-9]+)(,)([0-9]+)", "\\1\\3")
    no_chr2 <- nchar(txt)
  }
  
  # Remove punctuation
  txt <- stringr::str_replace_all(txt, "[:punct:]+", " ")
  
  # Remove many white-spaces
  txt <- stringr::str_replace_all(txt, "[ ]{2,}", " ")
  
  # All to lower
  txt <- tolower(txt)
  txt <- stringr::str_trim(txt)
  
  txt
}


#' Parse documents
#'
#'
parse_nyt_xml <- function(x){
  xmlfile <- xmlParseDoc(file = x)
  xml_full_text <- getNodeSet(xmlfile, path = "//body.content/block[contains(@class, 'full_text')]")
  xml_lead_paragraph <- getNodeSet(xmlfile, path = "//body.content/block[contains(@class, 'lead_paragraph')]")
  xml_headline <- getNodeSet(xmlfile, path = "//body.head/hedline")

  if(length(xml_full_text) == 0){
    txt <- ""
  } else if(length(xml_full_text) == 1){
    txt <- xmlValue(xml_full_text[[1]])
  } else {
    stop(paste0("No text in article", x))
  }
  txt <- parse_function(txt)
  
  if(length(xml_lead_paragraph) == 1){
    # If lead paragraph exist twice in full text remove duplictate
    lead_txt <- parse_function(xmlValue(xml_lead_paragraph[[1]]))

    if(nchar(lead_txt) > 1 & nchar(lead_txt) < nchar(txt)){
      duplicate_text <- 
        lead_txt == substr(txt, 1, nchar(lead_txt)) & 
        lead_txt == substr(txt, nchar(lead_txt) + 2, 2 * nchar(lead_txt) + 1)
      if(duplicate_text) txt <- substr(txt, start = nchar(lead_txt) + 2, stop = nchar(txt))
    }
  } 

  if(length(xml_headline) == 1) {
    txt <- paste0(parse_function(xmlValue(xml_headline[[1]])), " ", txt)
  } 
  
  file_name <- stringr::str_extract(x, "([0-9])+\\.xml")
  add_mallet_docid((txt), stringr::str_extract(file_name,"([0-9])+"))
}

#' Create mallet document
#'
#'
add_mallet_docid <- function(txt, id){
  stringr::str_c("docno:", id, "\tX\t", txt)
}


parse_nyt_xml_to_mallet <- function(corpus_dir, parsed_files, final_file, character_limit = 100, mc.cores = getOption("mc.cores", 2L)){
  stopifnot(!file.exists(final_file))
  tmp_directory <- file.path(tempdir(), "nyt_corpus")
  if(length(dir(tmp_directory)) > 0) unlink(tmp_directory, recursive = TRUE)
  
  # Already parsed
  already_parsed <- dir(parsed_files)
  already_parsed <- stringr::str_extract(already_parsed, "[0-9]{4,4}.*[0-9]{2,2}")
  already_parsed <- stringr::str_replace_all(already_parsed, "[^0-9]", "")
  
  # Files to UNTAR
  files_to_untar <- dir(path = corpus_dir, recursive = TRUE, full.names = TRUE)
  files_to_untar <- files_to_untar[stringr::str_detect(files_to_untar, "[0-9]+\\.tgz")]
  # Remove already parsed
  files_to_untar_short <- stringr::str_extract(files_to_untar, "[0-9]{4,4}.*[0-9]{2,2}")
  files_to_untar_short <- stringr::str_replace_all(files_to_untar_short, "[^0-9]", "")
  files_to_untar <- files_to_untar[!files_to_untar_short %in% already_parsed]
  
  bar <- msgProgressBar::msgProgressBar(length(files_to_untar))
  
  no_docs <- numeric(length(files_to_untar))
  no_xmls <- numeric(length(files_to_untar))
  tmp_dirs <- file.path(tmp_directory, stringr::str_extract(files_to_untar, "[0-9]+/[0-9]+"))
  
  # i <- 137
  for(i in seq_along(files_to_untar)){ #i <- 14
    # print(files_to_untar[i])
    dir.create(tmp_dirs[i], recursive = TRUE)
    # Untar to the directory
    untar(files_to_untar[i], exdir = tmp_dirs[i]) # i<-37; i <-1
    # List files to parse
    xml_files_to_parse <- dir(tmp_dirs[i], recursive = TRUE, full.names = TRUE)

    
    if(FALSE){
      txt <- character(length(xml_files_to_parse))
      for(l in seq_along(xml_files_to_parse)){
        txt[l] <- parse_nyt_xml(x = xml_files_to_parse[l]) # l <-2 892
      }
    } else {
      txt <- parallel::mclapply(xml_files_to_parse, parse_nyt_xml, mc.cores = mc.cores)
    }
  
    txt <- unlist(txt)
    big_enough <- nchar(txt) >= character_limit
    txt <- txt[big_enough]

    parsed <- paste0(parsed_files, "pnyt", stringr::str_c(stringr::str_split(files_to_untar[i], "/|\\.tgz")[[1]][5:6],collapse="_"), ".txt")
    #con <- file(file.path(final_file, "a", encoding = "UTF-8")
    writeLines(txt, con = parsed)
    #close(con)
    
    no_docs[i] <- length(txt)
    no_xmls[i] <- length(xml_files_to_parse)
    
    # Clean directory
    bar$increment()
  }
  unlink(tmp_directory, recursive = TRUE)
  return(list(txt=no_docs, xml=no_xmls))
}

parsed_files_to_final <- function(parsed_files, final_file){
  files <- dir(parsed_files, full.names = TRUE)
  no_docs <- numeric(length(files))
  bar <- msgProgressBar::msgProgressBar(length(files))
  ff <- file(final_file, "a")
  for(i in seq_along(files)){
    text <- readLines(files[i])
    no_docs[i] <- length(text)
    writeLines(text, ff)
    bar$increment()
  }
  close(ff)
  return(no_docs)
}

system.time(nodoc <- parse_nyt_xml_to_mallet(corpus_dir, parsed_files, final_file, mc.cores = 2))

system.time(crp_size <- parsed_files_to_final(parsed_files, final_file))

system.time(pcplda::random_order_corpus(input_file = final_file, output_file = final_file_random, have_enough_memory = FALSE))
