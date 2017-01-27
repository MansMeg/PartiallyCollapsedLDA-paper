#' Parse wiki corpus to Mallet format
#' 
#' @description
#' This function will parse the wikipedia corpus into Mallet format. The file that can be parsed is
#' the tagged wikipedia data (Wikipedia corpus v.1.0) found at: 
#' http://www.cs.upc.edu/~nlp/wikicorpus/
#'  
#' @param wiki_folder The folder from 
#' @param mallet_file Filename to printout to Mallet format 
#'  
#' @examples
#' \dontrun{
#'   wiki_folder <- "datasets/tagged.en/"
#'   tmp_folder <- "datasets/tmp/"
#'   mallet_file <- "datasets/wiki.txt"
#'   parse_wiki_corpus(wiki_folder = wiki_folder, tmp_folder=tmp_folder, mallet_file = mallet_file)
#'   }
#' 
#' @export
parse_wiki_corpus <- function(wiki_folder, tmp_folder = tempdir(), mallet_file){

  # Get chunk files
  chunk_files <- dir(wiki_folder, full.names = FALSE)
  done_files <- dir(tmp_folder, full.names = FALSE)
  done_files <- unlist(lapply(stringr::str_split(done_files,".Rdata"), FUN=function(X) X[1]))
  chunk_files <- chunk_files[!chunk_files %in% done_files]
  
  parse_and_store_wiki <- function(chunk_file, tmp_folder, wiki_folder) {
    tmp_doc <- suppressMessages(file_to_doc_vector_fast(raw_file = readLines(file.path(wiki_folder, chunk_file))))
    save(tmp_doc, file = file.path(tmp_folder, paste0(chunk_file, ".Rdata")))
    invisible(TRUE)
  }
  
  check <- parallel::mclapply(X = chunk_files, FUN = parse_and_store_wiki, tmp_folder = tmp_folder, wiki_folder = wiki_folder, mc.cores = parallel::detectCores())
  stopifnot(all(check))
  
  clean_wiki_docs <- function(doc_chunk){
    # Remove ENDOFARTICLE
    docs <- stringr::str_replace_all(doc_chunk[[1]], "ENDOFARTICLE", "")
    
    # Remove PUNCTUATION
    docs <- stringr::str_replace_all(docs, "[:punct:]", "")
    docs <- stringr::str_replace_all(docs, "~", "")

    # Remove numerous white space and trim last white space
    docs <- stringr::str_replace_all(docs, "[ ]+", " ")
    docs <- stringr::str_trim(docs)
    
    # All to lower 
    docs <- tolower(docs)
    
    # Store in Mallet format
    mallet <- stringr::str_c("docno:", doc_chunk[[2]], " X\t", docs)
    return(mallet)
  }

  files_to_clean <- dir(tmp_folder)
  con <- file(mallet_file, open = "a")
  # bar <- msgProgressBar::msgProgressBar(length(files_to_clean))
  for(i in seq_along(files_to_clean)){ # i <- 1
    #bar$increment()
    load(file = file.path(tmp_folder,files_to_clean[i]))
    writeLines(text = clean_wiki_docs(tmp_doc), con = con)
    rm(tmp_doc)
  }
  close(con)  
}


#' Parse file content
#' 
#' @description 
#' Parse a read raw file in tagged.en files to one doc per element vector.
#' 
#' @param raw_file A raw file read in from wikipedia corpus
#' 
file_to_doc_vector <- function(raw_file){
  doc_vec <- character(length(raw_file))
  doc_id <- numeric(length(raw_file))
  doc <- 1
  bar <- msg_progress_bar(length(raw_file))
  for(i in seq_along(raw_file)){ 
    bar$increment()
    if(stringr::str_detect(raw_file[i], "</doc>")) {
      doc <- doc + 1
      next()
    }
    if(stringr::str_detect(raw_file[i], "<doc.+>")) {
      doc_id[doc] <- 
        as.numeric(stringr::str_extract(
          stringr::str_split(raw_file[i], pattern = " ")[[1]][2], 
          pattern = "[0-9]+"))
    }
    txt <- stringr::str_replace_all(raw_file[i], pattern = "<.+>", "")
    txt <- stringr::str_split(txt, pattern = " ")[[1]][1]
    if(txt != "") doc_vec[doc] <- stringr::str_c(doc_vec[doc], txt, " ")
  }
  
  list(doc=doc_vec[1:doc], doc=doc_id[1:doc])
}

file_to_doc_vector_fast <- function(raw_file){
  id <- stringr::str_extract(raw_file, "(<doc id=\")([0-9]+)")
  id <- as.numeric(stringr::str_extract(id, "[0-9]+"))

  token <- stringr::str_split(string = raw_file, pattern = " ")
  token <- unlist(lapply(token, FUN=function(X) X[1]))
  token[!is.na(id)] <- "NEWDOCUMENTTOPARSE"
  token <- paste(token, collapse = " ")
  token <- unlist(stringr::str_split(token, "NEWDOCUMENTTOPARSE"))[-1]
  token <- stringr::str_replace_all(token, "</doc>", "")
  
  list(doc=token, id=id[!is.na(id)])
}
