#' Function to randomly reorder documents in corpus file
#' 
#' @description
#' Function that reads a corpus file, reorder the documents randomly and printout a new randomly ordered corpus file. 
#'  
#' @param input_file File to reorder
#' @param output_file Reordered file
#' @param seed seed to use for random reorder
#'  
#' @examples
#' \dontrun{
#'   input_file <- "datasets/enron.txt" # 39860
#'   output_file <- "datasets/enron_test1.txt" 
#'   random_order_corpus(input_file = "datasets/enron.txt", output_file = "datasets/enron_test1.txt", have_enough_memory = FALSE)
#'   }
#' @export
random_order_corpus <- function(input_file, output_file, seed = 4711, have_enough_memory=TRUE){
  if(have_enough_memory){
    crp <- readLines(input_file)
    set.seed(seed)
    index <- sample(x = 1:length(crp), size = length(crp))
    writeLines(crp[index], con = output_file)
  } else {
    dir.create(paste0(tempdir(), "/largecrp"))
    tmp_file <- paste0(tempdir(), "/largecrp/doc")
    file_i <- 0L
    no_lines <- 100000L
    batch_size <- no_lines
    crp_size <- 0L
    file_in <- file(input_file, "r")
    # Write each doc to tmp file
    while(batch_size == no_lines){
      crp <- readLines(file_in, n = no_lines)
      batch_size <- length(crp)
      crp_size <- crp_size + batch_size
      for(j in 1:length(crp)){
        file_i <- file_i + 1L
        writeLines(crp[j], paste0(tmp_file, "_", file_i))
      }
      message(crp_size)
    }
    close(file_in)

    # Create index
    set.seed(seed)
    index <- sample(x = 1L:crp_size, size = crp_size, replace = FALSE)
    
    # Write output file
    bar <- msg_progress_bar(crp_size)
    file_out <- file(output_file, "a")
    for(i in 1:crp_size){
      bar$increment()
      write(readLines(paste0(tmp_file, "_", index[i])), file=file_out, append=TRUE)
    }
    close(file_out)
    
    # Delete all files in temporary
    message("Cleaning up")
    unlink(paste0(tempdir(), "/largecrp"), recursive=TRUE)
  }
}



#' Message progress bar
#' 
#' @description 
#' A simple progress bar to use in R packages where messages are prefered to console output.
#' 
#' @field iter Total number of iterations
#' @field i Current iteration
#' @field width Width of the R console
#' @field width_bar Width of the progress bar
#' @field progress The number of character printed (continous)
#' @field progress_step Addition to progress per iteration
#' 
#' @examples
#' test_bar <- function(i = 10){
#'  bar <- msgProgressBar(i)
#'  for(j in 1:i){
#'    bar$increment()
#'    Sys.sleep(0.01)
#'    }
#'  }
#'  test_bar(100)
#'   
#' @author Mans Magnusson (MansMeg @ github)
#'   
msg_progress_bar <- 
  setRefClass(
    Class = "msg_progress_bar", 
    fields = list(iter = "numeric",
                  i = "numeric",
                  progress = "numeric",
                  progress_step = "numeric",
                  width = "numeric",
                  width_bar = "numeric"),
    
    methods = list(
      initialize = function(iter){
        'Initialize a messagebar object'
        .self$width <- getOption("width")
        .self$iter <- iter
        .self$i <- 0
        .self$progress <- 0
        white_part <- paste(rep(" ", (.self$width - 11) %/% 4), collapse="")
        init_length <- .self$width - ((.self$width - 11) %/% 4) * 4 - 11
        white_init <- paste(rep(" ", init_length), collapse="")
        .self$width_bar <- .self$width - init_length - 2 + 0.1
        .self$progress_step <- .self$width_bar / .self$iter
        message(paste(white_init, "|", white_part, "25%", white_part, "50%", white_part, "75%", white_part, "|","\n", white_init, "|", sep=""), appendLF = FALSE)
      },
      
      increment = function(){
        'A messagebar object.'
        if(.self$i > .self$iter) return(invisible(NULL))
        new_progress <- .self$progress + .self$progress_step
        diff_in_char <- floor(new_progress) - floor(.self$progress)
        if(diff_in_char > 0) {
          message(paste(rep("=", diff_in_char),collapse=""), appendLF = FALSE)
        }
        
        .self$progress <- new_progress
        .self$i <- .self$i + 1
        if(.self$i == .self$iter) message("|\n", appendLF = FALSE)
        
      }
    )
  )
