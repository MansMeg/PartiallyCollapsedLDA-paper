#' Calculate the rare word limit based on proportion in corpus
#' 
#' @description 
#' Function to calculate the rare word limit for a given corpus
#' 
#' @param corpus Corpus on the form used in Mallet
#' @param proportion Proportion of tokens that will be removed for a given rare word limit
#' 
#' @examples
#' corpus <- "datasets/enron_random.txt"
#' calculate_rareword(corpus)
#' @export
calculate_rareword <- function(corpus, prop=0.001, have_enough_memory=TRUE){
  if(have_enough_memory){
    bar <- pcplda:::msg_progress_bar(6)
    crp <- readLines(corpus)
    bar$increment()
    crp <- unlist(lapply(X = crp, FUN=function(X) strsplit(strsplit(X, "\t")[[1]][3], " ")[[1]]))
    bar$increment()
    df <- table(crp)
    bar$increment()
    df <- data.frame(type=names(df), tokens=as.integer(df))  
    bar$increment()
    df <- df[order(df$tokens, decreasing = TRUE),]
    df$index <- 1:nrow(df)
    df$t_cumsum <- cumsum(df$tokens)
    lim_tokens <- round(df$t_cumsum[nrow(df)]*prop)
    tok_sum <- 0
    i <- nrow(df) + 1
    bar$increment()
    while(tok_sum <= lim_tokens){
      i <- i - 1
      tok_sum <- tok_sum + df$tokens[i]
    }
    bar$increment()
    df$tokens[i]
  } else {
    bar <- pcplda:::msg_progress_bar(4)
    i <- 0
    no_lines <- 100000L
    batch_size <- no_lines
    crp_size <- 0L
    file_in <- file(corpus, "r")
    crp_tok_tab_list <- list()
    bar$increment()
    # Write each doc to tmp file
    while(batch_size == no_lines){
      i <- i + 1
      crp <- readLines(file_in, n = no_lines)
      batch_size <- length(crp)
      crp_size <- crp_size + batch_size
      crp_tok <- unlist(lapply(X = crp, FUN=function(X) strsplit(strsplit(X, "\t")[[1]][3], " ")[[1]]))
      crp_tok_tab_list[[i]] <- table(crp_tok)
    }
    close(file_in)
    bar$increment()

    dat <- data.frame(crp_tok_tab_list[[1]])
    names(dat) <- c("types","tokens")
    if(length(crp_tok_tab_list) > 1){
      for(i in 2:length(crp_tok_tab_list)){
        dat <- merge(x = dat,y = data.frame(crp_tok_tab_list[[i]]),by.x = "types", by.y = "crp_tok", all = TRUE) 
        dat$tokens[is.na(dat$tokens)] <- 0
        dat$Freq[is.na(dat$Freq)] <- 0
        dat$tokens <- dat$tokens + dat$Freq
        dat$Freq <- NULL
      }
    }
    bar$increment()
    dat <- dat[order(dat$tokens, decreasing = TRUE),]
    dat$index <- 1:nrow(dat)
    dat$t_cumsum <- cumsum(dat$tokens)
    lim_tokens <- round(dat$t_cumsum[nrow(dat)]*prop) 
    tok_sum <- 0
    i <- nrow(dat) + 1
    while(tok_sum <= lim_tokens){
      i <- i - 1
      tok_sum <- tok_sum + dat$tokens[i]
    }
    bar$increment()
    dat$tokens[i]
    
  }
}
