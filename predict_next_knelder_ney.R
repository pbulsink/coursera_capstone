library(stringi)
library(data.table)
library(tictoc)

cleanStream <- function(text_stream, badwords = NULL) {
  # get badwords if not supplied.
  if (is.null(badwords)) {
    badwords <- unlist(read.csv("./badwords.txt", header = FALSE, as.is = TRUE)[,
      1])
  }

  # clean formatting
  text_stream <- stri_trans_tolower(text_stream, "Latin-ASCII")
  text_stream <- gsub("[µºˆ_]+", "", text_stream, perl = TRUE)

  # Remove URLS
  text_stream <- gsub("(f|ht)tp(s?):\\/\\/(.*)[.][a-z=\\?\\/]+", "", text_stream,
    perl = TRUE)

  # Remove Twitter 'Via @username' and '@usernames' and 'rt' and hashtags
  text_stream <- gsub("via\\s*@[a-z_]{1,20}", "", text_stream, perl = TRUE)
  text_stream <- gsub("\\s@[a-z_]{1,20}", "", text_stream, perl = TRUE)
  text_stream <- gsub("\\brt\\b", "", text_stream, perl = TRUE)
  text_stream <- gsub("#[a-z0-9_]+", "", text_stream, perl = TRUE)

  # Cut punctuation/symbols/numbers
  text_stream <- gsub("[^a-zA-Z\\ ]", "", text_stream, perl = TRUE)

  # replacement builder code from getAnywhere(tm::removeWords.character)
  text_stream <- gsub(sprintf("(*UCP)\\b(%s)\\b", paste(badwords, collapse = "|")),
    "", text_stream, perl = TRUE)

  # Remove multiple spaces from deleted words
  text_stream <- gsub("\\s\\s+", " ", text_stream, perl = TRUE)

  # add start ^ and end $ tags to the sentences.
  text_stream <- paste("^", text_stream)

  return(text_stream)
}

cutNMax <- function(text_stream, nmax = 5) {
  t <- unlist(strsplit(text_stream, " "))
  if (length(t) < nmax)
    nmax <- length(t)
  return(paste(t[(length(t) - nmax + 1):length(t)], collapse = " "))
}

loadAndPrepGramsFromFile <- function(train = TRUE, small = FALSE) {

  if (train) {
    f <- paste0("./data/ng", 1:5, "_train.RDS")
  } else if (small) {
    f <- paste0("./data/ng", 1:5, "_small.RDS")
  } else {
    f <- paste0("./data/ng", 1:5, ".RDS")
  }
  message("reading 1")
  ng1 <- readRDS(f[1])
  setkey(ng1, pregrams)
  message("2")
  ng2 <- readRDS(f[2])
  setkey(ng2, pregrams)
  message("3")
  ng3 <- readRDS(f[3])
  setkey(ng3, pregrams)
  message(4)
  ng4 <- readRDS(f[4])
  setkey(ng4, pregrams)
  message(5)
  ng5 <- readRDS(f[5])
  setkey(ng5, pregrams)

  nglist <- list(ng1, ng2, ng3, ng4, ng5)
  nglist
}

knP <- function(text, candidate, ng, allNGrams) {
  # where ng = length(text) + 1 text is the pregrams candidate is the candidate
  # postgram
  if (ng > 1) {
    likegram <- allNGrams[[ng]][pregrams == text]
    a <- sum(likegram[postgrams == candidate, freq])
    d <- knD(ng)
    ct <- sum(likegram[, freq])
    n <- nrow(likegram)
    y <- d/ct * n
    plittle <- knPcont(cutNMax(text, ng - 2), candidate, ng - 1, allNGrams)
    p <- max(c(a - d), 0)/ct + y * plittle
  } else {
    p <- nrow(allNGrams[[2]][postgrams == candidate])/nrow(allNGrams[[2]])
  }

  return(p)
}

knPcont <- function(text, candidate, ng, allNGrams) {
  if (ng > 1) {
    likegram <- allNGrams[[ng + 1]][pregrams %like% paste0(text, "$")]
    a <- nrow(likegram[postgrams == candidate])
    d <- knD(ng)
    n <- nrow(allNGrams[[ng]][pregrams == text])
    ct <- nrow(likegram)
    y <- d/ct * n
    plittle <- knPcont(cutNMax(text, ng - 2), candidate, ng - 1, allNGrams)
    p <- max(c((a - d), 0))/n + y * plittle
  } else {
    p <- nrow(allNGrams[[2]][postgrams == candidate])/nrow(allNGrams[[2]])
  }

  return(p)
}

# knNpre <- function(text, candidate, ng, allNGrams) {
#   ag <- allNGrams[[ng + 1]][pregrams %like% paste0(text, "$") & postgrams == candidate]
#   return(nrow(ag))
# }
#
# knYp <- function(text, ng, allNGrams) {
#   #
#   n <- knN(text, ng, allNGrams)
#   ct <- sum(allNGrams[[ng]][pregrams == text, freq])
#   y <- knD(ng)/ct * n
#   return(y)
# }
#
# knYn <- function(text, ng, allNGrams) {
#   #
#   n <- knN(text, ng, allNGrams)
#   ct <- nrow(allNGrams[[ng + 1]][pregrams %like% paste0(text, "$")])
#   y <- knD(ng)/ct * n
#   return(y)
# }
#
# knN <- function(text, ng, allNGrams) {
#   return(nrow(allNGrams[[ng]][pregrams == text]))
# }

kn_predict <- function(text, allNGrams) {
  tic()
  text_stream <- cleanStream(text)

  text4 <- cutNMax(text_stream, 4)
  text3 <- cutNMax(text_stream, 3)
  text2 <- cutNMax(text_stream, 2)
  text1 <- cutNMax(text_stream, 1)

  if (length(unlist(strsplit(text4, " "))) == 4) {
    message("starting with 5g")
    message(paste(text4, collapse = " "))
    candidates <- kn_cand(text4, 5, allNGrams)
  } else if (length(unlist(strsplit(text3, " "))) == 3) {
    message("starting with 4g")
    candidates <- kn_cand(text3, 4, allNGrams)
  } else if (length(unlist(strsplit(text2, " "))) == 2) {
    message("starting with 3g")
    candidates <- kn_cand(text2, 3, allNGrams)
  } else if (length(unlist(strsplit(text1, " "))) == 1) {
    message("starting with 2g")
    candidates <- kn_cand(text1, 2, allNGrams)
  } else {
    message("starting with 1g")
    candidates <- kn_cand(text1, 1, allNGrams)
  }

  # candidates<-candidates[1:20, c('postgrams','prop')]
  toc()
  return(candidates[prop>1e-4])
}

kn_cand <- function(text, ng, allNGrams, cand) {
  if (ng > 1) {
    candidates <- allNGrams[[ng]][pregrams == text]
    message(paste0("There are ", nrow(candidates), " candidates"))
    if (nrow(candidates) > 0) {
      candidates[, `:=`(prop, 0)]
      for (i in 1:nrow(candidates)) {
        message(paste0(i, ' of ', nrow(candidates)))
        candidate <- candidates[i, postgrams]
        if (!candidate %in% cand)
          candidates[i, `:=`(prop, knP(text, candidate, ng, allNGrams))]
      }
      candidates[, `:=`(pregrams, NULL)]
    } else {
      message(paste0("no candidates ng", ng))
      candidates <- data.table(freq = integer(), postgrams = character(), prop = numeric())
    }
    if(nrow(candidates) < 25){
        cNGless <- kn_cand(cutNMax(text, ng - 2), ng - 1, allNGrams, candidates[,
        postgrams])
        candidates <- rbind(candidates, cNGless)
    }
  } else {
    message("Searching Base language")
    candidates <- head(allNGrams[[1]], 10)
    candidates[, `:=`(postgrams, pregrams)]
    candidates[, `:=`(pregrams, NULL)]
    for (i in 1:10) {
      candidate <- candidates[i, postgrams]
      candidates[i, `:=`(prop, nrow(allNGrams[[2]][postgrams == candidate])/nrow(allNGrams[[2]]))]
    }
  }
  return(candidates)
}

knD <- function(ng) {
  # Calculated as nrow(ng[freq==1])/(nrow(ng[freq==1])+2*nrow(ng[freq==2]))
  knd <- c(0.6951672, 0.7748497, 0.8661139, 0.9337619, 0.9700328)
  return(knd[ng])
}

test<-function(candidates){
    if (ng > 1) {
        countpre <- allNGrams[[ng]][pregrams == text]
        ncand<-list()
        npre<-list()
        npost<-list()
        pcont<-list()
        t<-list()
        for(i in (ng-1):2){
            t[i]<-cutNMax(text, i)
            npost[i]<-nrow(allNGrams[[i+1]][pregrams %like% paste0(text,'$')])
            npre[i]<-nrow(allNGrams[[i]][pregrams == text])
        }
        t[1]<-cutNMax(text,1)
        t[ng]<-text
        npre[ng]<-sum(allNGrams[[ng]][pregrams == text, freq])
        ncand[ng]<-NULL

        y <- d/ct * n
        plittle <- knPcont(cutNMax(text, ng - 2), candidate, ng - 1, allNGrams)
        p <- max(c(a - d), 0)/ct + y * plittle
    } else {
        p <- nrow(allNGrams[[2]][postgrams == candidate])/nrow(allNGrams[[2]])
    }
}
