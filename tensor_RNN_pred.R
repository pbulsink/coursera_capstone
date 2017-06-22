#TensorFlow Build Model

library(tensorflow)
library(tictoc)
library(readr)
library(stringi)
library(dplyr)
library(quanteda)

prep_text<-function(){

    pb<-progress_estimated(13)
    pb$print()
    blog<-read_lines("./data/en_US/en_US.blogs.txt", progress=FALSE)
    news<-read_lines("./data/en_US/en_US.news.txt", progress=FALSE)
    twitter<-read_lines("./data/en_US/en_US.twitter.txt", progress=FALSE)
    pb$tick()$print()

    blog<-as.character(tokens(blog, what='sentence'))
    news<-as.character(tokens(news, what='sentence'))
    twitter<-as.character(tokens(twitter, what='sentence'))

    pb$tick()$print()

    corp_tot<-c(blog, news, twitter)
    rm(blog, news, twitter)

    #corp_tot<-stri_trans_general(corp_tot,"Latin-ASCII")
    #pb$tick()$print()
    corp_tot <- gsub("[µºˆ_ðŸ˜Š]+", '', corp_tot, perl = TRUE)

    pb$tick()$print()
    #set to lowercase
    corp_tot <- tolower(corp_tot)
    pb$tick()$print()

    #Remove URLS
    corp_tot <- gsub("(f|ht)tp(s?):\\/\\/(.*)[.][a-z=\\?\\/]+", "<URL>", corp_tot, perl = TRUE)
    pb$tick()$print()

    #Remove Twitter @usernames and hashtags
    corp_tot <- gsub("\\s@[a-z_]{1,20}", "<TWITTERUSER>", corp_tot, perl = TRUE)
    pb$tick()$print()
    corp_tot <- gsub("#[a-z0-9_]+", "<HASHTAG>", corp_tot, perl = TRUE)
    pb$tick()$print()

    badwords <- unlist(read.csv("./badwords.txt", header = FALSE, as.is = TRUE)[,1])
    corp_tot <- gsub(sprintf("(*UCP)\\b(%s)\\b", paste(badwords, collapse = "|")), "", corp_tot, perl = TRUE)
    pb$tick()$print()
    corp_tot <- gsub("_|\\+|-|@|#|\\$|%|\\^|\\&|\\*|\\(|\\)|;|\\\\|\\/|<|>|\"|'|`|~|…|\\|", "", corp_tot, perl = TRUE)
    pb$tick()$print()
    corp_tot <- gsub("\\s\\s+", " ", corp_tot, perl = TRUE)

    corp_tot <- gsub("([a-z])\\.$", "\\1 . ", corp_tot)
    pb$tick()$print()
    corp_tot <- gsub("([a-z])\\,\\s", "\\1 ,", corp_tot)
    pb$tick()$print()
    corp_tot <- gsub("([a-z])\\?$", "\\1 ?", corp_tot)
    pb$tick()$print()
    corp_tot <- gsub("([a-z])\\!$", "\\1 !", corp_tot)
    pb$tick()$print()

    corp_tot<-paste(corp_tot, collapse = ' ')
    pb$tick()$print

    return(corp_tot)
}

prep_precursor<-function(text){
    text<-stri_trans_general(text,"Latin-ASCII")
    text <- gsub("[µºˆ_]+", '', text, perl = TRUE)

    #set to lowercase
    text <- char_tolower(text)

    #Remove URLS
    text <- gsub("(f|ht)tp(s?):\\/\\/(.*)[.][a-z=\\?\\/]+", "<URL>", text, perl = TRUE)

    #Remove Twitter @usernames and hashtags
    text <- gsub("\\s@[a-z_]{1,20}", "<TWITTERUSER>", text, perl = TRUE)
    text <- gsub("#[a-z0-9_]+", "<HASHTAG>", text, perl = TRUE)

    badwords <- unlist(read.csv("./badwords.txt", header = FALSE, as.is = TRUE)[,1])
    text <- gsub(sprintf("(*UCP)\\b(%s)\\b", paste(badwords, collapse = "|")), "", text, perl = TRUE)
    text <- gsub("_|\\+|-|@|#|\\$|%|\\^|\\&|\\*|\\(|\\)|;|\\\\|\\/|<|>|\"|'|`|~|\\|", "", text, perl = TRUE)
    text <- gsub("\\s\\s+", " ", text, perl = TRUE)

    text <- gsub("([a-z])\\.\\s", "\\1 . ", text)
    text <- gsub("([a-z])\\,\\s", "\\1 , ", text)
    text <- gsub("([a-z])\\?\\s", "\\1 ? ", text)
    text <- gsub("([a-z])\\!\\s", "\\1 ! ", text)

    return(text)
}

prep_tensorflow<-function(text, learning_rate=0.001, n_input=3L, n_hidden = 512L){

    text <- unlist(strsplit(text, ' ', fixed=TRUE))
    dictionary <- sort(unique(text))
    #simple transformation to vector, not efficient, for clairty
    text_v <- rep(0, length(text))
    for(i in 1:length(dictionary)){
        text_v[which(text == dictionary[[i]])]<-i
    }

    vocab_size <- length(dictionary)

    #TensorFlow input
    x <- tf$placeholder(tf$float32, shape(NULL, n_input))
    y <- tf$placeholder(tf$float32, shape(NULL, vocab_size))

    #Weights and Biases for RNN
    weights <- tf$Variable(tf$random_normal(shape(n_hidden, vocab_size)))
    biases <- tf$Variable(tf$random_normal(shape(vocab_size)))

    RNN<-function(x, weights, biases){

        x <- tf$reshape(x, c(-1L, n_input))
        x <- tf$split(x, n_input, 1L)

        rnn_cell<-tf$nn$rnn_cell$MultiRNNCell(list(tf$nn$rnn_cell$BasicLSTMCell(n_hidden), tf$nn$rnn_cell$BasicLSTMCell(n_hidden)))

        outputs<-tf$nn$static_rnn(cell = rnn_cell, inputs = x, dtype = tf$float32)

        return(tf$matmul(outputs[[1]][[n_input]], weights) + biases)
    }

    pred<-RNN(x, weights, biases)



    cost<-tf$reduce_mean(tf$nn$softmax_cross_entropy_with_logits(logits = pred, labels = y))
    optimizer <- tf$train$RMSPropOptimizer(learning_rate=learning_rate)$minimize(cost)

    correct_pred <- tf$equal(tf$argmax(pred, 1L), tf$argmax(y, 1L))
    accuracy <- tf$reduce_mean(tf$cast(correct_pred, tf$float32))

    init <- tf$global_variables_initializer()

    sess <- tf$Session()
    sess$run(init)

    return(list(session=sess, dictionary=dictionary, text_v=text_v))
}

train_tensorflow<-function(sess, training_iters=10000L, display_step=1000L, n_steps=10){
    step <- 0

    batch_start <- sample(1:(length(text_v)-n_input-n_steps), 1)

    loss_total <- 0
    acc_total <- 0

    tic()

    while(step < training_iters){
        batchx<-t(as.matrix(sapply(c(0:(n_steps-1)), function(x) text_v[(batch_start+x):(batch_start+x+n_input-1)])))
        batchy<-matrix(0, 10, 112)
        hoty<-sapply(c(0:(n_steps-1)), function(x) text_v[(batch_start+x+n_input)])
        for(i in 1:n_steps){
            batchy[i,hoty[i]]<-1
        }
        runs<-sess$run(c(optimizer, accuracy, cost, pred), feed_dict = dict(x = batchx, y=batchy))
        acc<-runs[[2]]
        loss<-runs[[3]]
        p<-runs[[4]]

        loss_total <- loss_total + loss
        acc_total <- acc_total + acc

        if(step %% display_step == 0){
            message(paste0("Step=", step, ", Avg. Loss=",round(loss_total/display_step, digits=4),
                           ", Avg. Acc.=", round(acc_total/display_step, digits=4),
                           ", [", dictionary[which.max(batchy[n_steps,])],
                           "] vs. [", dictionary[which.max(p[n_steps,])], "]"))
            acc_total<-0
            loss_total<-0
        }
        step <- step + 1
        batch_start <- sample(1:(length(text_v)-n_input-n_steps), 1)
    }
    message("Optimization Finished")
    toc()
}

predict_next_one_word<-function(sess, dictionary, input){
    #input<-cleaninput(input)
    #if input != dictionary Not a problem with word2vec
    input_text <- unlist(strsplit(input_text, ' ', fixed=TRUE))
    input_v <- rep(0, length(input_text))
    for(i in 1:length(dictionary)){
        input_v[which(input_text == dictionary[[i]])]<-i
    }
    batchx<-matrix(input_v, nrow = 1)
    w<-dictionary[[which.max(sess$run(pred, feed_dict = dict(x = batchx)))]]
    w
}

predict_next_n_word<-function(sess, dictionary, input, n=3){
    #input<-cleaninput(input)
    #if input != dictionary Not a problem with word2vec
    input_text <- unlist(strsplit(input_text, ' ', fixed=TRUE))
    input_v <- rep(0, length(input_text))
    for(i in 1:length(dictionary)){
        input_v[which(input_text == dictionary[[i]])]<-i
    }
    batchx<-matrix(input_v, nrow = 1)
    p<-sess$run(pred, feed_dict = dict(x = batchx))
    ps<-head(sort(p), n)
    w<-dictionary[[which(p >= min(ps))]][1:n]
    w
}
