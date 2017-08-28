#' Example script to generate text .
#'
#' At least 20 epochs are required before the generated text starts sounding
#' coherent.
#'
#' It is recommended to run this script on GPU, as recurrent networks are quite
#' computationally intensive.
#'
#' If you try this script on new data, make sure your corpus has at least ~100k
#' characters. ~1M is better.
#'

library(keras)
library(readr)
library(stringr)
library(stringi)
library(purrr)
library(tokenizers)


# Parameters --------------------------------------------------------------

maxlen <- 40
datafraction <- 0.5
iterations <- 2
neuralnodes <- 500
lr <- 0.01
batch_size <- 512
bychar <- 4
epoch_splits <- 50

# Data preparation --------------------------------------------------------

path <- c("./data/en_US/en_US.blogs.txt", "./data/en_US/en_US.news.txt", "./data/en_US/en_US.twitter.txt")

t1 <- read_lines(path[1])
t2 <- read_lines(path[2])
t3 <- read_lines(path[3])

t1 <- t1[sample(1:length(t1), size = length(t1) * datafraction, replace = FALSE)]
t2 <- t2[sample(1:length(t2), size = length(t2) * datafraction, replace = FALSE)]
t3 <- t3[sample(1:length(t3), size = length(t3) * datafraction, replace = FALSE)]

text <- c(t1, t2, t3) %>% str_c(collapse = " ") %>%
    stri_enc_toascii() %>%
    tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE, lowercase = FALSE)

rm(t1, t2, t3, path)

gc(verbose = FALSE)

text <- text[!text %in% c("\032", "\037", "\177", "\003", "\020", "\n", "\035")]

text_length <- length(text)
print(sprintf("corpus length: %d", text_length))

chars <- text %>% unique() %>% sort()

chars_length <- length(chars)
print(sprintf("total chars: %d", chars_length))

# Text Generator because all of the text is toooooo big.
text_generator <- function(text, chars, maxlen, batch_size = 25, start = NULL, bychar = 1) {
    if (is.null(start)) {
        i <- sample(1:bychar, 1)
    } else {
        i <- start
    }
    function() {
        # cut the text in semi-redundant sequences of maxlen characters
        dataset <- map(seq(1, batch_size * bychar, bychar),
                       ~list(sentence = text[.x:(.x + maxlen - 1)],
                             next_char = text[.x + maxlen]))

        dataset <- transpose(dataset)

        # vectorization
        X <- array(0, dim = c(length(dataset$sentence), maxlen, chars_length))
        y <- array(0, dim = c(length(dataset$sentence), chars_length))

        for (i in 1:length(dataset$sentence)) {

            X[i, , ] <- sapply(chars, function(x) {
                as.integer(x == dataset$sentence[[i]])
            })

            y[i, ] <- as.integer(chars == dataset$next_char[[i]])

        }

        if (i > (text_length - (batch_size * bychar) - maxlen)) {
            i <- sample(1:bychar, 1)
        }

        X <- to_numpy_array(X)
        y <- to_numpy_array(y)
        return(list(X, y))
    }
}

# Register Text_generator
tg <- text_generator(text, chars, maxlen = maxlen, batch_size = batch_size)
steps_per_epoch <- floor(((text_length - maxlen)/(batch_size * bychar))/epoch_splits)

gc(verbose = FALSE)
# Model definition --------------------------------------------------------

model <- keras_model_sequential()

model %>%
    layer_lstm(neuralnodes, input_shape = c(maxlen, chars_length)) %>%
    layer_dense(chars_length) %>%
    layer_activation("softmax")

optimizer <- optimizer_rmsprop(lr = lr)

model %>%
    compile(loss = "categorical_crossentropy", optimizer = optimizer)

model %>%
    save_model_hdf5('char_based_model')

# Training and results ----------------------------------------------------

sample_mod <- function(preds, temperature = 1) {
    preds <- log(preds)/temperature
    exp_preds <- exp(preds)
    preds <- exp_preds/sum(exp(preds))

    rmultinom(1, 1, preds) %>%
        as.integer() %>%
        which.max()
}

for (iteration in 1:iterations) {

    cat(sprintf("iteration: %02d ---------------\n\n", iteration))

    hist <- model %>%
        fit_generator(generator = tg,
                      steps_per_epoch = steps_per_epoch,
                      verbose = 1,
                      epochs = epoch_splits)

    model %>%
        save_model_weights_hdf5('char_based_model_weights')

    for (diversity in c(0.2, 0.5, 1, 1.2)) {

        cat(sprintf("diversity: %f ---------------\n\n", diversity))

        start_index <- sample(1:(text_length - maxlen), size = 1)
        sentence <- text[start_index:(start_index + maxlen - 1)]
        generated <- ""

        for (i in 1:400) {

            x <- sapply(chars, function(x) {
                as.integer(x == sentence)
            })
            dim(x) <- c(1, dim(x))

            preds <- predict(model, x)
            next_index <- sample_mod(preds, diversity)
            next_char <- chars[next_index]

            generated <- str_c(generated, next_char, collapse = "")
            sentence <- c(sentence[-1], next_char)

        }

        cat(generated)
        cat("\n\n")

        model %>%
            reset_states()
    }
}



