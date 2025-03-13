# Markov Chain Text Generator
# This script creates a simple Markov chain text generator
# with configurable "temperature" for controlling randomness

library(tidyverse)

# Function to build a Markov model from input text
build_markov_model <- function(text, n = 1) {
  # Split text into sentences
  sentences <- unlist(str_split(text, "[.!?]\\s+"))
  sentences <- sentences[sentences != ""]
  
  # Add sentence start/end tokens
  sentences <- paste0(paste(rep("<START>", n), collapse = " "), " ", sentences, " <END>")
  
  # Create a dataframe of words
  words_df <- tibble(sentence = sentences) %>%
    mutate(words = map(sentence, ~unlist(str_split(.x, "\\s+")))) %>%
    unnest(words)
  
  # Create n-grams and next words
  model_df <- words_df
  
  if (n == 1) {
    # For unigram model (standard Markov chain)
    model_df <- model_df %>%
      mutate(next_word = lead(words, 1)) %>%
      filter(!is.na(next_word))
    
    # Group by current word and count occurrences of next words
    transition_probs <- model_df %>%
      group_by(words) %>%
      count(next_word) %>%
      mutate(
        probability = n / sum(n),
        # Add a context column for consistency with n > 1 models
        context = words
      ) %>%
      ungroup() %>%
      # Reorder columns to match n > 1 format
      select(context, next_word, n, probability)
  } else {
    # For n-gram models (n > 1)
    # Create a column with the n-gram context (n previous words)
    model_df <- model_df %>%
      mutate(
        index = row_number(),
        context = map2(index, index, ~{
          if (.x >= n) {
            context_indices <- (.x - (n - 1)):.x
            paste(words_df$words[context_indices], collapse = " ")
          } else {
            NA_character_
          }
        }),
        next_word = lead(words, 1)
      ) %>%
      filter(!is.na(context), !is.na(next_word))
    
    # Group by n-gram context and count occurrences of next words
    transition_probs <- model_df %>%
      group_by(context) %>%
      count(next_word) %>%
      mutate(probability = n / sum(n)) %>%
      ungroup() %>%
      mutate(current_phrase = paste(context), .after = context)
  }
  
  return(transition_probs)
}

# Function to generate text using the Markov model with temperature control
generate_text <- function(model, num_sentences = 5, max_length = 50, temperature = 1.0, n = NULL) {
  # Auto-detect n-gram size if not provided
  if (is.null(n)) {
    # Check a sample of contexts to determine n-gram size
    sample_contexts <- head(unique(model$context), 20)
    # Count spaces to determine number of words in context
    detected_n <- max(stringr::str_count(sample_contexts, "\\s+")) + 1
    n <- detected_n
    
    # If detected n is too high (likely due to spaces in single words), default to 1
    if (n > 10) n <- 1
    
    # if (n > 1) {
    #   message("Auto-detected n-gram size: ", n)
    # }
  } else {
    # Check if provided n matches the model structure
    sample_contexts <- head(unique(model$context), 20)
    # For n=1, contexts should be single words (no spaces except for multi-word tokens)
    # For n>1, contexts should have n-1 spaces (for n words)
    expected_spaces <- ifelse(n == 1, 0, n-1)
    
    # Count spaces in each context and check if they're consistent with expected n
    space_counts <- stringr::str_count(sample_contexts, "\\s+")
    most_common_spaces <- as.numeric(names(sort(table(space_counts), decreasing = TRUE)[1]))
    
    if (most_common_spaces != expected_spaces) {
      warning("The n value (", n, ") doesn't match the model structure. ",
              "Auto-detecting n-gram size instead.")
      n <- most_common_spaces + 1
      message("Using n-gram size: ", n)
    }
  }
  
  generated_sentences <- vector("character", num_sentences)
  
  for (i in 1:num_sentences) {
    # Start with appropriate number of <START> tokens based on n
    if (n == 1) {
      current_context <- "<START>"
    } else {
      context_words <- rep("<START>", n)
      current_context <- paste(context_words, collapse = " ")
    }
    
    sentence <- character(0)
    word_count <- 0
    
    # Generate words until we hit <END> or max length
    while (!str_detect(current_context, "<END>$") && word_count < max_length) {
      # Get possible next words and their probabilities
      next_words <- model %>%
        filter(context == current_context)
      
      # If no next words found, end the sentence
      if (nrow(next_words) == 0) {
        # Try more generic context for higher n-gram models
        if (n > 1) {
          # Extract the last word from current context
          last_word <- stringr::str_extract(current_context, "[^ ]+$")
          next_words <- model %>%
            filter(context == last_word)
          
          if (nrow(next_words) == 0) {
            # Still no match, pick a random <START> context
            start_contexts <- model %>% 
              filter(stringr::str_detect(context, "^<START>"))
            
            if (nrow(start_contexts) > 0) {
              random_start <- sample(start_contexts$context, 1)
              next_words <- model %>% 
                filter(context == random_start)
            } else {
              break
            }
          }
        } else {
          break
        }
      }
      
      # Apply temperature to adjust randomness
      if (temperature != 1.0) {
        # Adjust probabilities based on temperature
        next_words <- next_words %>%
          mutate(
            # Avoid log(0) by adding a small epsilon
            log_prob = log(probability + 1e-10),
            adjusted_prob = exp(log_prob / temperature),
            # Renormalize to ensure probabilities sum to 1
            adjusted_prob = adjusted_prob / sum(adjusted_prob)
          )
        
        # Sample next word according to adjusted probabilities
        next_word <- sample(
          next_words$next_word, 
          size = 1, 
          prob = next_words$adjusted_prob
        )
      } else {
        # Sample according to original probabilities
        next_word <- sample(
          next_words$next_word, 
          size = 1, 
          prob = next_words$probability
        )
      }
      
      # Add word to sentence if it's not an end token
      if (next_word != "<END>") {
        sentence <- c(sentence, next_word)
      }
      
      # Update context for next iteration
      if (n == 1) {
        current_context <- next_word
      } else {
        # For n-gram models, update the context window
        context_words <- c(context_words[-1], next_word)
        current_context <- paste(context_words, collapse = " ")
      }
      
      word_count <- word_count + 1
    }
    
    # Join words into a sentence
    generated_sentences[i] <- paste(sentence, collapse = " ")
  }
  
  return(generated_sentences)
}
