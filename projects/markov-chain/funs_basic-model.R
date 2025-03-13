# Markov Chain Text Generator
# This script creates a simple Markov chain text generator
# with configurable "temperature" for controlling randomness

library(tidyverse)

# Function to build a Markov model from input text
build_markov_model_basic <- function(text) {
  # Split text into sentences
  sentences <- unlist(str_split(text, "[.!?]\\s+"))
  sentences <- sentences[sentences != ""]
  
  # Add sentence start/end tokens
  sentences <- paste0("<START> ", sentences, " <END>")
  
  # Create a dataframe of n-grams
  model_df <- tibble(sentence = sentences) |>
    mutate(words = map(sentence, ~unlist(str_split(.x, "\\s+")))) |>
    unnest(words) |>
    mutate(next_word = lead(words, 1)) |>
    filter(!is.na(next_word))
  
  # Group by current word and count occurrences of next words
  transition_probs <- model_df |>
    group_by(words) |>
    count(next_word) |>
    mutate(probability = n / sum(n)) |>
    ungroup()
  
  return(transition_probs)
}

# Function to generate text using the Markov model with temperature control
generate_text_basic <- function(model, num_sentences = 5, max_length = 50, temperature = 1.0) {
  generated_sentences <- vector("character", num_sentences)
  
  for (i in 1:num_sentences) {
    # Start with the <START> token
    current_word <- "<START>"
    sentence <- character(0)
    word_count <- 0
    
    # Generate words until we hit <END> or max length
    while (current_word != "<END>" && word_count < max_length) {
      # Get possible next words and their probabilities
      next_words <- model |>
        filter(words == current_word)
      
      # If no next words found, end the sentence
      if (nrow(next_words) == 0) {
        break
      }
      
      # Apply temperature to adjust randomness
      if (temperature != 1.0) {
        # Adjust probabilities based on temperature
        # Lower temperature makes high probability words more likely
        # Higher temperature makes distribution more uniform
        next_words <- next_words |>
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
      
      # Update current word and increment counter
      current_word <- next_word
      word_count <- word_count + 1
    }
    
    # Join words into a sentence
    generated_sentences[i] <- paste(sentence, collapse = " ")
  }
  
  return(generated_sentences)
}
