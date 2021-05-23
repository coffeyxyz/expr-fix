// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef LEXER_H
#define LEXER_H

#include "rtb_buffer.h"

/**
 * Get the sequence of tokens contained within a string. Tokens in the
 * string are expected to be separated by one or more spaces.
 *
 * @param expr	String to derive tokens from
 *
 * @return rtb_buffer_t containing tokens
 **/
rtb_buffer_t *string_to_tokens(char *expr);

#endif	// LEXER_H
