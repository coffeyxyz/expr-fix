// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"
#include "rtb_buffer.h"
#include "token.h"

#define DEFAULT_BUFFER_SIZE	3
#define	VALID_OPERATIONS	"+-*/"

rtb_buffer_t *expr_to_tokens(char *expr)
{
	rtb_buffer_t *buffer = rtb_buffer_init(DEFAULT_BUFFER_SIZE);

	if (*expr == '\0')
		return buffer;

	token_t *token = NULL;
	char *token_str = NULL;

	token_str = strtok(expr, " ");
	while (token_str != NULL) {

		token = malloc(sizeof token);
		if (!token)
			return NULL;

		token->str = token_str;
		if (isdigit(*token_str)) {
			token->flags |= IS_NUMBER;
		}
		else if (strchr(VALID_OPERATIONS, *token_str)) {
			if (*(token_str+1) && isdigit(*(token_str+1)))
				token->flags |= IS_NUMBER;
			else
				token->flags |= IS_OPERATOR;
		}
		else {
			return NULL;
		}

		rtb_buffer_push(buffer, token);

		token_str = strtok(NULL, " ");
	}

	return buffer;
}
