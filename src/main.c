// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <stdio.h>

#define RTB_DEFINE
#include "rtb_buffer.h"

#include "lexer.h"
#include "token.h"

int main(void)
{
	char expr[] = "1 + -1 * 2";

	rtb_buffer_t *buffer = string_to_tokens(expr);

	for (size_t i = 0; i < buffer->end; ++i) {
		token_t *token = (token_t *)rtb_buffer_at(buffer, i);
		if (i) printf("\n");
		printf("STRING:\t%s\nFLAGS:\t%x\n", token->str, token->flags);
	}

	return 0;
}
