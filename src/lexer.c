// Copyright (C) 2021 Robert Coffey
// All rights reserved

#include <stdio.h>
#include <string.h>

#include "lexer.h"

rtb_buffer_t *string_to_tokens(char *expr)
{
	rtb_buffer_t *buffer = rtb_buffer_init(4);

	for (int i = 0; i < strlen(expr); ++i) {

	}

	return buffer;
}
