// Copyright (C) 2021 Robert Coffey
// All rights reserved

#ifndef TOKEN_H
#define TOKEN_H

#define IS_NUMBER (1 << 1)
#define IS_OPERATOR (1 << 2)

typedef struct token {
	unsigned char flags;
	char *str;
} token_t;

#endif // TOKEN_H
