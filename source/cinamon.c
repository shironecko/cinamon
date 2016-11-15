#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

typedef int b32;

#define true 1
#define false 0

char* load_text_file(const char* path) {
	FILE* file = fopen(path, "rb");
	if (!file)
		return 0;

	fseek(file, 0, SEEK_END);
	int file_size = ftell(file);
	fseek(file, 0, SEEK_SET);
	char* contents = (char*)malloc(file_size + 1);
	fread(contents, 1, file_size, file);
	contents[file_size] = 0;
	fclose(file);
	return contents;
}

typedef struct {
	char* at;
	int len;
} string;

typedef struct {
	char *source;
	char *at;
	int iline, icol;
} source_ctx;

typedef struct {
	char* at;
	int iline, icol;
} symbol;

symbol current(source_ctx* ctx) {
	symbol result = { .at = ctx->at, .iline = ctx->iline, .icol = ctx->icol};
	return result;
}

symbol next(source_ctx* ctx) {
	symbol result = { .at = ctx->at + 1, .iline = ctx->iline, .icol = ctx->icol + 1 };
	if (*ctx->at == '\n') {
		++result.iline;
		result.icol = 0;
	}

	return result;
}

symbol forward(source_ctx* ctx) {
	char current = *ctx->at;
	assert(current);
	++ctx->at;
	++ctx->icol;
	if (current == '\n') {
		++ctx->iline;
		ctx->icol = 0;
	}

	symbol result = { .at = ctx->at, .iline = ctx->iline, .icol = ctx->icol };
	return result;
}

void forwardn(source_ctx* ctx, int n) {
	for (int i = 0; i < n; ++i) {
		forward(ctx);
	}
}

b32 is_identifier_first_sym(char c) {
	b32 result = 
		(c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		c == '_';

	return result;
}

b32 is_identifier_sym(char c) {
	b32 result = 
		is_identifier_first_sym(c) ||
		(c >= '0' && c <= '9');

	return result;
}

b32 is_whitespace(char c) {
	b32 result = c == ' ' || c == '\t' || c == '\r' || c == '\n';
	return result;
}

void eat_whitespace(source_ctx* ctx) {
	symbol sym = current(ctx);
	while (is_whitespace(*sym.at)) { 
		sym = forward(ctx); 
	}
}

typedef enum {
	TK_EOF = 0,
	TK_IDENTIFIER,
	TK_FN,
	TK_LET,
	TK_LBRACE,
	TK_RBRACE,
	TK_LPAREN,
	TK_RPAREN,
	TK_LBRACKET,
	TK_RBRACKET,
	TK_SEMICOLON,
	TK_COLON,
	TK_DOT,
	TK_COMMA,
	TK_HASH,
	TK_UNKNOWN = -1
} token_type;

const char* get_token_type_str(token_type type) {
	switch (type) {
		case TK_EOF:
			return "TK_EOF";
		case TK_IDENTIFIER:
			return "TK_IDENTIFIER";
		case TK_FN:
			return "TK_FN";
		case TK_LET:
			return "TK_LET";
		case TK_LBRACE:
			return "TK_LBRACE";
		case TK_RBRACE:
			return "TK_RBRACE";
		case TK_LPAREN:
			return "TK_LPAREN";
		case TK_RPAREN:
			return "TK_RPAREN";
		case TK_LBRACKET:
			return "TK_LBRACKET";
		case TK_RBRACKET:
			return "TK_RBRACKET";
		case TK_SEMICOLON:
			return "TK_SEMICOLON";
		case TK_COLON:
			return "TK_COLON";
		case TK_DOT:
			return "TK_DOT";
		case TK_COMMA:
			return "TK_COMMA";
		case TK_HASH:
			return "TK_HASH";
		case TK_UNKNOWN:
			return "TK_UNKNOWN";
	}

	return "{unexpected token type}";
}

typedef struct {
	string str;
	token_type type;
	int iline, icol;
} token;

token next_token(source_ctx* ctx) {
	eat_whitespace(ctx);

	symbol sym = current(ctx);
	token tk = { .str = { .at = sym.at, .len = 1 }, .type = TK_UNKNOWN, .iline = sym.iline, .icol = sym.icol };
	b32 forward_before_returning = true;
	if (!*sym.at) {
		tk.type = TK_EOF;
		forward_before_returning = false;
	} else if (*sym.at == '{') {
		tk.type = TK_LBRACE;
	} else if (*sym.at == '}') {
		tk.type = TK_RBRACE;
	} else if (*sym.at == '(') {
		tk.type = TK_LPAREN;
	} else if (*sym.at == ')') {
		tk.type = TK_RPAREN;
	} else if (*sym.at == '[') {
		tk.type = TK_LBRACKET;
	} else if (*sym.at == ']') {
		tk.type = TK_RBRACKET;
	} else if (*sym.at == ';') {
		tk.type = TK_SEMICOLON;
	} else if (*sym.at == ':') {
		tk.type = TK_COLON;
	} else if (*sym.at == '.') {
		tk.type = TK_DOT;
	} else if (*sym.at == ',') {
		tk.type = TK_COMMA;
	} else if (*sym.at == '#') {
		tk.type = TK_HASH;
	} else if (is_identifier_first_sym(*sym.at)) {
		symbol s;
		do { s = forward(ctx); } while (is_identifier_sym(*s.at));
		tk.str.len = s.at - sym.at;
		forward_before_returning = false;

		tk.type = TK_IDENTIFIER;
		if (!strncmp(tk.str.at, "fn", tk.str.len)) {
			tk.type = TK_FN;
		} else if (!strncmp(tk.str.at, "let", tk.str.len)) {
			tk.type = TK_LET;
		}
	}

	if (forward_before_returning) {
		forward(ctx);
	}

	return tk;
}

int main(int argc, char** argv) {
	char* source = load_text_file("test.cn");
	assert(source);

	source_ctx ctx = { .source = source, .at = source, .iline = 0, .icol = 0 };
	token tk;
	do {
		tk = next_token(&ctx);
		printf("%3d:%-3d %-15s %.*s\n", tk.iline + 1, tk.icol + 1, get_token_type_str(tk.type),
				tk.str.len, tk.str.at);
	} while (tk.type != TK_EOF);

	return 0;
}
