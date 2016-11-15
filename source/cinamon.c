#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

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

typedef enum {
	TK_EOF = 0,
	TK_IDENTIFIER,
	TK_FN,
	TK_LBRACE,
	TK_RBRACE,
	TK_LPAREN,
	TK_RPAREN,
} token_type;

typedef struct {
	string str;
	token_type type;
	int iline, icol;
} token;

typedef struct {
	string* lines;
	int nlines;
	int iline, icol;
} source_ctx;

void construct_source_ctx(source_ctx* ctx, char* source) {
	int nlines = 1;
	for (char *at = source; *at; ++at) {
		if (*at == '\n') {
			++nlines;
		}
	}

	string* lines = malloc(nlines * sizeof(*lines));
	char* at = source;
	for (int iline = 0; iline < nlines; ++iline) {
		string line = { .at = at };
		while (*at && *at != '\n') { ++at; }
		++at;
		line.len = at - source;
		lines[iline] = line;
	}

	ctx->lines = lines;
	ctx->nlines = nlines;
	ctx->iline = ctx->icol = 0;
}

typedef struct {
	char c;
	int iline, icol;
} symbol;

symbol current(source_ctx* ctx) {
	string line = ctx->lines[ctx->iline];
	symbol result = { .c = line.at[ctx->icol], .iline = ctx->iline, .icol = ctx->icol};
	return result;
}

void forward(source_ctx* ctx) {
	string line = ctx->lines[ctx->iline];
	assert(!((ctx->iline == ctx->nlines - 1) && (ctx->icol == line.len - 1)) && "past the end of source");
	++ctx->icol;
	if (ctx->icol >= line.len - 1) {
		++ctx->iline;
		assert(ctx->iline < ctx->nlines);
		ctx->icol = 0;
	}
}

void forwardn(source_ctx* ctx, int n) {
	for (int i = 0; i < n; ++i) {
		forward(ctx);
	}
}

void back(source_ctx* ctx) {
	--ctx->icol;
	if (ctx->icol < 0) {
		--ctx->iline;
		assert(ctx->iline >= 0);
		ctx->icol = ctx->lines[ctx->iline].len - 1;
	}
}

void backn(source_ctx* ctx, int n) {
}

void eat_whitespace(source_ctx* ctx) {
	symbol sym = peek(ctx);
}

token next_token(source_ctx* ctx) {
}

int main(int argc, char** argv) {
	char* source = load_text_file("test.cn");
	assert(source);

	source_ctx src_ctx;
	construct_source_ctx(&src_ctx, source);
	for (int i = 0; i < src_ctx.nlines; ++i) {
		string line = src_ctx.lines[i];
		printf("%.*s", line.len, line.at);
	}

	return 0;
}
