#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#define STB_DEFINE
#define STB_NO_REGISTRY
#pragma warning(push, 0)
#include "stb.h"
#pragma warning(pop)

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef u32 b32;

#define true 1
#define false 0

typedef struct {
	unsigned char* data;
	int size;
	int capacity;
	int item_size;
} array;

array new_array(void* memory, int memory_size, int item_size) {
	assert(memory);
	assert(memory_size);
	assert(item_size);

	return (array){ .data = (unsigned char*)memory, .item_size = item_size, .capacity = memory_size / item_size };
}

array malloc_array(int item_size, int items_count) {
	int array_data_size = item_size * items_count;
	unsigned char* array_data = malloc(array_data_size);
	assert(array_data);
	array result = new_array(array_data, array_data_size, item_size);
	assert(result.capacity == items_count);
	return result;
}

void free_array(array* arr) {
	free(arr->data);
	*arr = (array){0};
}

void assert_array_valid(array* arr) {
	assert(arr);
	assert(arr->data);
	assert(arr->capacity);
	assert(arr->item_size);
	assert(arr->size <= arr->capacity);
}

void* arr_push(array* arr) {
	assert_array_valid(arr);
	assert(arr->size < arr->capacity);
	void* new_item_ptr = arr->data + arr->size * arr->item_size;
	++arr->size;
	return new_item_ptr;
}

void arr_clear(array* arr) {
	assert_array_valid(arr);
	arr->size = 0;
}

void* arr_get(array* arr, int i) {
	assert_array_valid(arr);
	assert(i < arr->size);
	return arr->data + arr->item_size * i;
}

#define arr_foreach(arr, type, it_name, body) \
	{ for (int _foreach_ctr = 0; _foreach_ctr < (arr)->size; ++_foreach_ctr) { \
		type* it_name = arr_get((arr), _foreach_ctr); \
		{ body } \
	}}

#define arr_foreach_islast(arr) ((_foreach_ctr) == ((arr)->size - 1))

void arr_copy(array* dst, array* src) {
	assert_array_valid(dst);
	assert_array_valid(src);
	assert(dst->item_size == src->item_size);
	assert(dst->capacity >= src->size);
	memcpy(dst->data, src->data, src->size * src->item_size);
	dst->size = src->size;
}

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

b32 is_digit_sym(char c) {
	return c >= '0' && c <= '9';
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
	TK_STRUCT,
	TK_ENUM,
	TK_U32,
	TK_S32,
	TK_STRING,
	TK_NUMBER,
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
	TK_EQUALS,
	TK_UNKNOWN = -1
} TOKEN_TYPE;

const char* get_token_type_str(TOKEN_TYPE type) {
	switch (type) {
		case TK_EOF:
			return "TK_EOF";
		case TK_IDENTIFIER:
			return "TK_IDENTIFIER";
		case TK_FN:
			return "TK_FN";
		case TK_STRUCT:
			return "TK_STRUCT";
		case TK_ENUM:
			return "TK_ENUM";
		case TK_U32:
			return "TK_U32";
		case TK_S32:
			return "TK_S32";
		case TK_STRING:
			return "TK_STRING";
		case TK_NUMBER:
			return "TK_NUMBER";
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
		case TK_EQUALS:
			return "TK_EQUALS";
		case TK_UNKNOWN:
			return "TK_UNKNOWN";
	}

	return "{unexpected token type}";
}

typedef struct {
	string str;
	TOKEN_TYPE type;
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
	} else if (*sym.at == '=') {
		tk.type = TK_EQUALS;
	} else if (*sym.at == '/' && *next(ctx).at == '*') {
		while (!(*current(ctx).at == '*' && *next(ctx).at == '/')) {
			forward(ctx);
		}
		forward(ctx); forward(ctx);
		forward_before_returning = false;
		tk = next_token(ctx);
	} else if (is_digit_sym(*sym.at)) {
		tk.type = TK_NUMBER;
		b32 decimal_point_present = false;
		symbol s;
		while (true) {
			s = forward(ctx);
			if (*s.at == '.') {
				assert(!decimal_point_present);
				decimal_point_present = true;
				s = forward(ctx);
			}

			if (!is_digit_sym(*s.at)) {
				break;
			}
		}

		tk.str.len = s.at - sym.at;
		forward_before_returning = false;
	} else if (*sym.at == '"') {
		tk.type = TK_STRING;
		symbol s;
		s = forward(ctx);
		tk.str.at = s.at;
		while (*s.at != '"') { s = forward(ctx); } 
		tk.str.len = s.at - sym.at - 1;
	} else if (is_identifier_first_sym(*sym.at)) {
		tk.type = TK_IDENTIFIER;
		symbol s;
		do { s = forward(ctx); } while (is_identifier_sym(*s.at));
		tk.str.len = s.at - sym.at;
		forward_before_returning = false;

		if (!strncmp(tk.str.at, "fn", tk.str.len)) {
			tk.type = TK_FN;
		} else if (!strncmp(tk.str.at, "struct", tk.str.len)) {
			tk.type = TK_STRUCT;
		} else if (!strncmp(tk.str.at, "enum", tk.str.len)) {
			tk.type = TK_ENUM;
		} else if (!strncmp(tk.str.at, "u32", tk.str.len)) {
			tk.type = TK_U32;
		} else if (!strncmp(tk.str.at, "s32", tk.str.len)) {
			tk.type = TK_S32;
		}
	}

	if (forward_before_returning) {
		forward(ctx);
	}

	return tk;
}

token peek_token(source_ctx ctx) {
	return next_token(&ctx);
}

token require_token(source_ctx* ctx, TOKEN_TYPE required_type) {
	token tk = next_token(ctx);
	assert(tk.type == required_type);
	return tk;
}

typedef enum {
	VT_S32,
	VT_U32,
	VT_STRUCT,
	VT_ENUM,
	VT_UNKNOWN,
} VARIABLE_TYPE;

typedef struct {
	VARIABLE_TYPE type;
	string custom_type_name;
	string name;
} variable_sig;

b32 is_primitive(VARIABLE_TYPE type) {
	return type != VT_STRUCT && type != VT_ENUM;
}

VARIABLE_TYPE token_type_2_variable_type(TOKEN_TYPE type) {
	switch (type) {
		case TK_S32:
			return VT_S32;
		case TK_U32:
			return VT_U32;
	}

	assert(0 && "unexpected token type!");
	return VT_UNKNOWN;
}

typedef enum {
	ST_LITERAL_STR,
	ST_LITERAL_S32,
	ST_LITERAL_U32,
	ST_VAR_DECL,
	ST_FN,
	ST_FN_DECL,
	ST_FN_CALL,
	ST_IDENTIFIER,
	ST_ASSIGNMENT,
	ST_UNKNOWN,
} STATEMENT_TYPE;

const char* get_statement_type_str(STATEMENT_TYPE type) {
	switch (type) {
		case ST_LITERAL_STR:
			return "ST_LITERAL_STR";
		case ST_LITERAL_S32:
			return "ST_LITERAL_S32";
		case ST_LITERAL_U32:
			return "ST_LITERAL_U32";
		case ST_VAR_DECL:
			return "ST_VAR_DECL";
		case ST_FN:
			return "ST_FN";
		case ST_FN_DECL:
			return "ST_FN_DECL";
		case ST_FN_CALL:
			return "ST_FN_CALL";
		case ST_IDENTIFIER:
			return "ST_IDENTIFIER";
		case ST_ASSIGNMENT:
			return "ST_ASSIGNMENT";
		case ST_UNKNOWN:
			return "ST_UNKNOWN";
	};

	return "{unexpected statement type}";
}

typedef struct {
	string name;
	variable_sig* parameters;
	struct statement** statements;
} function_sig;

typedef struct {
	string name;
	struct statement** parameters;
} function_call;

typedef struct {
	string var_name;
	struct statement* statement;
} assignment;

typedef struct {
	string name;
} identifier;

typedef struct statement {
	STATEMENT_TYPE type;

	union {
		s32 value_s32;
		u32 value_u32;
		string value_str;

		variable_sig value_var_decl;

		function_sig value_fn_decl;
		function_call value_fn_call;

		assignment value_assignment;

		identifier value_identifier;
	};
} statement;

#define MAX_FUNCTIONS 1024
#define MAX_FUNCTION_STATEMENTS (1024 * 1024)
#define MAX_FUNCTION_PARAMETERS 64

function_sig* construct_function_sig(function_sig* f) {
	*f = (function_sig){0};
	return f;
}

statement* add_statement(function_sig* f, STATEMENT_TYPE type) {
	statement* result = *stb_arr_add(f->statements);
	*result = (statement){0};
	result->type = type;
	return result;
}

statement* malloc_statement(STATEMENT_TYPE type) {
	statement* result = malloc(sizeof(statement));
	*result = (statement){0};
	result->type = type;
	return result;
}

statement* parse_statement(source_ctx* ctx) {
	statement* st = malloc_statement(ST_UNKNOWN);
	token tk = next_token(ctx);
	if (tk.type == TK_IDENTIFIER) {
		token id_token = tk;
		tk = peek_token(*ctx);
		if (tk.type == TK_COLON) { // declaration of something
			require_token(ctx, TK_COLON);
			tk = next_token(ctx);
			if (tk.type == TK_FN) { // function
				st->type = ST_FN_DECL;
				function_sig* f = &st->value_fn_decl;
				f->name = id_token.str;

				tk = require_token(ctx, TK_LPAREN);
				do { // parameters
					tk = next_token(ctx);
					// TODO: parameters parsing
				} while (tk.type != TK_RPAREN);

				// TODO: return value type parsing
				while (tk.type != TK_LBRACE) { tk = next_token(ctx); }
				while (true) { // body
					tk = peek_token(*ctx);
					if (tk.type == TK_RBRACE) {
						break;
					}

					stb_arr_push(f->statements, parse_statement(ctx));
				}
				require_token(ctx, TK_RBRACE);
			} else if (is_primitive(tk.type)) { // primitive type
				st->type = ST_VAR_DECL;
				variable_sig* var = &st->value_var_decl;
				var->name = id_token.str;
				var->type = token_type_2_variable_type(tk.type);
				// TODO: add support for in place initialization
				require_token(ctx, TK_SEMICOLON);
			}
		} else if (tk.type == TK_LPAREN) { // function call
			require_token(ctx, TK_LPAREN);
			st->type = ST_FN_CALL;
			function_call* f = &st->value_fn_call;
			f->name = id_token.str;
			tk = peek_token(*ctx);
			if (tk.type != TK_RPAREN) {
				stb_arr_push(f->parameters, parse_statement(ctx));
				while (true) {
					tk = peek_token(*ctx);
					if (tk.type == TK_RPAREN) {
						break;
					} else {
						require_token(ctx, TK_COMMA);
						stb_arr_push(f->parameters, parse_statement(ctx));
					}
				}
			}
			require_token(ctx, TK_RPAREN);
			require_token(ctx, TK_SEMICOLON);
		} else if (tk.type == TK_EQUALS) { // assignment
			require_token(ctx, TK_EQUALS);
			st->type = ST_ASSIGNMENT;
			assignment* a = &st->value_assignment;
			a->var_name = id_token.str;
			a->statement = parse_statement(ctx);
			require_token(ctx, TK_SEMICOLON);
		} else {
			st->type = ST_IDENTIFIER;
			identifier* id = &st->value_identifier;
			id->name = id_token.str;
		}
	} else if (tk.type == TK_NUMBER) { // number literal
		// TODO: actual number parsing instead of this bullshit
		st->type = ST_LITERAL_S32;
		st->value_s32 = atoi(tk.str.at);
	} else if (tk.type == TK_STRING) { // string literal
		st->type = ST_LITERAL_STR;
		st->value_str = tk.str;
	}

	return st;
}

void print_ast(statement* root) {
	if (root->type == ST_LITERAL_S32) {
		printf("%d", root->value_s32);
	} else if (root->type == ST_LITERAL_U32) {
		printf("%u", root->value_u32);
	} else if (root->type == ST_LITERAL_STR) {
		printf("\"%.*s\"", root->value_str.len, root->value_str.at);
	} else if (root->type == ST_IDENTIFIER) {
		printf("%.*s", root->value_identifier.name.len, root->value_identifier.name.at);
	} else if (root->type == ST_VAR_DECL) {
		variable_sig* var = &root->value_var_decl;
		printf("%.*s : ", var->name.len, var->name.at);
		switch (var->type) {
			case VT_S32: {
				printf("s32");
			} break;
			case VT_U32: {
				printf("u32");
			} break;
			case VT_STRUCT:
			case VT_ENUM: {
				printf("%.*s", var->custom_type_name.len, var->custom_type_name.at);
			} break;
			default: {
				printf("{unknown var type}");
			} break;
		}
	} else if (root->type == ST_FN_DECL) {
		function_sig* f = &root->value_fn_decl;
		// TODO: parameters printing
		printf("%.*s : fn() {", f->name.len, f->name.at);
		for (u32 i = 0; i < stb_arr_len(f->statements); ++i) {
			printf("\n\t");
			print_ast(f->statements[i]);
		}
		printf("\n}\n");
	} else if (root->type == ST_FN_CALL) {
		function_call* f = &root->value_fn_call;
		printf("%.*s(", f->name.len, f->name.at);
		for (u32 i = 0, n = stb_arr_len(f->parameters); i < n; ++i) {
			print_ast(f->parameters[i]);
			if (i != n - 1) {
				printf(", ");
			}
		}
		printf(")");
	} else if (root->type == ST_ASSIGNMENT) {
		assignment* a = &root->value_assignment;
		printf("%.*s = ", a->var_name.len, a->var_name.at);
		print_ast(a->statement);
	} else if (root->type == ST_UNKNOWN) {
		printf("ST_UNKNOWN");
	} else {
		printf("{unknown node type}");
	}
}

int main(int argc, char** argv) {
	char* source = load_text_file("test.cn");
	assert(source);

	source_ctx ctx = { .source = source, .at = source, .iline = 0, .icol = 0 };
	token tk;
#if 0 // print all tokens
	printf("#if 0\n");
	do {
		tk = next_token(&ctx);
		printf("%3d:%-3d %-15s %.*s\n", tk.iline + 1, tk.icol + 1, get_token_type_str(tk.type),
				tk.str.len, tk.str.at);
	} while (tk.type != TK_EOF);
	printf("#endif\n");
#endif

#if 1
	ctx = (source_ctx){ .source = source, .at = source, .iline = 0, .icol = 0 };
	tk = peek_token(ctx);
	statement** statements = 0;
	while (tk.type != TK_EOF) {
		stb_arr_push(statements, parse_statement(&ctx));
		tk = peek_token(ctx);
	}
#endif

#if 1
	for (u32 i = 0; i < stb_arr_len(statements); ++i) {
		print_ast(statements[i]);
	}
#endif

	return 0;
}
