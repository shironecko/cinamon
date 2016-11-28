#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

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
		case TK_STRUCT:
			return "TK_STRUCT";
		case TK_ENUM:
			return "TK_ENUM";
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
		}
	}

	if (forward_before_returning) {
		forward(ctx);
	}

	return tk;
}

token require_token(source_ctx* ctx, token_type required_type) {
	token tk = next_token(ctx);
	assert(tk.type == required_type);
	return tk;
}

typedef enum {
	VT_S32,
	VT_U32,
	VT_STRUCT,
	VT_ENUM,
} VARIABLE_TYPE;

typedef struct {
	VARIABLE_TYPE type;
	string struct_name;
} data_type;

typedef struct {
	data_type type;
	string name;
} variable;

typedef struct {
	string name;
	array parameters;
	array statements;
} function;

typedef enum {
	ST_LITERAL_STR,
	ST_LITERAL_S32,
	ST_LITERAL_U32,
	ST_CALL,
} STATEMENT_TYPE;

const char* get_statement_type_str(STATEMENT_TYPE type) {
	switch (type) {
		case ST_LITERAL_STR:
			return "ST_LITERAL_STR";
		case ST_LITERAL_S32:
			return "ST_LITERAL_S32";
		case ST_LITERAL_U32:
			return "ST_LITERAL_U32";
		case ST_CALL:
			return "ST_CALL";
	};

	return "{unexpected statement type}";
}

typedef struct {
	STATEMENT_TYPE type;

	union {
		s32 value_s32;
		u32 value_u32;
		string value_str;

		struct {
			string fn_name;
			array parameters;
		} value_call;
	};
} statement;

#define MAX_FUNCTIONS 1024
#define MAX_FUNCTION_STATEMENTS (1024 * 1024)
#define MAX_FUNCTION_PARAMETERS 64

function* construct_function(function* f) {
	f->name = (string){0};
	f->parameters = malloc_array(sizeof(variable), MAX_FUNCTION_PARAMETERS);
	f->statements = malloc_array(sizeof(statement), MAX_FUNCTION_STATEMENTS);
	return f;
}

statement* add_statement(function* f, STATEMENT_TYPE type) {
	statement* result = arr_push(&f->statements);
	*result = (statement){0};
	result->type = type;

	if (type == ST_CALL) {
		result->value_call.parameters = malloc_array(sizeof(statement), MAX_FUNCTION_PARAMETERS);
	}

	return result;
}

int main(int argc, char** argv) {
	char* source = load_text_file("test.cn");
	assert(source);

	array functions = malloc_array(sizeof(function), MAX_FUNCTIONS);

	source_ctx ctx = { .source = source, .at = source, .iline = 0, .icol = 0 };
	token tk;
#if 0
	do {
		tk = next_token(&ctx);
		printf("%3d:%-3d %-15s %.*s\n", tk.iline + 1, tk.icol + 1, get_token_type_str(tk.type),
				tk.str.len, tk.str.at);
	} while (tk.type != TK_EOF);
#endif

	ctx = (source_ctx){ .source = source, .at = source, .iline = 0, .icol = 0 };
	do {
		tk = next_token(&ctx);
		if (tk.type == TK_IDENTIFIER) {
			token id_token = tk;
			tk = next_token(&ctx);
			if (tk.type == TK_COLON) { // declaration of something
				tk = next_token(&ctx);
				switch (tk.type) {
					case TK_FN: { // function
						function* f = construct_function(arr_push(&functions));
						f->name = id_token.str;

						tk = require_token(&ctx, TK_LPAREN);
						do { // parameters
							tk = next_token(&ctx);
							// TODO: parameters parsing
						} while (tk.type != TK_RPAREN);

						tk = require_token(&ctx, TK_LBRACE);
						do { // body
							tk = next_token(&ctx);
							if (tk.type == TK_IDENTIFIER) {
								token st_id = tk;
								tk = next_token(&ctx);
								if (tk.type == TK_LPAREN) { // fn call
									statement* call = add_statement(f, ST_CALL);
									call->value_call.fn_name = st_id.str;
									do { // parameters
										tk = next_token(&ctx);
										if (tk.type == TK_STRING) {
											statement* str_param = arr_push(&call->value_call.parameters);
											str_param->type = ST_LITERAL_STR;
											str_param->value_str = tk.str;
										} else if (tk.type == TK_NUMBER) {
											statement* num_param = arr_push(&call->value_call.parameters);
											// TODO: non s32 numbers
											num_param->type = ST_LITERAL_S32;
											num_param->value_s32 = atoi(tk.str.at);
										}

										if (tk.type == TK_COMMA) { tk = next_token(&ctx); }
									} while (tk.type != TK_RPAREN);

									// TODO: add support for compound statements
									require_token(&ctx, TK_SEMICOLON);
								}
							}
						} while (tk.type != TK_RBRACE);
					} break;
					case TK_STRUCT: {
					} break;
					case TK_ENUM: {
					} break;
					default: {
						assert(0 && "unexpected type!");
					} break;
				}
			}
		}
	} while (tk.type != TK_EOF);

	// output C
	printf(
		"#include <stdio.h>\n"
		"#include <stdint.h>\n"
		"#include <assert.h>\n"
		"typedef int8_t s8;\n"
		"typedef int16_t s16;\n"
		"typedef int32_t s32;\n"
		"typedef int64_t s64;\n"
		"typedef uint8_t u8;\n"
		"typedef uint16_t u16;\n"
		"typedef uint32_t u32;\n"
		"typedef uint64_t u64;\n"
		"typedef u32 b32;\n"
		"#define true 1\n"
		"#define false 0\n\n");

	arr_foreach(&functions, function, fn, {
		printf("%.*s() {\n", fn->name.len, fn->name.at);
		arr_foreach(&fn->statements, statement, st, {
			switch (st->type) {
				case ST_CALL: {
					printf("\t%.*s(", st->value_call.fn_name.len, st->value_call.fn_name.at);
					arr_foreach(&st->value_call.parameters, statement, param, {
						switch (param->type) {
							case ST_LITERAL_STR: {
								printf("\"%.*s\"", param->value_str.len, param->value_str.at);
							} break;
							case ST_LITERAL_S32: {
								printf("%d", param->value_s32);
							} break;
						};

						if (!arr_foreach_islast(&st->value_call.parameters)) {
							printf(", ");
						}
					});
					printf(");\n");
				} break;
			}
		});
		printf("}\n\n");
	});

	return 0;
}
