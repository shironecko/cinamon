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

/*******************************ENUM SIGNATURES*******************************/

#define META_ENUM_MEMBER(member_name) member_name,

#define META_EMIT_ENUM_SIGNATURE(enum_members, enum_name) \
	typedef enum enum_name { \
		enum_members \
	} enum_name;

#define META_TOKEN_TYPE_MEMBERS \
	META_ENUM_MEMBER(TK_EOF) \
	META_ENUM_MEMBER(TK_IDENTIFIER) \
	META_ENUM_MEMBER(TK_FN) \
	META_ENUM_MEMBER(TK_STRUCT) \
	META_ENUM_MEMBER(TK_ENUM) \
	META_ENUM_MEMBER(TK_U32) \
	META_ENUM_MEMBER(TK_S32) \
	META_ENUM_MEMBER(TK_STRING) \
	META_ENUM_MEMBER(TK_NUMBER) \
	META_ENUM_MEMBER(TK_LBRACE) \
	META_ENUM_MEMBER(TK_RBRACE) \
	META_ENUM_MEMBER(TK_LPAREN) \
	META_ENUM_MEMBER(TK_RPAREN) \
	META_ENUM_MEMBER(TK_LBRACKET) \
	META_ENUM_MEMBER(TK_RBRACKET) \
	META_ENUM_MEMBER(TK_SEMICOLON) \
	META_ENUM_MEMBER(TK_COLON) \
	META_ENUM_MEMBER(TK_DOT) \
	META_ENUM_MEMBER(TK_COMMA) \
	META_ENUM_MEMBER(TK_HASH) \
	META_ENUM_MEMBER(TK_ADD) \
	META_ENUM_MEMBER(TK_SUB) \
	META_ENUM_MEMBER(TK_MUL) \
	META_ENUM_MEMBER(TK_DIV) \
	META_ENUM_MEMBER(TK_EQUALS) \
	META_ENUM_MEMBER(TK_ARROW) \
	META_ENUM_MEMBER(TK_UNKNOWN)

#define META_STATEMENT_TYPE_MEMBERS \
	META_ENUM_MEMBER(ST_LITERAL_STR) \
	META_ENUM_MEMBER(ST_LITERAL_S32) \
	META_ENUM_MEMBER(ST_LITERAL_U32) \
	META_ENUM_MEMBER(ST_VAR_DECL) \
	META_ENUM_MEMBER(ST_FN) \
	META_ENUM_MEMBER(ST_FN_DECL) \
	META_ENUM_MEMBER(ST_FN_CALL) \
	META_ENUM_MEMBER(ST_IDENTIFIER) \
	META_ENUM_MEMBER(ST_ASSIGNMENT) \
	META_ENUM_MEMBER(ST_BINARY_OP) \
	META_ENUM_MEMBER(ST_VOID) \
	META_ENUM_MEMBER(ST_UNKNOWN)

#define META_BINARY_OP_TYPE_MEMBERS \
	META_ENUM_MEMBER(BO_ADD) \
	META_ENUM_MEMBER(BO_SUB) \
	META_ENUM_MEMBER(BO_MUL) \
	META_ENUM_MEMBER(BO_DIV) \
	META_ENUM_MEMBER(BO_ASSIGNMENT)

#define META_VARIABLE_TYPE_MEMBERS \
	META_ENUM_MEMBER(VT_VOID) \
	META_ENUM_MEMBER(VT_S32) \
	META_ENUM_MEMBER(VT_U32) \
	META_ENUM_MEMBER(VT_STRUCT) \
	META_ENUM_MEMBER(VT_ENUM) \
	META_ENUM_MEMBER(VT_UNKNOWN)

META_EMIT_ENUM_SIGNATURE(META_TOKEN_TYPE_MEMBERS, TOKEN_TYPE);
META_EMIT_ENUM_SIGNATURE(META_STATEMENT_TYPE_MEMBERS, STATEMENT_TYPE);
META_EMIT_ENUM_SIGNATURE(META_BINARY_OP_TYPE_MEMBERS, BINARY_OP_TYPE);
META_EMIT_ENUM_SIGNATURE(META_VARIABLE_TYPE_MEMBERS, VARIABLE_TYPE);

/***************************ENUM TO STRING FUNCTIONS***************************/

#undef META_ENUM_MEMBER
#define META_ENUM_MEMBER(member_name) #member_name,

#define META_EMIT_ENUM_TO_STR_FN(enum_members, enum_name) \
	const char* enum_name##_STR(enum_name value) { \
		char* strings[] = { enum_members }; \
		u32 nstrings = sizeof(strings) / sizeof(*strings); \
		if (value < nstrings) { \
			return strings[value]; \
		} \
		return "{outside of " #enum_name " range}"; \
	}

META_EMIT_ENUM_TO_STR_FN(META_TOKEN_TYPE_MEMBERS, TOKEN_TYPE);
META_EMIT_ENUM_TO_STR_FN(META_STATEMENT_TYPE_MEMBERS, STATEMENT_TYPE);
META_EMIT_ENUM_TO_STR_FN(META_BINARY_OP_TYPE_MEMBERS, BINARY_OP_TYPE);
META_EMIT_ENUM_TO_STR_FN(META_VARIABLE_TYPE_MEMBERS, VARIABLE_TYPE);

/***************************COMMON DATA TYPES****************************/

typedef struct {
	char* at;
	int len;
} string;

typedef struct {
	string str;
	TOKEN_TYPE type;
	int iline, icol;
} token;

/*********************************LEXING*********************************/

typedef struct {
	char *source;
	char *at;
	int iline, icol;
} lexing_ctx;

typedef struct {
	char* at;
	int iline, icol;
} lx_symbol;

lx_symbol lx_current(lexing_ctx* ctx) {
	lx_symbol result = { .at = ctx->at, .iline = ctx->iline, .icol = ctx->icol};
	return result;
}

lx_symbol lx_next(lexing_ctx* ctx) {
	lx_symbol result = { .at = ctx->at + 1, .iline = ctx->iline, .icol = ctx->icol + 1 };
	if (*ctx->at == '\n') {
		++result.iline;
		result.icol = 0;
	}

	return result;
}

lx_symbol lx_forward(lexing_ctx* ctx) {
	char lx_current = *ctx->at;
	assert(lx_current);
	++ctx->at;
	++ctx->icol;
	if (lx_current == '\n') {
		++ctx->iline;
		ctx->icol = 0;
	}

	lx_symbol result = { .at = ctx->at, .iline = ctx->iline, .icol = ctx->icol };
	return result;
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

b32 lx_is_digit(char c) {
	return c >= '0' && c <= '9';
}

b32 is_whitespace(char c) {
	b32 result = c == ' ' || c == '\t' || c == '\r' || c == '\n';
	return result;
}

void lx_eat_whitespace(lexing_ctx* ctx) {
	lx_symbol sym = lx_current(ctx);
	while (is_whitespace(*sym.at)) { 
		sym = lx_forward(ctx); 
	}
}

token lx_next_token(lexing_ctx* ctx) {
	lx_eat_whitespace(ctx);

	lx_symbol sym = lx_current(ctx);
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
	} else if (*sym.at == '/' && *lx_next(ctx).at == '*') {
		while (!(*lx_current(ctx).at == '*' && *lx_next(ctx).at == '/')) {
			lx_forward(ctx);
		}
		lx_forward(ctx); lx_forward(ctx);
		forward_before_returning = false;
		tk = lx_next_token(ctx);
	} else if (*sym.at == '-' && *lx_next(ctx).at == '>') {
		tk.type = TK_ARROW;
		tk.str.len = 2;
		lx_forward(ctx);
	} else if (*sym.at == '+') {
		tk.type = TK_ADD;
	} else if (*sym.at == '-') {
		tk.type = TK_SUB;
	} else if (*sym.at == '*') {
		tk.type = TK_MUL;
	} else if (*sym.at == '/') {
		tk.type = TK_DIV;
	} else if (lx_is_digit(*sym.at)) {
		tk.type = TK_NUMBER;
		b32 decimal_point_present = false;
		lx_symbol s;
		while (true) {
			s = lx_forward(ctx);
			if (*s.at == '.') {
				assert(!decimal_point_present);
				decimal_point_present = true;
				s = lx_forward(ctx);
			}

			if (!lx_is_digit(*s.at)) {
				break;
			}
		}

		tk.str.len = s.at - sym.at;
		forward_before_returning = false;
	} else if (*sym.at == '"') {
		tk.type = TK_STRING;
		lx_symbol s;
		s = lx_forward(ctx);
		tk.str.at = s.at;
		while (*s.at != '"') { s = lx_forward(ctx); } 
		tk.str.len = s.at - sym.at - 1;
	} else if (is_identifier_first_sym(*sym.at)) {
		tk.type = TK_IDENTIFIER;
		lx_symbol s;
		do { s = lx_forward(ctx); } while (is_identifier_sym(*s.at));
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
		lx_forward(ctx);
	}

	return tk;
}

token* lx_parse_all_tokens(char* source_code) {
	lexing_ctx ctx = { .source = source_code, .at = source_code, .iline = 0, .icol = 0 };
	token* tokens = 0;
	token tk;
	do {
		tk = lx_next_token(&ctx);
		stb_arr_push(tokens, tk);
	} while (tk.type != TK_EOF);

	return tokens;
}

/*********************************PARSING*********************************/

typedef struct {
	VARIABLE_TYPE type;
	string custom_type_name;
} type_sig;

typedef struct {
	type_sig type;
	string name;
} variable_sig;

b32 is_primitive(VARIABLE_TYPE type) {
	return type != VT_STRUCT && type != VT_ENUM;
}

b32 is_token_primitive_type(TOKEN_TYPE type) {
	return type == TK_S32 || type == TK_U32;
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

typedef struct {
	string name;
	type_sig return_type;
	variable_sig* parameters;
	struct statement** statements;
} function_sig;

typedef struct {
	struct statement* fn_statement;
	struct statement** parameters;
} function_call;

typedef struct {
	struct statement* left_hand;
	struct statement* right_hand;
} assignment;

typedef struct {
	string name;
} identifier;

typedef struct {
	BINARY_OP_TYPE type;
	struct statement* left_hand;
	struct statement* right_hand;
} binary_op;

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
		binary_op value_binary_op;

		identifier value_identifier;
	};
} statement;

statement* malloc_statement(STATEMENT_TYPE type) {
	statement* result = malloc(sizeof(statement));
	*result = (statement){0};
	result->type = type;
	return result;
}

b32 is_token_binary_op(TOKEN_TYPE tt) {
	return tt == TK_ADD || tt == TK_SUB || tt == TK_MUL || tt == TK_DIV || tt == TK_EQUALS;
}

BINARY_OP_TYPE token_type_2_binary_op_type(TOKEN_TYPE tt) {
	switch (tt) {
		case TK_ADD:
			return BO_ADD;
		case TK_SUB:
			return BO_SUB;
		case TK_MUL:
			return BO_MUL;
		case TK_DIV:
			return BO_DIV;
		case TK_EQUALS:
			return BO_ASSIGNMENT;
	}

	assert(0 && "unexpected token type!");
	return ST_UNKNOWN;
}

int get_binary_op_priority(BINARY_OP_TYPE type) {
	switch (type) {
		case BO_ASSIGNMENT:
			return 0;
		case BO_ADD:
		case BO_SUB:
			return 1;
		case BO_MUL:
		case BO_DIV:
			return 2;
	}

	assert(0 && "unexpected binary operator type!");
	return -1;
}

typedef struct {
	token* tokens;
	token* at;
} parse_ctx;

token peek_token(parse_ctx* ctx, int offset) {
	token* at = ctx->at + offset;
	assert(at >= ctx->tokens);
	assert(at - ctx->tokens < stb_arr_len(ctx->tokens));
	return *at;
}

token next_token(parse_ctx* ctx) {
	token tk = *ctx->at;
	++ctx->at;
	assert(ctx->at >= ctx->tokens);
	assert(ctx->at - ctx->tokens <= stb_arr_len(ctx->tokens));
	return tk;
}

token require_token(parse_ctx* ctx, TOKEN_TYPE required_type) {
	token tk = next_token(ctx);
	assert(tk.type == required_type);
	if (tk.type != required_type) {
		exit(1);
	}
	return tk;
}

statement* parse_statement(parse_ctx* ctx);

statement* parse_number(parse_ctx* ctx) {
	// TODO: actual number parsing instead of this bullshit
	token tk = require_token(ctx, TK_NUMBER);
	statement* st = malloc_statement(ST_LITERAL_S32);
	st->value_s32 = atoi(tk.str.at);
	return st;
}

statement* parse_string(parse_ctx* ctx) {
	token tk = require_token(ctx, TK_STRING);
	statement* st = malloc_statement(ST_LITERAL_STR);
	st->value_str = tk.str;
	return st;
}

type_sig parse_type_sig(parse_ctx* ctx) {
	type_sig result = {0};
	token tk = next_token(ctx);
	if (is_token_primitive_type(tk.type)) {
		result.type = token_type_2_variable_type(tk.type);
	} else {
		assert(0 && "unexpected variable type!");
	}

	return result;
}

statement* parse_variable_sig(parse_ctx* ctx) {
	token tk = require_token(ctx, TK_IDENTIFIER);
	statement* st = malloc_statement(ST_VAR_DECL);
	variable_sig* decl = &st->value_var_decl;
	decl->name = tk.str;
	require_token(ctx, TK_COLON);
	decl->type = parse_type_sig(ctx);

	return st;
}

statement* parse_fn_call(parse_ctx* ctx, statement* fn_statement) {
	require_token(ctx, TK_LPAREN);
	statement* st = malloc_statement(ST_FN_CALL);
	function_call* fn_call = &st->value_fn_call;
	fn_call->fn_statement = fn_statement;

	token tk = peek_token(ctx, 0);
	while (tk.type != TK_RPAREN) {
		statement* fn_param = parse_statement(ctx);
		stb_arr_push(fn_call->parameters, fn_param);
		tk = peek_token(ctx, 0);
		if (tk.type != TK_RPAREN) {
			require_token(ctx, TK_COMMA);
			tk = peek_token(ctx, 0);
			assert(tk.type != TK_RPAREN);
		}
	}
    require_token(ctx, TK_RPAREN);

	return st;
}

statement* parse_factor(parse_ctx* ctx) {
	statement* st = 0;
	token tk = peek_token(ctx, 0);
	if (tk.type == TK_NUMBER) {
		st = parse_number(ctx);
	} else if (tk.type == TK_STRING) {
		st = parse_string(ctx);
	} else if (tk.type == TK_IDENTIFIER) {
		tk = peek_token(ctx, 1);
		if (tk.type == TK_COLON) {
			return parse_variable_sig(ctx);
		} else {
            tk = require_token(ctx, TK_IDENTIFIER);
            st = malloc_statement(ST_IDENTIFIER);
			st->value_identifier.name = tk.str;
		}
	} else if (tk.type == TK_LPAREN) {
		next_token(ctx);
		st = parse_statement(ctx);
        tk = require_token(ctx, TK_RPAREN);
	} else {
		assert(0 && "unexpected token!");
	}

	tk = peek_token(ctx, 0);
	if (tk.type == TK_LPAREN) {
		st = parse_fn_call(ctx, st);
	}

	return st;
}

statement* parse_binary_operator_recursive(parse_ctx* ctx, statement* left_hand, int min_precedence) {
	// precedence climbing algorithm: http://faculty.ycp.edu/~dhovemey/fall2012/cs340/lecture/lecture06.html
	while (1) {
		token tk = peek_token(ctx, 0);
		if (!is_token_binary_op(tk.type) || get_binary_op_priority(token_type_2_binary_op_type(tk.type)) < min_precedence) {
			break;
		}
		next_token(ctx);
		statement* operator = malloc_statement(ST_BINARY_OP);
		binary_op* bop = &operator->value_binary_op;
		bop->type = token_type_2_binary_op_type(tk.type);
		bop->left_hand = left_hand;

		statement* right_hand = parse_factor(ctx);

		while(1) {
			tk = peek_token(ctx, 0);
			if (!is_token_binary_op(tk.type) || get_binary_op_priority(token_type_2_binary_op_type(tk.type)) <= get_binary_op_priority(bop->type)) {
				break;
			}
			right_hand = parse_binary_operator_recursive(ctx, right_hand, get_binary_op_priority(token_type_2_binary_op_type(tk.type)));
		}

		bop->right_hand = right_hand;
		left_hand = operator;
	}

	return left_hand;
}

statement* parse_binary_operator(parse_ctx* ctx, statement* left_hand) {
	return parse_binary_operator_recursive(ctx, left_hand, -1);
}

statement* parse_statement(parse_ctx* ctx) {
	statement* st = parse_factor(ctx);

	token tk = peek_token(ctx, 0);
	if (is_token_binary_op(tk.type)) {
		st = parse_binary_operator(ctx, st);
	}

	return st;
}

statement* parse_fn_statement(parse_ctx* ctx) {
	statement* st = parse_statement(ctx);
	require_token(ctx, TK_SEMICOLON);
	
	return st;
}

statement* parse_fn_declaration(parse_ctx* ctx) {
	token id = require_token(ctx, TK_IDENTIFIER);
	statement* st = malloc_statement(ST_FN_DECL);
	function_sig* fn = &st->value_fn_decl;
	fn->name = id.str;

	require_token(ctx, TK_COLON);
	require_token(ctx, TK_FN);
	require_token(ctx, TK_LPAREN);

	// TODO: parse parameters

	require_token(ctx, TK_RPAREN);
	token tk = peek_token(ctx, 0);
	if (tk.type == TK_ARROW) {
		require_token(ctx, TK_ARROW);
		fn->return_type = parse_type_sig(ctx);
	}

	require_token(ctx, TK_LBRACE);
    tk = peek_token(ctx, 0);
    while (tk.type != TK_RBRACE) {
        statement* fn_st = parse_fn_statement(ctx);
        stb_arr_push(fn->statements, fn_st);
        tk = peek_token(ctx, 0);
    }
	require_token(ctx, TK_RBRACE);

	return st;
}

statement* parse(parse_ctx* ctx) {
	token tk = peek_token(ctx, 0);
	if (tk.type == TK_EOF) {
		return 0;
	}
	
	if (tk.type == TK_IDENTIFIER) {
		token follow_up = peek_token(ctx, 1);
		if (follow_up.type == TK_COLON) { // declaration of something
			follow_up = peek_token(ctx, 2);
			if (follow_up.type == TK_FN) { // function declaration
				return parse_fn_declaration(ctx);
			} else {
				assert(0 && "unexpected token type!");
			}
		}
	} else {
		assert(0 && "unexpected token type!");
	}

	return 0;
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
		switch (var->type.type) {
			case VT_S32: {
				printf("s32");
			} break;
			case VT_U32: {
				printf("u32");
			} break;
			case VT_STRUCT:
			case VT_ENUM: {
				printf("%.*s", var->type.custom_type_name.len, var->type.custom_type_name.at);
			} break;
			default: {
				printf("{unknown var type}");
			} break;
		}
	} else if (root->type == ST_FN_DECL) {
		function_sig* f = &root->value_fn_decl;
		// TODO: parameters printing
		printf("%.*s : fn() ", f->name.len, f->name.at);
		if (f->return_type.type != VT_VOID) {
			printf("-> %s ", VARIABLE_TYPE_STR(f->return_type.type));
			if (!is_primitive(f->return_type.type)) {
				printf("%.*s ", f->return_type.custom_type_name.len, f->return_type.custom_type_name.at);
			}
		}
		printf("{");
		for (u32 i = 0; i < stb_arr_len(f->statements); ++i) {
			printf("\n\t");
			print_ast(f->statements[i]);
		}
		printf("\n}\n");
	} else if (root->type == ST_FN_CALL) {
		function_call* f = &root->value_fn_call;
		print_ast(f->fn_statement);
		printf("(");
		for (u32 i = 0, n = stb_arr_len(f->parameters); i < n; ++i) {
			print_ast(f->parameters[i]);
			if (i != n - 1) {
				printf(", ");
			}
		}
		printf(")");
	} else if (root->type == ST_BINARY_OP) {
		binary_op* bop = &root->value_binary_op;
		if (bop->left_hand->type == ST_BINARY_OP) {
			printf("(");
		}
		print_ast(bop->left_hand);
		if (bop->left_hand->type == ST_BINARY_OP) {
			printf(")");
		}
		if (bop->type == BO_ADD) {
			printf(" + ");
		} else if (bop->type == BO_SUB) {
			printf(" - ");
		} else if (bop->type == BO_MUL) {
			printf(" * ");
		} else if (bop->type == BO_DIV) {
			printf(" / ");
		} else if (bop->type == BO_ASSIGNMENT) {
			printf(" = ");
		} else {
			printf(" {unknown binary op} ");
		}
		if (bop->right_hand->type == ST_BINARY_OP) {
			printf("(");
		}
		print_ast(bop->right_hand);
		if (bop->right_hand->type == ST_BINARY_OP) {
			printf(")");
		}
	} else {
		printf("{ %s }", STATEMENT_TYPE_STR(root->type));
	}
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

int main(int argc, char** argv) {
	char* source_path = "test.cn";
	if (argc > 1) {
		source_path = argv[1];
	}

	char* source = load_text_file(source_path);
	assert(source);
	token* tokens = lx_parse_all_tokens(source);

#if 0 // print all tokens
	printf("#if 0\n");
	for (int i = 0; i < stb_arr_len(tokens); ++i) {
		token tk = tokens[i];
		printf("%3d:%-3d %-15s %.*s\n", tk.iline + 1, tk.icol + 1, TOKEN_TYPE_STR(tk.type),
				tk.str.len, tk.str.at);
	}
	printf("#endif\n");
#endif

#if 1
	statement** statements = 0;
	parse_ctx pctx = { .tokens = tokens, .at = tokens };
	statement* st = parse(&pctx);
	while (st) {
		stb_arr_push(statements, st);
		st = parse(&pctx);
	}
#endif

#if 1
	for (u32 i = 0; i < stb_arr_len(statements); ++i) {
		print_ast(statements[i]);
	}
#endif

	return 0;
}
