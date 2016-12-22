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

#define META_ENUM_MEMBER(member_name, readable_name) member_name,

#define META_EMIT_ENUM_SIGNATURE(enum_members, enum_name) \
	typedef enum enum_name { \
		enum_members \
	} enum_name;

#define META_TOKEN_TYPE_MEMBERS \
	META_ENUM_MEMBER(TK_EOF, 		"eof") \
	META_ENUM_MEMBER(TK_IDENTIFIER, "identifier") \
	META_ENUM_MEMBER(TK_FN, 		"fn") \
	META_ENUM_MEMBER(TK_RETURN, 	"return") \
	META_ENUM_MEMBER(TK_STRUCT, 	"struct") \
	META_ENUM_MEMBER(TK_ENUM, 		"enum") \
	META_ENUM_MEMBER(TK_U32, 		"u32") \
	META_ENUM_MEMBER(TK_S32, 		"s32") \
	META_ENUM_MEMBER(TK_STRING, 	"string") \
	META_ENUM_MEMBER(TK_NUMBER, 	"number") \
	META_ENUM_MEMBER(TK_LBRACE, 	"left brace") \
	META_ENUM_MEMBER(TK_RBRACE, 	"right brace") \
	META_ENUM_MEMBER(TK_LPAREN, 	"left parenthese") \
	META_ENUM_MEMBER(TK_RPAREN, 	"right parenthese") \
	META_ENUM_MEMBER(TK_LBRACKET, 	"left bracket") \
	META_ENUM_MEMBER(TK_RBRACKET,	"right bracket") \
	META_ENUM_MEMBER(TK_SEMICOLON,	"semicolon") \
	META_ENUM_MEMBER(TK_COLON,		"colon") \
	META_ENUM_MEMBER(TK_DOT,		"dot") \
	META_ENUM_MEMBER(TK_COMMA,		"comma") \
	META_ENUM_MEMBER(TK_HASH, 		"hash") \
	META_ENUM_MEMBER(TK_PLUS, 		"plus") \
	META_ENUM_MEMBER(TK_MINUS, 		"minus") \
	META_ENUM_MEMBER(TK_MUL, 		"multiply") \
	META_ENUM_MEMBER(TK_DIV,		"divide") \
	META_ENUM_MEMBER(TK_EQUALS, 	"equals") \
	META_ENUM_MEMBER(TK_ARROW, 		"arrow") \
	META_ENUM_MEMBER(TK_UNKNOWN,	"unknown")

#define META_STATEMENT_TYPE_MEMBERS \
	META_ENUM_MEMBER(ST_LITERAL,	 	"literal") \
	META_ENUM_MEMBER(ST_VAR_DEFINITION,	"var definition") \
	META_ENUM_MEMBER(ST_FN,				"fn") \
	META_ENUM_MEMBER(ST_FN_DEFINITION,	"fn definition") \
	META_ENUM_MEMBER(ST_FN_CALL,		"fn call") \
	META_ENUM_MEMBER(ST_IDENTIFIER,		"identifier") \
	META_ENUM_MEMBER(ST_ASSIGNMENT,		"assignment") \
	META_ENUM_MEMBER(ST_BINARY_OP,		"binary operator") \
	META_ENUM_MEMBER(ST_UNARY_OP,		"unary operator") \
	META_ENUM_MEMBER(ST_VOID,			"void") \
	META_ENUM_MEMBER(ST_UNKNOWN,		"unknown")

#define META_BINARY_OP_TYPE_MEMBERS \
	META_ENUM_MEMBER(BO_UNKNOWN,	"unknown") \
	META_ENUM_MEMBER(BO_ADD,		"add") \
	META_ENUM_MEMBER(BO_SUB,		"substract") \
	META_ENUM_MEMBER(BO_MUL,		"multiply") \
	META_ENUM_MEMBER(BO_DIV,		"divide") \
	META_ENUM_MEMBER(BO_ASSIGNMENT,	"assign")

#define META_UNARY_OP_TYPE_MEMBERS \
	META_ENUM_MEMBER(UO_UNKNOWN,	"unknown") \
	META_ENUM_MEMBER(UO_POSITIVE,	"positive") \
	META_ENUM_MEMBER(UO_NEGATIVE,	"negative") \
	META_ENUM_MEMBER(UO_RETURN,		"return")

#define META_SYMBOL_TYPE_MEMBERS \
	META_ENUM_MEMBER(SM_VOID,		"void") \
	META_ENUM_MEMBER(SM_S32,		"s32") \
	META_ENUM_MEMBER(SM_U32,		"u32") \
	META_ENUM_MEMBER(SM_STRING,		"string") \
	META_ENUM_MEMBER(SM_STRUCT,		"struct") \
	META_ENUM_MEMBER(SM_ENUM,		"enum") \
	META_ENUM_MEMBER(SM_FN,			"fn") \
	META_ENUM_MEMBER(SM_UNKNOWN,	"unknown")

META_EMIT_ENUM_SIGNATURE(META_TOKEN_TYPE_MEMBERS, TOKEN_TYPE);
META_EMIT_ENUM_SIGNATURE(META_STATEMENT_TYPE_MEMBERS, STATEMENT_TYPE);
META_EMIT_ENUM_SIGNATURE(META_BINARY_OP_TYPE_MEMBERS, BINARY_OP_TYPE);
META_EMIT_ENUM_SIGNATURE(META_UNARY_OP_TYPE_MEMBERS, UNARY_OP_TYPE);
META_EMIT_ENUM_SIGNATURE(META_SYMBOL_TYPE_MEMBERS, SYMBOL_TYPE);

/************************ENUM TO STRING FUNCTIONS************************/

#undef META_ENUM_MEMBER
#define META_ENUM_MEMBER(member_name, readable_name) readable_name,

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
META_EMIT_ENUM_TO_STR_FN(META_UNARY_OP_TYPE_MEMBERS, UNARY_OP_TYPE);
META_EMIT_ENUM_TO_STR_FN(META_SYMBOL_TYPE_MEMBERS, SYMBOL_TYPE);

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

string cstring_2_string(char* cstring) {
	string result = { .at = cstring, .len = strlen(cstring) };
	return result;
}

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
		tk.type = TK_PLUS;
	} else if (*sym.at == '-') {
		tk.type = TK_MINUS;
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
		} else if (!strncmp(tk.str.at, "return", tk.str.len)) {
			tk.type = TK_RETURN;
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
	SYMBOL_TYPE type;
	union {
		string custom_type_name;
		struct fn_signature* fn_signature;
	};
} type_signature;

typedef struct {
	string name;
	type_signature type;
} var_definition;

b32 is_primitive(SYMBOL_TYPE type) {
	return type != SM_STRUCT && type != SM_ENUM;
}

b32 is_token_primitive_type(TOKEN_TYPE type) {
	return type == TK_S32 || type == TK_U32;
}

b32 can_token_be_unary_op(TOKEN_TYPE type) {
	return type == TK_PLUS || type == TK_MINUS || type == TK_RETURN;
}

SYMBOL_TYPE token_type_2_symbol_type(TOKEN_TYPE type) {
	switch (type) {
		case TK_S32:
			return SM_S32;
		case TK_U32:
			return SM_U32;
	}

	assert(0 && "unexpected token type!");
	return SM_UNKNOWN;
}

typedef struct fn_signature {
	type_signature return_type;
	var_definition** parameters;
} fn_signature;

typedef struct {
	struct statement** statements;
	struct sym_table* sym_table;
} scope;

typedef struct {
	string name;
	fn_signature signature;
	scope* scope;
	struct symbol* symbol;
} fn_definition;

typedef struct {
	struct statement* fn_statement;
	struct statement** parameters;
} fn_call;

typedef struct {
	string name;
	struct symbol* symbol;
} identifier;

typedef struct {
	BINARY_OP_TYPE type;
	struct statement* left_hand;
	struct statement* right_hand;
} binary_op;

typedef struct {
	UNARY_OP_TYPE type;
	struct statement* operand;
} unary_op;

typedef struct {
	SYMBOL_TYPE type;
	struct symbol* symbol;
	union {
		s32 s32;
		u32 u32;
		string string;
	};
} literal;

typedef struct statement {
	STATEMENT_TYPE type;
	type_signature resulting_type;

	union {
		literal value_literal;

		var_definition value_var_decl;

		fn_definition value_fn_decl;
		fn_call value_fn_call;

		binary_op value_binary_op;
		unary_op value_unary_op;

		identifier value_identifier;
	};
} statement;

scope* malloc_scope() {
	scope* result = malloc(sizeof(scope));
	*result = (scope){0};
	return result;
}

statement* malloc_statement(STATEMENT_TYPE type) {
	statement* result = malloc(sizeof(statement));
	*result = (statement){0};
	result->type = type;
	result->resulting_type.type = SM_VOID;
	return result;
}

b32 is_token_binary_op(TOKEN_TYPE tt) {
	return tt == TK_PLUS || tt == TK_MINUS || tt == TK_MUL || tt == TK_DIV || tt == TK_EQUALS;
}

BINARY_OP_TYPE token_type_2_binary_op_type(TOKEN_TYPE tt) {
	switch (tt) {
		case TK_PLUS:
			return BO_ADD;
		case TK_MINUS:
			return BO_SUB;
		case TK_MUL:
			return BO_MUL;
		case TK_DIV:
			return BO_DIV;
		case TK_EQUALS:
			return BO_ASSIGNMENT;
	}

	assert(0 && "unexpected token type!");
	return BO_UNKNOWN;
}

UNARY_OP_TYPE token_type_2_unary_op_type(TOKEN_TYPE tt) {
	switch (tt) {
		case TK_PLUS:
			return UO_POSITIVE;
		case TK_MINUS:
			return UO_NEGATIVE;
		case TK_RETURN:
			return UO_RETURN;
	}

	assert(0 && "unexpected token type!");
	return UO_UNKNOWN;
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
	statement* st = malloc_statement(ST_LITERAL);
	literal* l = &st->value_literal;
	l->type = SM_S32;
	l->s32 = atoi(tk.str.at);
	return st;
}

statement* parse_string(parse_ctx* ctx) {
	token tk = require_token(ctx, TK_STRING);
	statement* st = malloc_statement(ST_LITERAL);
	literal* l = &st->value_literal;
	l->type = SM_STRING;
	l->string = tk.str;
	return st;
}

type_signature parse_type_sig(parse_ctx* ctx) {
	type_signature result = {0};
	token tk = next_token(ctx);
	if (is_token_primitive_type(tk.type)) {
		result.type = token_type_2_symbol_type(tk.type);
	} else {
		assert(0 && "unexpected variable type!");
	}

	return result;
}

statement* parse_variable_sig(parse_ctx* ctx) {
	token tk = require_token(ctx, TK_IDENTIFIER);
	statement* st = malloc_statement(ST_VAR_DEFINITION);
	var_definition* decl = &st->value_var_decl;
	decl->name = tk.str;
	require_token(ctx, TK_COLON);
	decl->type = parse_type_sig(ctx);

	return st;
}

statement* parse_fn_call(parse_ctx* ctx, statement* fn_statement) {
	require_token(ctx, TK_LPAREN);
	statement* st = malloc_statement(ST_FN_CALL);
	fn_call* fn_call = &st->value_fn_call;
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

statement* parse_factor(parse_ctx* ctx);

statement* parse_unary_op(parse_ctx* ctx) {
	token tk = next_token(ctx);
	assert(can_token_be_unary_op(tk.type));
	statement* st = malloc_statement(ST_UNARY_OP);
	unary_op* uop = &st->value_unary_op;
	uop->type = token_type_2_unary_op_type(tk.type);
	uop->operand = parse_factor(ctx);
	return st;
}

statement* parse_factor(parse_ctx* ctx) {
	statement* st = 0;
	token tk = peek_token(ctx, 0);
	if (tk.type == TK_NUMBER) {
		st = parse_number(ctx);
	} else if (tk.type == TK_STRING) {
		st = parse_string(ctx);
	} else if (can_token_be_unary_op(tk.type)) {
		st = parse_unary_op(ctx);
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
	statement* st = malloc_statement(ST_FN_DEFINITION);
	fn_definition* fn = &st->value_fn_decl;
	fn->name = id.str;

	require_token(ctx, TK_COLON);
	require_token(ctx, TK_FN);
	require_token(ctx, TK_LPAREN);

	// TODO: parse parameters

	require_token(ctx, TK_RPAREN);
	token tk = peek_token(ctx, 0);
	if (tk.type == TK_ARROW) {
		require_token(ctx, TK_ARROW);
		fn->signature.return_type = parse_type_sig(ctx);
	}

	// TODO: parse declarations of foreign functions
	require_token(ctx, TK_LBRACE);
	fn->scope = malloc_scope();
    tk = peek_token(ctx, 0);
    while (tk.type != TK_RBRACE) {
        statement* fn_st = parse_fn_statement(ctx);
        stb_arr_push(fn->scope->statements, fn_st);
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

/*********************************AST PRINTING*********************************/

void print_ast(statement* root, b32 print_statement_resulting_types) {
	if (root->type == ST_LITERAL) {
		literal* l = &root->value_literal;
		if (l->type == SM_S32) {
			printf("%d", l->s32);
		} else if (l->type == SM_U32) {
			printf("%u", l->u32);
		} else if (l->type == SM_STRING) {
			printf("\"%.*s\"", l->string.len, l->string.at);
		} else {
			printf("{%s}", SYMBOL_TYPE_STR(l->type));
		}
	} else if (root->type == ST_IDENTIFIER) {
		printf("%.*s", root->value_identifier.name.len, root->value_identifier.name.at);
		if (!root->value_identifier.symbol) {
			printf("{no symbol ptr!}");
		}
	} else if (root->type == ST_VAR_DEFINITION) {
		var_definition* var = &root->value_var_decl;
		printf("%.*s : ", var->name.len, var->name.at);
		switch (var->type.type) {
			case SM_S32: {
				printf("s32");
			} break;
			case SM_U32: {
				printf("u32");
			} break;
			case SM_STRUCT:
			case SM_ENUM: {
				printf("%.*s", var->type.custom_type_name.len, var->type.custom_type_name.at);
			} break;
			default: {
				printf("{%s}", SYMBOL_TYPE_STR(var->type.type));
			} break;
		}
	} else if (root->type == ST_FN_DEFINITION) {
		fn_definition* f = &root->value_fn_decl;
		// TODO: parameters printing
		printf("%.*s : fn() ", f->name.len, f->name.at);
		if (f->signature.return_type.type != SM_VOID) {
			printf("-> %s ", SYMBOL_TYPE_STR(f->signature.return_type.type));
			if (!is_primitive(f->signature.return_type.type)) {
				printf("%.*s ", f->signature.return_type.custom_type_name.len, f->signature.return_type.custom_type_name.at);
			}
		}
		if (f->scope) {
			printf("{");
			for (u32 i = 0; i < stb_arr_len(f->scope->statements); ++i) {
				printf("\n\t");
				print_ast(f->scope->statements[i], print_statement_resulting_types);
			}
			printf("\n}\n");
		} else {
			printf(";\n");
		}
	} else if (root->type == ST_FN_CALL) {
		fn_call* f = &root->value_fn_call;
		print_ast(f->fn_statement, print_statement_resulting_types);
		printf("(");
		for (u32 i = 0, n = stb_arr_len(f->parameters); i < n; ++i) {
			print_ast(f->parameters[i], print_statement_resulting_types);
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
		print_ast(bop->left_hand, print_statement_resulting_types);
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
			printf(" {%s} ", BINARY_OP_TYPE_STR(bop->type));
		}
		if (bop->right_hand->type == ST_BINARY_OP) {
			printf("(");
		}
		print_ast(bop->right_hand, print_statement_resulting_types);
		if (bop->right_hand->type == ST_BINARY_OP) {
			printf(")");
		}
	} else if (root->type == ST_UNARY_OP) {
		unary_op* uop = &root->value_unary_op;
		if (uop->type == UO_POSITIVE) {
			printf("+");
		} else if (uop->type == UO_NEGATIVE) {
			printf("-");
		} else if (uop->type == UO_RETURN) {
			printf("return");
		}
		printf("(");
		print_ast(uop->operand, print_statement_resulting_types);
		printf(")");
	} else {
		printf("{%s}", STATEMENT_TYPE_STR(root->type));
	}

	if (print_statement_resulting_types) {
		printf("[%s]", SYMBOL_TYPE_STR(root->resulting_type.type));
	}
}

/*********************************SYMBOL TABLE*********************************/

typedef struct symbol {
	string name;
	type_signature type;
	u32 iline, icol;
	b32 is_literal;
	// this is used identify usages of symbols that are in the current scope but was not yet defined
	b32 is_defined;
} symbol;

typedef struct sym_table {
	string name;
	symbol** symbols;
	struct sym_table** children;
} sym_table;

symbol* malloc_symbol(SYMBOL_TYPE type) {
	symbol* result = malloc(sizeof(symbol));
	*result = (symbol){0};
	result->type.type = type;
	if (type == SM_FN) {
		result->type.fn_signature = malloc(sizeof(fn_signature));
		*result->type.fn_signature = (fn_signature){0};
	}
	return result;
}

sym_table* malloc_sym_table() {
	sym_table* result = malloc(sizeof(sym_table));
	*result = (sym_table){0};
	return result;
}

void build_symbol_table_recursive(statement* node, sym_table* current_table, sym_table* global_table, b32 is_current_table_global) {
	if (node->type == ST_FN_DEFINITION) {
		symbol* sym = malloc_symbol(SM_FN);
		// functions can always be used before they are defined
		sym->is_defined = true;
		fn_definition* fn = &node->value_fn_decl;
		fn->symbol = sym;
		sym->name = fn->name;
		*sym->type.fn_signature = fn->signature;
		stb_arr_push(current_table->symbols, sym);

		if (fn->scope) {
			sym_table* child_table = malloc_sym_table();
			fn->scope->sym_table = child_table;
			child_table->name = sym->name;
			for (u32 i = 0; i < stb_arr_len(fn->scope->statements); ++i) {
				build_symbol_table_recursive(fn->scope->statements[i], child_table, global_table, false);
			}
			stb_arr_push(current_table->children, child_table);
		}
	} else if (node->type == ST_VAR_DEFINITION) {
		var_definition* var = &node->value_var_decl;
		symbol* sym = malloc_symbol(var->type.type);
		// only global variables can be used before they are defined
		sym->is_defined = is_current_table_global;
		sym->name = var->name;
		stb_arr_push(current_table->symbols, sym);
	} else if (node->type == ST_FN_CALL) {
		fn_call* fc = &node->value_fn_call;
		for (u32 i = 0; i < stb_arr_len(fc->parameters); ++i) {
			build_symbol_table_recursive(fc->parameters[i], current_table, global_table, is_current_table_global);
		}
	} else if (node->type == ST_LITERAL) {
		literal* l = &node->value_literal;
		if (l->type == SM_STRING) {
			symbol* sym = malloc_symbol(SM_STRING);
			sym->is_literal = true;
			sym->is_defined = true;
			sym->name = l->string;
			l->symbol = sym;
			stb_arr_push(global_table->symbols, sym);
		}
	} else if (node->type == ST_BINARY_OP) {
		build_symbol_table_recursive(node->value_binary_op.left_hand, current_table, global_table, is_current_table_global);
		build_symbol_table_recursive(node->value_binary_op.right_hand, current_table, global_table, is_current_table_global);
	} else if (node->type == ST_UNARY_OP) {
		build_symbol_table_recursive(node->value_unary_op.operand, current_table, global_table, is_current_table_global);
	}
}

sym_table* build_symbol_table(statement** ast_root_nodes) {
	sym_table* result = malloc_sym_table();
	result->name = cstring_2_string("global");
	for (u32 i = 0; i < stb_arr_len(ast_root_nodes); ++i) {
		build_symbol_table_recursive(ast_root_nodes[i], result, result, true);
	}
	return result;
}

void print_indent(s32 indent, b32 insert_new_line) {
	if (insert_new_line) {
		printf("\n");
	}
	for (u32 i = 0; i < indent; ++i) {
		printf("\t");
	}
}

void print_type(type_signature* type) {
	printf("%s", SYMBOL_TYPE_STR(type->type));
	if (type->type == SM_FN) {
		// TODO: arguments printing
		printf("()");
		if (type->fn_signature->return_type.type != SM_VOID) {
			printf(" -> ");
			print_type(&type->fn_signature->return_type);
		}
	}
}

print_symbol_table(sym_table* s, u32 indent) {
	print_indent(indent, false);
	printf("%.*s {", s->name.len, s->name.at);
	for (u32 i = 0; i < stb_arr_len(s->symbols); ++i) {
		print_indent(indent + 1, true);
		symbol* sym = s->symbols[i];
		if (sym->is_literal) {
			printf("#literal : ");
		} else {
			printf("%.*s : ", sym->name.len, sym->name.at);
		}
		print_type(&sym->type);

		if (sym->is_literal && sym->type.type == SM_STRING) {
			printf(" = \"%.*s : \"", sym->name.len, sym->name.at);
		}
	}

	for (u32 i = 0; i < stb_arr_len(s->children); ++i) {
		print_indent(indent, true);
		sym_table* child_table = s->children[i];
		print_symbol_table(child_table, indent + 1);
	}

	print_indent(indent, true);
	printf("}");
}

b32 are_strings_equal(string a, string b) {
	u32 min_len = a.len < b.len ? a.len : b.len;
	return !strncmp(a.at, b.at, min_len);
}

// TODO: rewrite this bit after making scopes into just a type of statement
void patch_symbol_pointers_recursive(statement* st, sym_table** sym_table_stack) {
	if (st->type == ST_BINARY_OP) {
		patch_symbol_pointers_recursive(st->value_binary_op.left_hand, sym_table_stack);
		patch_symbol_pointers_recursive(st->value_binary_op.right_hand, sym_table_stack);
	} else if (st->type == ST_UNARY_OP) {
		patch_symbol_pointers_recursive(st->value_unary_op.operand, sym_table_stack);
	} else if (st->type == ST_FN_CALL) {
		fn_call* fc = &st->value_fn_call;
		patch_symbol_pointers_recursive(fc->fn_statement, sym_table_stack);
		for (u32 i = 0; i < stb_arr_len(fc->parameters); ++i) {
			patch_symbol_pointers_recursive(fc->parameters[i], sym_table_stack);
		}
	} else if (st->type == ST_IDENTIFIER) {
		identifier* id = &st->value_identifier;
		for (s32 i = stb_arr_len(sym_table_stack) - 1; i >= 0 && !id->symbol; --i) {
			sym_table* table = sym_table_stack[i];
			for (u32 j = 0; j < stb_arr_len(table->symbols); ++j) {
				symbol* s = table->symbols[j];
				if (s->is_literal) {
					continue;
				}

				if (are_strings_equal(s->name, id->name)) {
					id->symbol = s;
					break;
				}
			}
		}
	}
}

void patch_symbol_pointers(scope* global_scope) {
	sym_table** sym_table_stack = 0;
	stb_arr_push(sym_table_stack, global_scope->sym_table);
	for (u32 i = 0; i < stb_arr_len(global_scope->statements); ++i) {
		statement* s = global_scope->statements[i];
		if (s->type == ST_FN_DEFINITION) {
			fn_definition* fn = &s->value_fn_decl;
			if (!fn->scope) {
				continue;
			}

			stb_arr_push(sym_table_stack, fn->scope->sym_table);
			for (u32 j = 0; j < stb_arr_len(fn->scope->statements); ++j) {
				patch_symbol_pointers_recursive(fn->scope->statements[j], sym_table_stack);
			}
			stb_arr_pop(sym_table_stack);
		}
	}
}

/************************RESOLVE STATEMENT TYPES************************/

SYMBOL_TYPE resolve_statement_types_recursive(statement* st) {
	if (st->type == ST_LITERAL) {
		literal* l = &st->value_literal;
		st->resulting_type.type = l->type;
		return l->type;
	} else if (st->type == ST_IDENTIFIER && st->value_identifier.symbol) {
		symbol* s = st->value_identifier.symbol;
		if (s->type.type == SM_FN) {
			st->resulting_type = s->type.fn_signature->return_type;
		} else {
			st->resulting_type = s->type;
		}
		return st->resulting_type.type;
	} else if (st->type == ST_FN_DEFINITION && st->value_fn_decl.symbol) { // NOTE: this does not work cause of the shitty way I handle scopes now
		st->resulting_type = st->value_fn_decl.symbol->type;
		return st->resulting_type.type;
	} else if (st->type == ST_FN_CALL) {
		fn_call* fc = &st->value_fn_call;
		for (u32 i = 0; i < stb_arr_len(fc->parameters); ++i) {
			resolve_statement_types_recursive(fc->parameters[i]);
		}
		SYMBOL_TYPE type = resolve_statement_types_recursive(fc->fn_statement);
		st->resulting_type.type = type;
		return type;
	} else if (st->type == ST_VAR_DEFINITION) {
		return st->value_var_decl.type.type;
	} else if (st->type == ST_BINARY_OP) {
		// TODO: actual logic, not this dumb shit
		SYMBOL_TYPE left_type = resolve_statement_types_recursive(st->value_binary_op.left_hand);
		SYMBOL_TYPE right_type = resolve_statement_types_recursive(st->value_binary_op.right_hand);
		assert(left_type == right_type);
		st->resulting_type.type = left_type;
		return left_type;
	} else if (st->type == ST_UNARY_OP) {
		// TODO: same as binary op
		SYMBOL_TYPE type = resolve_statement_types_recursive(st->value_unary_op.operand);
		st->resulting_type.type = type;
		return type;
	}

	return SM_VOID;
}

void resolve_statement_types(scope* global_scope) {
	for (u32 i = 0; i < stb_arr_len(global_scope->statements); ++i) {
		if (global_scope->statements[i]->type == ST_FN_DEFINITION) {
			scope* fn_scope = global_scope->statements[i]->value_fn_decl.scope;
			if (!fn_scope) {
				continue;
			}

			for (u32 j = 0; j < stb_arr_len(fn_scope->statements); ++j) {
				resolve_statement_types_recursive(fn_scope->statements[j]);
			}
		}
	}
}

/******************************LLVM IR GEN*****************************/

char* llvm_ir_type(type_signature* t) {
	if (t->type == SM_S32 || t->type == SM_U32) {
		return"i32";
	} else if (t->type == SM_VOID) {
		return"void";
	}

	assert(0 && "unexpected type!");
	return "fuck";
}

char* llvm_ir_binary_op(BINARY_OP_TYPE type) {
	switch (type) {
		case BO_ADD:
			return "add";
		case BO_SUB:
			return "sub";
	}

	assert(0 && "unexpected operator type!");
	return "fuck";
}

char* llvm_ir_unary_op(UNARY_OP_TYPE type) {
	switch(type) {
		case UO_NEGATIVE:
			return "-";
	}

	assert(0 && "unexpected operator type!");
	return "fuck";
}

llvm_ir_symbol_decl(symbol* s) {
	if (s->type.type == SM_FN) {
		printf("declare %s", llvm_ir_type(&s->type.fn_signature->return_type));
		printf(" @%.*s(", s->name.len, s->name.at);
		// TODO: generate ir for arguments
		printf(")\n");
	}
}

typedef struct {
	u32 registers_used;
} ssa_gen_ctx;

u32 llvm_ir_statement(statement* st, ssa_gen_ctx* ctx, u32 indent) {
	if (st->type == ST_LITERAL) {
		literal* l = &st->value_literal;
		if (l->type == SM_S32 || l->type == SM_U32) {
			u32 reg = ctx->registers_used++;
			print_indent(indent, true);
			printf("%%%u = i32 %u", reg, l->s32);
			return reg;
		}
	} else if (st->type == ST_BINARY_OP) {
		binary_op* bop = &st->value_binary_op;
		if (bop->type == BO_ASSIGNMENT) {
			/* statement* left_hand = bop->left_hand; */
			/* if (left_hand->type == ST_VAR_DEFINITION) { */
			/* } */
		} else {
			u32 left_reg = llvm_ir_statement(bop->left_hand, ctx, indent);
			u32 right_reg = llvm_ir_statement(bop->right_hand, ctx, indent);
			u32 reg = ctx->registers_used++;
			print_indent(indent, true);
			printf("%%%u = %s %s %%%u, %s %%%u", reg, llvm_ir_binary_op(bop->type),
					llvm_ir_type(&bop->left_hand->resulting_type), left_reg,
					llvm_ir_type(&bop->right_hand->resulting_type), right_reg);
			return reg;
		}
	} else if (st->type == ST_UNARY_OP) {
		unary_op* uop = &st->value_unary_op;
		if (uop->type == UO_RETURN) {
			u32 reg = llvm_ir_statement(uop->operand, ctx, indent);
			print_indent(indent, true);
			printf("ret %s %%%u", llvm_ir_type(&uop->operand->resulting_type), reg);
		} else {
		}
	}

	return 0xFFFFFFFF;
}

void generate_llvm_ir(scope* global_scope_ast)
{
	printf(
			"; ModuleID = 'test.cn'\n"
			"source_filename = \"test.cn\"\n"
			"declare i32 @printf(i8*, ...)\n\n");

	for (u32 i = 0; i < stb_arr_len(global_scope_ast->statements); ++i) {
		statement* s = global_scope_ast->statements[i];
		if (s->type == ST_FN_DEFINITION) {
			fn_definition* fn = &s->value_fn_decl;
			printf("define %s", llvm_ir_type(&fn->signature.return_type));
			printf(" @%.*s(", fn->name.len, fn->name.at);
			// TODO: generate ir for arguments
			printf(")");
			printf(" {");
			ssa_gen_ctx ssa_ctx = {0};
			for (u32 j = 0; j < stb_arr_len(fn->scope->statements); ++j) {
				statement* st = fn->scope->statements[j];
				llvm_ir_statement(st, &ssa_ctx, 1);
			}
			if (fn->signature.return_type.type == SM_VOID) {
				printf("\n\tret void");
			}
			printf("\n}\n");
		}
	}
}

/*********************************MAIN*********************************/

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
	scope* global_scope = malloc_scope();
	parse_ctx pctx = { .tokens = tokens, .at = tokens };
	statement* st = parse(&pctx);
	while (st) {
		stb_arr_push(global_scope->statements, st);
		st = parse(&pctx);
	}
#endif

#if 1
	sym_table* global_sym_table = build_symbol_table(global_scope->statements);
	global_scope->sym_table = global_sym_table;
	patch_symbol_pointers(global_scope);
	resolve_statement_types(global_scope);
#endif

#if 0
	print_symbol_table(global_sym_table, 0);
#endif

#if 0
	for (u32 i = 0; i < stb_arr_len(global_scope->statements); ++i) {
		print_ast(global_scope->statements[i], false);
	}
#endif

#if 1
	generate_llvm_ir(global_scope);
#endif

	return 0;
}
