#include <stdio.h>
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

main() {
	s32 answer_to_everything = 0;
	answer_to_everything = 5;
	printf("Hello, world! Answer to everything is %d\n");
	printf("Sorry, I've double checked, the answer to everything is actually %d\n");
}

get_answer_to_everything() {
}

