main : fn() -> VT_S32 {
	x : u32 = (-(5) + (3 * 2))
	printf("Hello, world! The answer to everything is %u\n", x)
	x = get_answer()
	printf("Sorry, my bad, it's actually %d", x)
	return(0)
}
get_answer : fn() -> VT_S32 {
	result : u32 = 0
	result = 42
	return(result)
}
do_nothing : fn() {
}
