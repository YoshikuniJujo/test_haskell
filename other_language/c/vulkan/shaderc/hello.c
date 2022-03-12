#include <stdio.h>
#include <shaderc/shaderc.h>

int
main(int argc, char *argv[])
{
	printf("Hello, world!\n");

	shaderc_compiler_t compiler = shaderc_compiler_initialize();

	shaderc_compilation_result_t result = shaderc_compile_into_spv(
		compiler, "#version 450\nvoid main() {}", 27,
		shaderc_glsl_vertex_shader, "main.vert", "main", NULL);

	printf("length            : %ld\n", shaderc_result_get_length(result));
	printf("num warnings      : %ld\n", shaderc_result_get_num_warnings(result));
	printf("num errors        : %ld\n", shaderc_result_get_num_errors(result));
	printf("compilation status: %d\n",  shaderc_result_get_compilation_status(result));

	shaderc_result_release(result);
	shaderc_compiler_release(compiler);

	return 0;
}
