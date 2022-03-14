#include <stdio.h>
#include <stdlib.h>
#include <shaderc/shaderc.h>

int
main(int argc, char *argv[])
{
	printf("Hello, world!\n");

	shaderc_compiler_t compiler = shaderc_compiler_initialize();

	shaderc_compilation_result_t result = shaderc_compile_into_spv(
		compiler, "#version 450\nvoid main() {}", 27,
		shaderc_glsl_vertex_shader, "main.vert", "main", NULL);

	printf("length            : %ld\n",
		shaderc_result_get_length(result));
	printf("num warnings      : %ld\n",
		shaderc_result_get_num_warnings(result));
	printf("num errors        : %ld\n",
		shaderc_result_get_num_errors(result));
	printf("compilation status: %d\n",
		shaderc_result_get_compilation_status(result));
	printf("error message     : \"%s\"\n",
		shaderc_result_get_error_message(result));

	size_t ln = shaderc_result_get_length(result);
	const char *bt = shaderc_result_get_bytes(result);

	FILE *op = fopen("tmp.spv", "w");

	if (op == NULL) printf("file open error!!\n");

	fwrite(bt, 1, ln, op);

	fclose(op);

	shaderc_result_release(result);
	shaderc_compiler_release(compiler);

	return 0;
}
