#include <stdio.h>
#include <GLFW/glfw3.h>

int
main()
{
	printf("Hello, GLFW!\n");

	GLFWwindow* window;

	if (!glfwInit()) {
		printf("glfwInit() return FALSE\n");
		return -1;
	}

	window = glfwCreateWindow(640, 480, "HELLO GLFW", NULL, NULL);

	if (!window) {
		printf("glfwCrateWindow fail\n");
		glfwTerminate();
		return -1;
	}

	int present = glfwJoystickPresent(GLFW_JOYSTICK_1);
	int present2 = glfwJoystickPresent(GLFW_JOYSTICK_2);

	printf ("JOYSTICK 1 PRESENT: %d\n", present);
	printf ("JOYSTICK 2 PRESENT: %d\n", present2);

	while (!glfwWindowShouldClose(window)) {
//		glClear(GL_COLOR_BUFFER_BIT);
		glfwSwapBuffers(window);
		glfwPollEvents();

		int a;
		a = (a + 1) % 50;

		int count;
		const float* axes = glfwGetJoystickAxes(GLFW_JOYSTICK_1, &count);
		if (a == 0) {
			printf("%d\n", count);
			for (int i = 0; i < count; i++) {
				printf("%f\n", axes[i]);
			}
		}
	}

	glfwTerminate();
	return 0;
}
