#version 450

#include <foo>

layout(location = 0) out vec3 fragColor;

vec3 colors[3] = vec3[](
	vec3(1.0, 0.0, 0.0),
	vec3(0.0, 1.0, 0.0),
	vec3(0.0, 0.0, 1.0)
);

void main()
{
	fragColor = colors[gl_VertexIndex];
}
