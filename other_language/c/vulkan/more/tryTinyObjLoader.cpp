#include <iostream>

#define TINYOBJLOADER_IMPLEMENTATION
#include <tiny_obj_loader.h>

int
main(int argc, char *argv[])
{
	tinyobj::attrib_t attrib;
	std::vector<tinyobj::shape_t> shapes;
	std::vector<tinyobj::material_t> materials;
	std::string warn, err;

	const std::string MODEL_PATH = "viking_room/viking_room.obj";

	if (!tinyobj::LoadObj(
		&attrib, &shapes, &materials, &warn, &err, MODEL_PATH.c_str())) {
		throw std::runtime_error(warn + err);
	}

	int i = 0;
	for (const auto& shape : shapes) {
		for (const auto& index : shape.mesh.indices) {
			if (i < 50) {
				auto& vs = attrib.vertices;
				std::cout
					<< vs[3 * index.vertex_index + 0] << " "
					<< vs[3 * index.vertex_index + 1] << " "
					<< vs[3 * index.vertex_index + 2]
					<< std::endl;
			}
			i++;
		}
	}

	std::cout << i << std::endl;

	return 0;
}
