#include <iostream>
#include <stdio.h>
#define STB_IMAGE_IMPLEMENTATION
#include <stb/stb_image.h>

int
main(int argc, char *argv[])
{
	int textWidth, textHeight, textChannels;

	stbi_uc* pixels = stbi_load(
		"textures/texture.jpg", &textWidth, &textHeight, &textChannels,
		STBI_rgb_alpha);

	std::cout << "Width   : " << textWidth << std::endl;
	std::cout << "Height  : " << textHeight << std::endl;
	std::cout << "Channels: " << textChannels << std::endl;

	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 32; j++) {
			printf("%02x ", pixels[i * 16 + j]);
		}
		std::cout << std::endl;
	}

	stbi_image_free(pixels);

	return 0;
}
