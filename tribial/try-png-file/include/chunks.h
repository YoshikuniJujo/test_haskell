#include <stdint.h>

#pragma pack(1)

typedef struct chunk_ihdr {
	uint32_t	width;
	uint32_t	height;
	uint8_t		depth;
	uint8_t		color_type;
	uint8_t		compression;
	uint8_t		filter;
	uint8_t		interlace; } chunk_ihdr;

typedef enum ColorType {
	Pallet = 1,
	Color = 2,
	Alpha = 4 } ColorType;

typedef enum CompressionMethod {
	DeflateInflate32 = 0 } CompressionMethod;

typedef enum FilterMethod {
	FilterMethod0 = 0 } FilterMethod;

typedef enum InterlaceMethod {
	NoInterlace = 0,
	Adam7Interlace = 1 } InterlaceMethod;
