#ifndef _FOO_H
#define _FOO_H

typedef struct { int x; int y; } Foo;

typedef enum StructureType {
	STRUCTURE_TYPE_INT,
	STRUCTURE_TYPE_FLOAT,
	STRUCTURE_TYPE_DOUBLE } StructureType;

typedef struct {
	StructureType	sType;
	const void* 	pNext;
	int		intNum; } IntValue;

typedef struct {
	StructureType	sType;
	const void*	pNext;
	float		floatNum; } FloatValue;

typedef struct {
	StructureType	sType;
	const void*	pNext;
	double		doubleNum; } DoubleValue;		

#endif
