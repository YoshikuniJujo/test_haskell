#include "foo.h"

Foo sampleFoo = { 123, 456 };

Foo *sample_foo() { return &sampleFoo; }
