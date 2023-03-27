#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

int64_t *mix(int64_t *coords, int length, int rounds);

#ifdef __cplusplus
}
#endif