#ifndef __MEM_H__
#define __MEM_H__

#include <stdlib.h>

struct S_tas;
typedef struct S_tas S_tas;
typedef S_tas *T_tas;

extern T_tas memCreeTas(void);
extern void *memAlloue(T_tas tas, size_t t);
extern void *memRealloue(void *p, size_t t);
extern void memLibere(void *p);
extern void memLibereTas(T_tas tas);
extern void memLibereTout(void);

#endif /* __MEM_H__ */
