#include <setjmp.h>
#include <stddef.h>

const size_t gc_registers_size = sizeof(jmp_buf);

__attribute__((noinline)) // So that the frame address is consistent. Inlining would give the wrong sp.
void gc_capture_ctx(void *regs, void **sp) {
  *sp = __builtin_frame_address(0);
  setjmp(*(jmp_buf *) regs);
}
