#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

CAMLprim value string_get(value str, value index)
{
  intnat idx = Long_val(index);
  // printf("%ld \n", idx);
  if (idx < 0 || idx >= caml_string_length(str)) caml_array_bound_error();
printf("actual returned value");
  return Val_int(Byte_u(str, idx));
}

CAMLprim value get_from_runtime(value str, value index)
{
  return string_get(str, index);
}

