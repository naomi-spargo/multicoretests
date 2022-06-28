#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

CAMLprim value string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
printf("hi");
  CAMLassert (Byte (s, temp - Byte (s, temp)) == 0);
  return Val_long(temp - Byte (s, temp));
}

CAMLprim value bytes_length(value s)
{
  return string_length(s);
}
