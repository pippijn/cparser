typedef int foo __attribute__ ((__vector_size__ (16)));

int
moo (foo f)
{
  return f[0];
}
