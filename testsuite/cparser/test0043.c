int Foo (int f);

int Foo (int f)
{
  int m;
  return 0;
}

void
moo (int f)
{
  if (0)
    {
      typedef int Foo;
      Foo (f);
    }
  Foo (f);
}

int
foo (int f)
{
  Foo (f);
  return f;
}
