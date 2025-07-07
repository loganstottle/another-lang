making this to learn rust
please ignore the bad code, i do not know what i'm doing

# REPL EXAMPLE
```
> fn add(a, b) a + b // identical
return: function
> add = fn(a, b) a + b // identical
return: function
> x = 1
return: 1
> y = x / 4 + 10
return: 10.25
> log(x, y, add, add(x, y))
1, 10.25, fn add(a, b) { ... }, 11.25
return: NULL
> (fn(x, y) x + y)(1, 2)
return: 3
```