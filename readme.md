# Bracketians!
it's a programming like lisp, that you can use it for documentation or a replacement for latex software.

it's completely modular and extensible.

file format: `.bm` Bracketian Markdown.

**CURRENT STATE**: development

feel free to tweak it for your own use case.

## Basics:

### data types
#### number (int, float)
```nim
1
1.24
.32
```

#### string
```nim
"hey, escape \" Kisho!"
```

#### booleans, nil
```nim
true
false
nil
```

#### list, table
these are more advanced data types, you'll know about them below. 

### calling a function
```nim
[call your-function (arg1 arg2 ...)]
```

### creating a list

**using a function call**:
```nim
[! 1 2 3 4 5]
```

**using special syntax**:
```nim
(1 2 3 4 5)
```

### creating a table
you call a special function `:`
```nim
[: name "ali" age 12]
```

### define a var
```nim
[def 
  var1 value1
  var2 value2 
  ... 
]
```

### define a lambda (closure function)
with `lambda` or `fn`.

```nim
[def sum [lambda (x y) [+ x y]]]
[def minus [fn (x y) [- x y]]]
```

### call a lambda
```nim
[call sum (1 2)]
```

### if statement
```nim
[if cond true false]
```

## more examples
see `examples/` and `tests/`

## repl
complie and run `tests/interactive_shell.nim`

## helpful links:
* [lispy](https://norvig.com/lispy.html)
* [inspiration](https://github.com/xigoi/xidoc)
