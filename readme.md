# Bracketians!
document stuff with it

file format: `.bm` Bracketian Markdown

## Basics:
```nim
call []
list [!] => [! 1 2 3 4 5]
table [:] => [: name "ali" age 12]
symbol fa
text "dasd"
number 3213 12.221
```

## usage

```nim
[info 
  author "@hamidb80"
  email "hr.bolouri@gmail.com"
  copyright "MIT"
]

[def
  age 12
  name "hamid"
]

[ch 1 
  [multi-lang [:

    EN [&
      "dad"
    ]

    FA [&
      "متن 1"
    ]
  ]]

  [ref myFormula [LatEx "2^2 + 2*4 - 1"]]
]

[ch 2
  [addr myFormula]
  [pic "./me.png"]
  [include "./another-page.bm"]
]
```


## helpful links:
* [lispy](https://norvig.com/lispy.html)