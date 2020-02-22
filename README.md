# Yakult(unfinished)

Toy language which is useless

+ dynamic typing
+ strong typing
+ lexical scoping
+ procedural language with class

## Manual

```
let foo = 1;
foo++;
```

```
if (true) 
    printf("Hello, world!");

if (true) {
    printf("Hello\n");
    printf("World\n");    
}


let n = 1;
switch (n) {
case 1:
    printf("1");
case 2:
    printf("2");    
}

cond {
case n == 1:
    printf("1");
case n == 2:
    printf("2");    
}


while (#t) 
    printf("Hello, world");

while (#t) {
    printf("Hello\n");
    printf("World\n");    
}
```


```
let i = 1;
let f = 1.0;
let b = true;
let c = 'c';
let s = "str";
let l = #[1, 2, 3];
let v = <<1, 2, 3>>;
let h = #{
    1 => 2,
    2 => 3,
}

fn bar(n) {
    return n+1; 
}

class Cls {
    attr {
        n;
    }

    method {
        fn get_n() {
            return self.n; 
        }

        fn set_n(n) {
            self.n = n;    
        }

        fn equal?(c) {
            return self.n == c.n; 
        }
    }
}
```

### Keyword

```
KEYWORD = ['let', 'if', 'while', 'break', 'continue', 'switch', 'case', 'cond', 'fn', 'return', 'class', 'method', 'attr', 'require', 'import']
```
