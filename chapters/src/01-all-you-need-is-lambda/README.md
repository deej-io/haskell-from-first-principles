# Chapter 1: All You Need Is Lambda

## Notes and Definitions

### Functions
- **Function**: A mapping from a domain to a codomain
- **Domain**: The set of input values (argument type)
- **Codomain**: The set of possible output values (return type)
- **Image**: The subset of the output values that correspond to a particular
  input value
- **Referential Transparency**: An input value cannot map to more than one output
  value

### Lambda Calculus
- **Variable**: A name for potential input to a function
- **Abstraction**: A function with a head (with parameters) and body e.g.
```
   \ x . x
   ^-----^
      `- abstraction (anonymous function).

   \ x . x
   ^---^
     `- head

   \ x . x
     ^- parameter

   \ x . x
         ^- body
```

- **Alpha Equivalence**: Functions with different parameter names but have the
  same _shape_ are equivalent. When applied to a value the produce the same
  value e.g.
```
\x.x = \d.d = \z.z
```

- **Beta Reduction**: The process of applying the function to an argument,
  substituting the input expression for all instances of bound variables and
  removing the head .e.g
```
(\x.x)2 -> [x := 2]
2

(\x.x + 1)2 -> [x := 2]
2

(\x.x)(\y.y) -> [x := (\y.y)]
\y.y

(\x.x)(\y.y)z -> [x := (\y.y)]
(\y.y)z       -> [y := z]
z
```

- **Free Variable**: A variable that exists in the body but is not a parameter to
  the function e.g.
```
\x.xy -- y is a free variable
```

- **Currying**: Lambda expressions can only have one parameter so functions that
  have more than one parameter is actually a multiple nested lambda
  expressions e.g.
```
\xy.xy
```
  is shorthand for
```
\x.(\y.xy)
```

Beta reduction of curried lambdas:

```
(\xy.xy)(\z.a)1     -> curry the function
(\x.(\y.xy))(\z.a)1 -> [x := (\z.a)]
(\y.(z.a)y)1        -> [y := 1]
(\z.a)1             -> [z := 1]
a
```

### Equivalence Exercises

1. `\xy.xz`
   * [ ] `\xz.xz`
   * [x] `\mn.mz`
   * [ ] `\z(\x.xz)`

2. `\xy.xxy`
   * [ ] `\mn.mnp`
   * [ ] `\x.(\y.xy)`
   * [x] `\a.(\b.aab)`

3. `\xyz.zx`
   * [ ] `\x.(\y.(\z.z))`
   * [x] `\tos.st`
   * [ ] `\mnp.mn`

### Notes and definitions (cont.)

- **Beta normal form**: A lambda expression that is fully evaluated and cannot
  be reduced further
- **Combinator**: A function with no free variables i.e. it only _combines_
  its arguments
- **Divergence**: The reduction process never terminates, e.g. the following
  example loops indefinitely
```
(\x.xx)(\x.xx) -> [x := (x.xx)]
(\x.xx)(\x.xx) -> [x := (x.xx)]
(\x.xx)(\x.xx) -> [x := (x.xx)]
...
```

## Chapter Exercises

1. Which of the following are Combinators?
   * [x] `\x.xxx`
   * [ ] `\xy.zx`
   * [x] `\xyz.xy(zx)`
   * [x] `\xyz.xy(zxy)`
   * [ ] `\xy.xy(zxy)`

2. Which of these diverge?
   * [ ] `x.xxx` - already in normal form
   * [x] `(\z.zz)(\y.yy)` - will diverge due to alpha-equivalence
   * [ ] `(\x.xxx)z` - reduces to `zzz` which is in normal form

3. Beta-reduce the following:
   * `(\abc.cba)zz(\wv.w)`
     ```
     (\abc.cba)zz(\wv.w) -> [a := z]
     (\bc.cbz)z(\wv.w)   -> [b := z]
     (\c.czz)(\wv.w)     -> [c := (\wv.w)]
     (\wv.w)zz           -> [w := z]
     (\v.z)z             -> [v := z]
     z
     ```
   * `(\x.\y.xyy)(\a.a)b`
     ```
     (\x.\y.xyy)(\a.a)b -> [x := (\a.a)]
     (\y.(\a.a)yy)b     -> [a := y]
     (\y.yy)b           -> [y := b]
     bb
     ```
   * `(\y.y)(\x.xx)(\z.zq)`
     ```
     (\y.y)(\x.xx)(\z.zq) -> [y := (\x.xx)]
     (\x.xx)(\z.zq)       -> [x := (\z.zq)]
     (\z.zq)(\z.zq)       -> [z := (\z.zq)]
     (\z.zq)q             -> [z := q]
     qq
     ```
   * `(\z.z)(\z.zz)(\z.zy)`
     ```
     (\z.z)(\z.zz)(\z.zy) -> rename variables
     (\a.a)(\b.bb)(\c.cy) -> [a := (\b.bb)]
     (\b.bb)(\c.cy)       -> [b := (\c.cy)]
     (\c.cy)(\c.cy)       -> [c := (\c.cy)]
     (\c.cy)y             -> [c := y]
     yy
     ```
   * `(\x.\y.xyy)(\y.y)y`
     ```
     (\x.\y.xyy)(\y.y)y -> rename variables
     (\a.\b.abb)(\c.c)y -> [a := (\c.c)]
     (\b.(\c.c)bb)y     -> [c := b]
     (\b.bb)y           -> [b := y]
     yy
     ```
   * `(\a.aa)(\b.ba)c`
     ```
     (\a.aa)(\b.ba)c -> [a := (\b.ba)]
     (\b.ba)(\b.ba)c -> [b := (\b.ba)]
     (\b.ba)ac       -> [b := a]
     aac
     ```
   * `(xyz.xz(yz)(x.z)(\x.a)`
     ```
     (\xyz.xz(yz)(\x.z)(\x.a) -> rename non-free variables
     (\mno.mo(no)(\p.z)(\q.a) -> [m := (\p.z)]
     (\no.(\p.z)o(no))(\q.a)  -> [p := o]
     (\no.z(no))(\q.a)        -> [n := (\q.a)]
     (\o.z((\q.a)o)           -> [q := (o)]
     (\o.za)
     ```

