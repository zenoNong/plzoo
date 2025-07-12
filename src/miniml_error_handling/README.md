# This is an Assignment project where I extend the basic miniml language to support for error and exception handling.

For better understanding please see the example.miniml_error_handling code file. There are many test cases you can try for this implementation.
> More features will be added in the future too.

## One example walkthrough to show the internal working how the interpreter is working to produce the result.
- Consider the input: `try 3 / 0 with 42;;` 
- The output will be: `42`.

Lets Analyse this part by part

## 1. **Lexical Analysis (lexer.mll)**

**Input:**  
```
try 3 / 0 with 42;;
```

**Tokens produced:**  
- `TRY`
- `INT 3`
- `DIVIDE`
- `INT 0`
- `WITH`
- `INT 42`
- `SEMISEMI`

---

## 2. **Parsing (parser.mly)**

**Tokens parsed into AST:**  
The parser recognizes the structure as (showing in AST style):

```ocaml
TryWith (
  Divide (Int 3, Int 0),
  Int 42
)
```

---

## 3. **Type Checking(type_check.ml)**

- `Divide (Int 3, Int 0)` is checked: both operands are `int`, so type is `int`.
- `Int 42` is `int`.
- Both branches of `TryWith` are `int`, so the whole expression is type `int`.

---

## 4. **Compilation (to Abstract Machine Instructions)**

The AST is compiled to a list of instructions:
```
[ITryWith ([IInt 3; IInt 0; IDiv], [IInt 42])]
```
- The first frame computes `3 / 0`.
- The second frame is the handler, which just pushes `42`.

---

## 5. **Execution (Abstract Machine)**

- The machine starts executing the `ITryWith` instruction.
- It runs the first frame:  
  - Push `3` → stack: `[3]`
  - Push `0` → stack: `[0; 3]`
  - `IDiv` pops `0` and `3`, sees divisor is `0`, pushes `MExn ("DivisionByZero", "Division by zero")` onto the stack.
- The machine sees an exception on the stack, so it runs the handler frame (`[IInt 42]`) with an empty stack:
  - Push `42` → stack: `[42]`
- The result is `42`.

---

## 6. **Printing the Result**

- The type checker determined the type is `int`.
- The value on the stack is `42`.
- The interpreter prints:
  ```
  - : int = 42
  ```