# Parser and Transpiler

Most parsing needs can be fulfilled by the `expr` or `stmt` parser functions as these are able to parse all sorts of expressions or statements supported by the program. Specific parsing functions can be found in Parser.hs if needed.

After loading src/Parser.hs into ghci, the general method to run a parser is 

```
parse <parser function> <expression to parse>
```

Examples: <br>

**Expression**
```
parse expr "((2+2) === (3*3))"
```

Output:
```
Result >< Comparison (Equals (Arithmetic (Add (JsVal (JSInt 2)) (JsVal (JSInt 2)))) (Arithmetic (Mul (JsVal (JSInt 3)) (JsVal (JSInt 3)))))
```

**Statement**
```
parse stmt "const a = (true || true);"
```

Output:
```
Result >< StmtConst (ConstDecl "a" (Logical (LOr (JsVal (JSBool True)) (JsVal (JSBool True)))))
```

_multiple statements can be parsed with the `stmts` parser_

To run the pretty printers, add any valid JS (that is supported by this parser) in a .js inside any folder in root/javascript/inputs

In a corrresponding folder file under root/javascript/output, the pretty printed version will appear after running `stack test`

Note that files under **Folder A** can only parse/pretty print Expressions. Other folders should be able to handle statements or expressions. For more description on what Statment or Expression is, check Parser.hs

For evaluating expressions, we have a `evalExpr` function. But the most convenient way to test evaluation is to modify existing or create new .js files in `Folder D` in inputs and the corresponding output folder will contain the evaluated value

## Running the Code

```
$ stack test
```

This will generate the transpiled JS files using the sample input JS files, by running your pretty printing function for each exercise.

## Running the Javascript Tests

In the javascript folder run:

```
$ npm i
$ npm run dev
```

All example scripts are stored within `javascript/inputs` and the output of your parser will be saved in `javascript/output`.

The tests on the page test:

- The generated code is valid JS (i.e. it runs without errors, including non-termination error)
- The generated code has certain properties of the original code (e.g. immutable variables are still immutable)
- The output is "prettified" from the input based on visual, side-by-side inspection
