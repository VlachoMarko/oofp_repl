# REPL Calculators - Integer and Multiset

This project was developed as part of the course **Object-Oriented and Functional Programming** at **Vrije Universiteit Amsterdam (VU)**. The aim is to implement two REPL (Read-Eval-Print-Loop) calculators in Scala:
1. **Integer Calculator REPL**
2. **Multiset Calculator REPL**

Both calculators support expression evaluation, variable assignment, and expression simplification. The design encourages code reuse between the two REPLs using advanced Scala techniques like pattern matching, inheritance, and abstract types.

## Features

### Commands
- **Expression Evaluation**: Handles arithmetic operations or multiset operations.
  - Example (Int): `1 + 4 * 3` → `13`
  - Example (Multiset): `{a, b} + {b, c}` → `{a, b, b, c}`
  
- **Variable Assignment**: Binds expressions to variables.
  - Example: `n = 3 * 4 + 2` → `n = 14`
  
- **Expression Simplification**: Simplifies expressions based on defined rules.
  - Example: `@ (x * 2 + x * 3)` → `x * 5`

### Supported Operators

#### Integer REPL:
- Arithmetic: `+`, `-`, `*`
  
#### Multiset REPL:
- **Summation**: `{a, b} + {b, c}` → `{a, b, b, c}`
- **Intersection**: `{a, b} * {b, c}` → `{b}`
- **Subtraction**: `{a, b, b} - {b}` → `{a, b}`

### Simplification Rules

#### Integer REPL:
- `0 + e → e`, `1 * e → e`
- Distributivity: `(a * b) + (a * c)` → `a * (b + c)`

#### Multiset REPL:
- `{} + e → e`, `e * {} → {}`
- `e * e → e`, `e - e → {}`

### Code Reuse Techniques
- **Inheritance and Abstract Classes**: Both REPLs extend from a common base class `REPLBase`.
- **Pattern Matching**: Used for simplifying expressions.
- **Type Abstraction**: Shared evaluation and parsing logic via abstract type `Base`, which is concretized as either `Int` or `Multiset`.
