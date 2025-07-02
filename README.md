# Typing Problems for Lambda Calculus

This project was developed as part of the PEEC 2024/2025 program.

**Tutor:** António Mário da Silva Marcos Florido  
**Co-tutor:** Sandra Maria Mendes Alves

This project implements **Type Checking** and **Type Inference** algorithms in Haskell for a subsystem of the Lambda Calculus.

## Supported Structures

### Type Checking

The following structures are supported for type checking:

  * **Variables**
  * **Applications**
  * **Abstractions**
  * **Pairs**
  * **Booleans**
  * **Naturals**
  * **If-then-else statements**

### Type Inference

The following structures are supported for type inference:

  * **Variables**
  * **Applications**
  * **Abstractions**

## How to Use

### Type Checking

To perform type checking, use the `typeCheck` function with a term, a type, and a context:

```haskell
typeCheck term type context
```

**Example:**

```haskell
typeCheck (Lambda ('w', VarType 'D') (Var 'w')) (Arrow (VarType 'D') (VarType 'D')) context
```

### Type Inference

To perform type inference, use the `infer` function with a term:

```haskell
infer term
```

**Example:**

```haskell
infer (Fst (Zero, True))
```

