## Introduction
This document is intended to serve as a representation of my design goals for Honeybee (working title).
It encompasses my ideas of what makes a language "good" and simple to use, not just for the developer --
but for the person who has to review or debug the code later too. A lot of code is written once but read
many times. This should be reflected in the visual structure of the language: it should be simple to
understand, given a piece of code, what it is supposed to do and how it does it. This is achieved
through consistency without weird quirks or exceptions. The goals and principles of Honeybee are the following:

 - It does only one thing
 - The arrow of execution goes one way
 - Everything works the same way
 - What the language can, the user can too
 
### It does only one thing
A single call, or statement, or expression, should do only one thing. If it does multiple things, then it will be easy
to confuse or foget about the secondary effect. Double effects (implicit or otherwise) and the resulting ambiguity is
very easy to create.

#### Declaration and mutation
Consider the following program in some fictional language:
```
y = 3;
x = y;
y = 4;
```
The first statement _declares_ a variable called `y` and assigns to it the integer `3`. That's stating the obvious, and
there is nothing really wrong with that.

What makes matters interesting is that the same operator `=` can be used as an _assignment_ as well, as in the third
statement. If it were always both a declaration, the third line should throw a "variable `y` already
exists in scope" type of error (or at the very least a warning).

It is also unclear _how_ exactly `x` is made to be equal to `y`. Is it by reference or by value? When the program ends,
will `x` be equal to `3` or to `4`?

In Honeybee, it will be made explicit through the operators `:=` and `&=`:
```
x := 3;      // Variable declaration: a new variable is made and initialized to 3
y := x;      // y is created and set to be a copy of x (by value)
x = 4;       // x is changed and now represents the number 4; y remains at 3

z &= x;      // z is created and set to be an alias of x (reference-like)
z = 5;       // z now represents the number 5, but as z aliases x
             //   this means x is set to equal 5 too.

w = 4;       // Will throw a syntax error: variable use before declaration
```
This way it is more difficult to accidentally pass a variable by reference and mutate it when you actually wanted to
mutate a copy. But if you wanted to mutate the given object through an alias, for example a deeply nested array, you
have the power to do so.

##### Why not a `let` keyword?
Because something such as `let x = 3` and `x = 4` use the same binary operator `=` but they mean seemingly similar, but
actually radically different things: one initializes, the other mutates. Having these operations share the same
operator makes this distinction more vague when it should be crystal clear. I would consider `let x = 3` to actually be
a statement using a `let=` pre-circumfix operator with two arguments, `x` and `3`. Think about that for a second: a
_pre-circumfix_ operator for something as common and elementary as a variable initialization, the second part of which
shares its token with another operator!

##### How about references to static values?
There will be nothing that prevents you from assigning an alias to the number `3`. Preventing that would violate the
principle that all things work in the same way. However, the number `3`, being something static and immutable, means
that any alias to it cannot be mutated either:
```
x &= 3;
x = 4;      // Fatal error: trying to mutate immutable object
```

##### How about unintended mutations due to passes by reference?
Variables are mutable by default. This allows for easy manipulation, but also for easy error. Consider the following
example of not quite Honeybee code:
``` 
foo := (&x) -> {
    x += 1;
}

x := 3;
y &= x;

foo(y);
```
The function parameter signature shows that the variable is passed as reference. 