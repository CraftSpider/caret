
# Caret DEF file format

The caret library allows one to parse many types of files, both to and from disk. This library is especially handy
for writing applications that need to read/write rare or custom file formats, or convert between formats that may
not be 1 to 1. Here you can learn how to write your own `.def` files, so you can jump right into the world of caret

## General syntax

`.def` files are built around 2 basic concepts: Sections and Constraints. A section represents a repeatable set of
data and rules, and a constraint is a rule that data must follow. A quick example:

```def
section Chunk {
    length : uint
    type : sint8[4]
    data : sint8[length]
    crc : uint
}

section EndChunk {
    length : 0x00000000
    type : "IEND"
    crc : uint
}

file PNG {
    magic : 0x89504E470D0A1A0A
    chunks : Chunk[]
    end_chunk : EndChunk
}
```

This is a minimal def file for parsing PNGs. Stepping through this line-by-line:
- `section Chunk {`: This declares a new section type named `Chunk`.
- `length : uint`: This declares our first bit of data, an unsigned integer that will be referred to as 'length' in the
  generated code.
- `type : sint8[4]`: This is a character array of length 4, so the next 4 bytes will be read in to fill the 'type' slot.
- `data : sint8[length]`: Our first dependent value. 'data' will be an array of characters of the same length as our
  previous 'length' value
- `crc : uint`: Another unsigned integer, named 'crc'
- `}` And a final closing bracket to finish this section

Overall, that should hopefully be very simple and intuitive. The next section will introduce constant constraints,
with both a hex literal and a string as their values. In that case, the constraint will enforce that that data exactly
is found in the file. The final thing we see is a `file` declaration, which behaves just like a section in syntax.
The only difference is that `file` tells caret this section is top-level, and that when we read a file we can request
that name and it will use this set of declarations.

There is much more to the syntax, and all the abilities and rules will be explained below. Also feel free to check out
the examples/ directory for more complex definitions

## Full Syntax

### Keywords

Caret has a relatively small number of keywords, in an attempt to keep the language simple

| keyword  | use                                                                              |
|----------|----------------------------------------------------------------------------------|
| type     | Placed on the top-level, declares rules used in this file                        |
| encoding | Placed on the top-level, declares the encoding to use for the input              |
| section  | Define sections of data with constraints that may even be used in multiple files |
| file     | Register a file type with caret, otherwise same as Section                       |
| context  | Access info about the current parsing status, for constraints                    |

### Types

Caret has a number of builtin types, for common operations

| type    | definition                      | size (in binary) | size (in text)                            |
|---------|---------------------------------|------------------|-------------------------------------------|
| sint8   | A signed character              | 1                | 1                                         |
| sint16  | A signed short                  | 2                |                                           |
| sint32  | A signed integer                | 4                | variable                                  |
| sint64  | A signed long                   | 8                |                                           |
| uint8   | An unsigned character           | 1                | 1                                         |
| uint16  | An unsigned short               | 2                |                                           |
| uint32  | An unsigned integer             | 4                | variable                                  |
| uint64  | An unsigned long                | 8                |                                           |
| float32 | A floating-point value          | 4                | variable                                  |
| float64 | A double-width floating point   | 8                | variable                                  |
| string  | A string of characters          | variable         | variable                                  |
| bool    | A boolean value                 | 1                | 'true' or 'false' with any capitalization |
| null    | A null value                    | 0*               | 0*                                        |
| []      | An array of values              | num or variable  | num or variable                           |
| *       | A pointer to a value            | 4 or 8           |                                           |

* Implementations may choose to support arbitrary sizing for sint/uint/float.
* Null is special. A null on its own has size 0, but a null in a union has the size of the smallest union member

### Encoding declaration

encoding is used to set the encoding rules for the input. The default encoding is binary, remember that 
binary encoding behaves different from most of the textual encodings.

- `encoding <encoding>`
    - Set the file encoding
    - Valid options include 'binary' (default), 'ascii', 'utf8', and 'utf16'

### Type declaration

type can be used to set up type aliases and change global type rules.

- `type <type> <rule>`
    - Set a type rule. The value rules are different for each type
    - string
        - nullencoded (default) - strings are null terminated
        - lengthencoded - strings start with their length numerically
        - quoted - strings are either single- or double- quoted
        - singledquoted - strings are single-quoted
        - doublequoted - strings are double-quoted
- `type <name> = <type> [rule]`
    - Set a type alias. The new name is equivalent to the given type (including type unions), or type with rule.

### Constraint syntax

Constraints provide limits on what data may be. All builtin types and defined sections can be used as a constraint,
but they also allow much more complex declarations, including conditional types or post-evalutation limits. The general
form is encapsulated as:

\[name] : \<type constraint> \[-> \<value constraint>]

The name is optional, and is used to refer to the value of that position in context and generated code. Without one,
the value is anonymous and cannot be referred to.

The type constraint is what type to read the value as, and is evaluated before/as anything is read

The value constraint places limits on the value, is optional, and is evaluated after everything is read

Types of constraint:
- Literal
    - Integer literal: 1, -7, 0xFF
    - Float literal: 1.f, 1.0
    - String literal: "Test"
    - Unicode literal: u"Test"
    - Regex to match: r"\s+"
    - Single character: 'C'
    - Unicode character: u'C'
    - Boolean value: true, false
    - Array value: \[1, 2, false]
- Type
    - Builtin: sint32, bool, null
    - Section: SectionName, FileName
    - Array: Section\[4], Section[]
    - Pointer: sint32\*, Section\*
- Union
    - Any set of types, joined by |
    - sint32|null
    - Section|sint32[]
- Conditional
    - An expression followed by the type if expression is true, then if expression is false
    - ? (1 is 1 or not a and b) \<true> \<false>
- Global
    - A limit that all pointers to objects of a type must appear here. Does not otherwise guarantee anything
    - @SectionName

Global functions (for expressions):

| name                   | behavior                        |
|------------------------|---------------------------------|
| isunique(value, array) | Checks value is unique in array |

### Context object

There exists a special object that is available in all expressions named 'context'. It has several attributes which
represent various bits of parsing state for conditionals.

| Attribute       | Value                          |
|-----------------|--------------------------------|
| section.\[name] | File section                   |
| array           | Current array or null          |
| index           | Current index in array or null |

### Section/File syntax

Sections (and files) are simply a name with a series of constraints. They are defined simply as:

```
section A {
    constraint
    constraint
    ...
}

file B {
    constraint
    constraint
    ...
}
```

## Design Rules (Needs cleanup)

- Need to define sections

- Need to define byte length, repetition, type (pointer to section, base type), nullabillity

- Need to be able to define traits in terms of another value

- Read and write must match, a definition must work both ways

- Arrays without size are lazy, they match *minimum* number

- Parsing is done linearly, except in the presence of conditionals based on other sections

- Saving, pointers will be saved in any order
    - Unless a constraint is on the file that they must be in a given location

- Type/section names are evaluated lazily, they may be used before they're defined as long as they're defined somewhere
