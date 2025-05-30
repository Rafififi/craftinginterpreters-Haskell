#HLox

HLox is a simple, expressive programming language inspired by Lox from Crafting Interpreters. It is currently under active development and aims to be a readable and concise scripting language with basic control flow features.
✨ Features Implemented

    ✅ for loops

    ✅ while loops

    ✅ print statements

    ✅ function calls

    ⚠️ return statement (partially implemented)

🚀 Getting Started

```
cabal install

cabal run // this will get you into the REPL
```

🧠 Language Features
🔁 For Loops

```
for (var i = 0; i < 5; i = i + 1) {
    print i;
}
```

🔁 While Loops

```
var count = 3;
while (count > 0) {
    print count;
    count = count - 1;
}
```

🖨️ Print Statements

```
print "Hello, world!";
print 42;
```



⚠️ Return Statement (In Progress)

The return keyword is being worked on and may not behave as expected in all contexts. Function return behavior is not yet stable.

```
fun greet() {
    return "Hello"; // May not work correctly yet
}
```

📌 Notes

    HLox syntax is C-style, but optimized for clarity and teaching.

    Great for learning interpreters and language design basics.

    Inspired by the Lox language from Robert Nystrom’s Crafting Interpreters.

🛠️ In Development

Planned additions include:

    Full function support with proper return semantics

    Scopes and closures

    Native functions and error handling
