# Cronut

[![CI](https://github.com/rocketnia/cronut/actions/workflows/ci.yml/badge.svg)](https://github.com/rocketnia/cronut/actions/workflows/ci.yml)

It's more fun to program when we can forget what we're programming for. 

Cronut is (or rather, will be) a programming language that emphasizes two things:

* Cross-compilation ("cro-"). Cronut programs can be code-walked to compile them according to custom compiler backends. Who says we were ever trying to program just for Racket?

* Abstractions that we've poked holes through ("-nought"). Yes, we may be thoughtful about the abstractions we define so that they have a clear separation between interface and implementation, but no, that's not going to stop us from also writing custom compiler backends and fancy error messages that snoop around in those implementation details. At compile time, the program's implementation will be traversable in a kind of database, not unlike Prolog.

Cronut will be a language for Racket. While it will be usable within the Racket ecosystem, and largely cooperate with Racket's model of module compilation and package distribution, it will make some fundamentally distinct choices about how modules and types work:

* Unrestricted mutual recursion between modules and macro definitions: When multiple Cronut modules refer to each other using certain import forms, they'll be compiled together, without the "cycle in loading" errors from Racket. When multiple mutually recursive definitions are being macroexpanded, they'll be expanded concurrently rather than one at a time. The implementations of macros will be able to refer to the macros being defined rather than being isolated to a different phase level.

* No generativity for type definitions: Constructors and interfaces defined in Cronut code will have identities based on something that already uniquely identifies the code, such as its module path. Unlike Racket's structure types and generic interfaces, these won't need to generate unique identities using load-time side effects.

* Explicit annotation of side effects: We expect most Cronut code will be annotated to declare that it uses a collection of side effects tailored to its needs. This way, it doesn't bend over backward to avoid mechanisms that have efficient implementations on many target platforms, and at the same time, it doesn't overly couple itself to a particular platform's effect suite.

* Expressive control over the Expression Problem feature interaction matrix: Extension points are a rare thing to see in most software. If a piece of software has M extensions of one kind and N extensions of another kind, their (M + N) features can interact in (M * N) ways unless some helpers and intermediary protocols are sorted out. But hey, extensible things are fun and useful, even if we don't always see how to make the effort proportional to the number of extensions. With Cronut, we embrace that our designs will sometimes call for (M * N) effort. We'll just make sure it's easy to split all that effort across multiple files and codebases.

Once there's a simple version of Cronut up and running, I (rocketnia) am pretty excited to start using it. I've have had my visual novel engine Mise-en.cene on the back burner until I could polish up a new version of Cene, and Cronut will probably be my new playground for that project.


# Cronut in relation to the Era and Lathe projects

First, some background on Era, Cene, and Lathe:

Era is a project for developing new computing platforms that don't have certain complexities that existing systems have taken for granted, such as requiring packages to be installed under a particular name or loaded in a particular order, or requiring different parts of a codebase to be written in different programming languages.

Cene is a programming language that's part of the Era project. Cene is designed to have text files as source code and to cross-compile its abstractions to several platforms at once.

The Lathe project is a subset of the Era project. While Era is about building simplifying computing platforms in a more standalone sense, Lathe is about instantiating some (hopefully) simplifying concepts in existing programming languages. Lathe libraries serve as examples that stand apart from Era languages like Cene so that they can be understood piece by piece, and Lathe libraries can also serve as infrastructure for Era projects. To distinguish Lathe from Era, the Lathe libraries usually won't introduce a new module system to a language unless it doesn't have a predominating standard module system in the first place.

Cronut seems related to these projects in some deep ways. In particular, Cronut may represent an intermediate stage in Cene's development:

Cronut's module system is quite a bit better thought-out than Cene's lexical unit system at this point; we have ideas for features like cyclic imports, parameterized imports, and an Expression Problem solution that have all been missing from Cene. And of course, Cronut's cross-compilation-centric design is an idea right out of Cene.

Meanwhile, the Cene codebase itself could probably be factored into much tidier pieces if Racket had the kind of cyclic imports and feature interaction matrix manipulation that we have in mind for Cronut. Hence, Cronut could serve as an implementation language for Cene.

However, Cronut leaves behind some goals that are more critical to the Era project. Cronut itself isn't designed to be simple to reimplement, nor necessarily to encourage people to write more software that's simple to reimplement. The Era project regards programming as an essentially irresponsible activity that should be made as avoidable as possible, but Cronut is basically meant to make programming in Racket more fun.

While Cronut may serve as infrastructure for Cene and as an example of Cene concepts, it doesn't fit into Lathe because it isn't merely a library for Racket. It introduces new module system notions and new notions of user-defined type that aren't seamless with the existing ones.

In short, Cronut creates a new standalone platform, but it isn't a plaform that particularly aims to minimize and simplify the world of programming like the Era project does. It has different priorities. It embraces the fun parts of extensible design.

So, for now, Cronut is its own project. Maybe someday Era's priorities will shift so that it fits in.


## Installation and use

This is a library for Racket that implements a programming language. To install it, run `raco pkg install --deps search-auto` from the `cronut-lib/` directory, and then write a module starting with `#lang cronut/racket-default-reader` in your Racket program.

The interface to Cronut will eventually be documented in the `cronut-doc/` package's documentation.
