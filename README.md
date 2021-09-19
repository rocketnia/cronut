# Cronut

[![CI](https://github.com/rocketnia/cronut/actions/workflows/ci.yml/badge.svg)](https://github.com/rocketnia/cronut/actions/workflows/ci.yml)

It's more fun to program when we can forget what we're programming for. 

Cronut is (or rather, will be) a programming language that emphasizes two things:

* Cross-compilation ("cro-"). Cronut programs can be code-walked to compile them according to custom compiler backends. Who says we were ever trying to program just for Racket?

* Abstractions that we've poked holes through ("-nought"). Yes, we may be thoughtful about the abstractions we define so that they have a clear separation between interface and implementation, but no, that's not going to stop us from also writing custom compiler backends and fancy error messages that snoop around in those implementation details. At compile time, the program's implementation will be traversable in a kind of database, not unlike Prolog.

Cronut will be a language for Racket. Although it will be usable within the Racket ecosystem, and while it may largely cooperate with Racket's model of module compilation and package distribution, it will reimagine certain things about how modules and user-defined data types work:

* Mutual recursion between modules: Certain ways of structuring codebases lead to files that have compile-time dependencies on each other, especially when the codebase defines a variety of subsystems that are each useful during each other's metaprogramming. In Racket, dependencies like these lead to "cycle in loading" errors. In Cronut, we're exploring letting a file declare its participation in a dependency cycle. In terms of Racket's compilation model, a whole Cronut cycle can be compiled in one place, and the rest of the files can redirect to it.

* Concurrent cooperation between declarations at compile time: Cronut's approach to the macroexpansion of module-level declarations may involve the use of compile-time concurrency to make sense of mutual recursion and other interactions. Racket's macroexpander is rather determined to fully macroexpand one part of a file before moving on to the next, even if to do so, it has to commit to assumptions that certain variables are not macros (which is when it inserts `#%top`). With concurrency, these parts of macroexpansion can block until the necessary information is available.

* Type definitions without generativity: Constructors and interfaces defined in Cronut code will typically have identities based on something that already uniquely identifies the code, such as its module path. Unlike Racket's structure types and generic interfaces, these won't need to generate unique identities using load-time side effects.

* Explicit annotation of side effects: Potentially, Cronut code may be statically annotated to declare that it uses a collection of side effects tailored to its needs. This way, the code can make direct use of procedural side effects that have efficient implementations on many target platforms, and at the same time, these effects will be part of its interface and can be compiled differently if the user is targeting a different platform with unique needs.

* Expressive control over the Expression Problem feature interaction matrix: Expressive extensibility is a scarce thing in most software, and perhaps for good reason. If a piece of software has M extensions of one kind and N extensions of another kind, their (M + N) features can interact in (M * N) ways unless some default-behavior helpers and intermediary protocols are sorted out. If these two extension points come from different independently developed libraries, it may be no one but their users who can understand both libraries' needs and design the right intermediary protocols. But hey, extensible things are fun and useful, even before we have a scalable maintenance plan for the entire interaction space. With Cronut, we embrace that sometimes users will be faced with the need to fill in (M * N) gaps in the user experience of the libraries they're using. We'll just make sure it's possible (and ideally, easy) for users to do this, by making feature interaction matrices a dedicated entity in the language. This will be unlike Racket, where there's no mechanism for users to write a specialization of one library's structure type for another library's generic interface.

* Eventual collapse of compile-time phases: Due to Cronut's cross-compilable abstractions and its more careful use of module-level side effects, Cronut will tend to have definitions that make sense at multiple Racket phases. Typically, they won't use phase-specific module state, and they'll be able to compile to Racket code for other phases to run. In the long run, for codebases that make pervasive use of these Cronut abstractions, the Racket notion of phase separation may not serve a purpose. To reflect this, Cronut may at some point define a language where variable bindings are shared across phases by default, with the non-default behavior being reserved for Racket interaction. Cronut programs whose definitions fail to stratify into distinct phases may risk deadlock during their concurrent macroexpansion process, so there is a correctness incentive to keep things stratified, but we expect a proper approach to such stratification will also account for Cronut's other concurrency techniques. Until such an approach is available, we'll probably tolerate some compile-time deadlock debugging.

* Embracing behind-the-scenes APIs: Many libraries have unsafe or experimental interfaces. Some have interfaces that are mostly stable, but they have unstable usage modes, like observing what order a hash map is iterated in or what happens to its behavior when its keys are mutated. Cronut will introduce at least one more unstable API to most libraries that use it, namely a user-accessible intermediate representation of the code of its abstractions so they can be hooked up to user-defined compiler backends. Making the source code available like this will probably make information hiding more difficult in Cronut than it is in Racket, but there may be a way to steer into the slide: If we programmatically encode the idea that some observations of an abstraction are unstable, we can pass around more information that's useful for error reporting, optimization, and cross-compilation with a more tool-assisted awareness of when our code relies on it so that we can avoid it in our  mainline program behavior.

* Modules and collections with compile-time arguments: Sometimes a library author has a need to release multiple compiled variants of a library, but not the luxury to be able to predict the full possibility space and compile it all ahead of time. Perhaps they want people to be able to fake any number of the library's dependencies for the sake of unit testing, even though some of those dependencies define macros or other compile-time abstractions that affect the library's compilation. Writing a macro system with compile-time arguments seems to be a rather tricky problem to reconcile with Racket's usual method of compile-time memoization (.zo files) and run-time memoization (module registries) of modules. Racket modules can have side effects, and the memoization controls how often those happen, but for a Racket module that can be referenced multiple times with different arguments, there may be a need for different parts of the module to have different memoization granularities for caching their side effects, and the place to store these extra compile-time results in the filesystem is unclear. We'll be experimenting with this for Cronut anyway.

* Perhaps even package names with dependency-declaration-time arguments: Sometimes a Racket package has architecture-specific, OS-specific FFI dependencies. The user may not want to download every possible system-specific build of the library they're installing; instead, they might want to install it with arguments. Some simple argument pattern-matching and constructor-calling code could be written in a package's info.rkt without making that information Turing-complete, and that could be a way for Racket's package manager to be nicer for polyglot development projects. We probably won't shake up the package manager anytime soon with Cronut, but if we do, this might be a direction we explore.

* Modules, collections, and perhaps even package names that count as generic interface methods: This would let us express compile-time first-class modules using compile-time objects that specialize a module-valued interface method. Racket's submodules would be one of the things we could express this way, but these simulated submodules would have a certain advantage over the originals: Since these would be exposed via regular module exports, we could choose to keep some of these submodules private.

The above is a rather ambitious set of goals, but we might not have to tackle them all at once. One of the latest experiments (still incomplete) keeps most of the Racket macro system and user-defined type system intact and just extends modules with support for cycles and compile-time arguments. In the short term, we might only need a small set of compile-time argument combinations which *can* be predicted and compiled in advance, thus simplifying the question of where to store the compilation results. Once these baseline macro system features are in place, some of the reimagined user-defined types, reimagined macros, and code-available function abstractions can be expressed in a more Racket-library-like style rather than requiring deep integration with the macro system.

Once there's any kind of simple version of Cronut up and running, I (Rocketnia) am pretty excited to start using it. I've have had my visual novel engine Mise-en.cene on the back burner until I could polish up a new version of Cene, and Cronut will probably be my new playground for that project.


## Cronut in relation to the Era and Lathe projects

First, some background on Era, Cene, and Lathe:

Era is a project for developing new computing platforms that don't have certain complexities that existing systems have taken for granted. Era systems shouldn't require packages to be installed under a particular name or loaded in a particular order, nor should they require different parts of a codebase to be written in different programming languages.

Cene is a programming language that's part of the Era project. Cene is designed to have text files as source code and to cross-compile its abstractions to several platforms at once.

The Lathe project is a subset of the Era project. While Era is about building simplifying computing platforms in a more holistic sense, Lathe is about instantiating some (hopefully) simplifying concepts in existing programming languages. Lathe libraries serve as examples of Era techniques that aren't quite so tangled up with unrelated parts of Era, so that these techniques can be understood piece by piece. Lathe libraries can also serve as infrastructure for Era projects. To distinguish the Lathe libraries from the Era languages, the Lathe libraries usually won't introduce a new module system to a language ecosystem unless it doesn't have a predominating standard module system in the first place.

Cronut seems related to these projects in some deep ways. In particular, Cronut may represent an intermediate stage in Cene's development:

* Cronut's module system is quite a bit better thought-out than Cene's lexical unit system at this point. We have ideas for features like cyclic imports, parameterized imports, and an Expression Problem solution that have all been missing from Cene. Cronut's cross-compilation-centric design is an idea right out of Cene.

* Meanwhile, the Cene codebase itself could probably be factored into much tidier pieces if Racket had the kind of cyclic imports and feature interaction matrix manipulation that we have in mind for Cronut. Hence, Cronut could serve as a more conducive implementation language for Cene for reasons more tangible than mere similarity of design.

However, Cronut leaves behind some goals that are more critical to the Era project. We won't necessarily try to make Cronut simple to reimplement, nor necessarily to encourage people to write more software that's simple to reimplement. The Era project regards programming as an essentially irresponsible activity that should be made as avoidable as possible, but Cronut is basically meant to make programming in Racket more fun.

While Cronut may serve as infrastructure for Cene and as an example of Cene concepts, it doesn't fit into Lathe because it isn't merely a library for Racket. It introduces new module system notions and new notions of user-defined type that aren't seamless with the existing ones.

In short, Cronut creates a new standalone platform, but it isn't a plaform that particularly aims to minimize and simplify the world of programming like the Era project does. It has different priorities. It embraces the fun parts of extensible design.

So, for now, Cronut is its own project. Maybe someday Era's or Cronut's priorities will shift to such an extent that we can revisit this decision.


## Installation and use

This is a library for Racket that implements a programming language. To install it, run `raco pkg install --deps search-auto` from the `cronut-lib/` directory, and then write a module starting with `#lang cronut/racket-default-reader` in your Racket program.

The interface to Cronut will eventually be documented in the `cronut-doc/` package's documentation.
