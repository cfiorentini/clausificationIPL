A new approach to clausification for Intuitionistic Propositional Logic
=======================================================================


We provide the following tools:

- an ASP generator to search for countermodels for a  sequent. 
- A clausifier that, given an intuitionistic formula &alpha;, performs the following steps:

  1. computes the set of IPL-clauses &Theta; obtained by clausifying  the formula &alpha; &rarr;$goal,
     where $goal is a special atom.

  2. yields the  ASP encoding   of the sequent &Theta; &rArr; $goal.

  We assume that &alpha; does not contain atoms starting with $ (in particular,  &alpha; does not contain $goal).

Quick use:

1. install Potassco (see next section)

2. run the ASP generator with one of the encoded sequents.


Generator
--------

To run the generator, you have to install

- [Potassco](https://potassco.org/), the Potsdam Answer Set Solving Collection

The file encoding the countermodel generation algorithm is   `generator.lp`.
You have to supply a file `.lp` encoding the input sequent:
example of  such files, referring to the examples discussed in the paper,  are available in the directory `paperExamples`.



To search for a countermodel  for the sequent &sigma; encoded in the file `sigma.lp`,  issue the command:

```console
clingo generator.lp  sigma.lp
```
We are assuming that both the  `.lp` files are in the current directory;
if not, you have to write the correct path to the files.

To search for more solutions,  use the option `-n`:

```console
clingo generator.lp -n k  sigma.lp      -- search for k solutions (k >= 1)
clingo generator.lp -n 0  sigma.lp      -- search for all the solutions 
```


To search for a minimal countermodel:


```console
clingo generator.lp minimize.lp  sigma.lp
```


Clausifier
----------

The clausifier is in the directory `clausifier`.

To use the clausifier, you have to install the [Haskell toolchain](https://www.haskell.org/downloads)
(see  the section  [Installation instructions](https://www.haskell.org/downloads#ghcup)), 
in particular:

- [GHC](https://www.haskell.org/ghc/): the Glasgow Haskell Compiler
- [cabal-install](https://cabal.readthedocs.io/en/3.6/): the Cabal installation tool for managing Haskell software.


**Compilation**

From the  root directory (i.e., the directory `clausifier` containing the file  `clausifier.cabal`) run the command:

```console
 cabal install
```



It should be printed a message like this:


```console
 ....
 Symlinking 'clausifier' to '/myHome/.cabal/bin/clausifier'
```


This means that `clausifier` is the command to launch the prover. Actually,
`clausifier` is a symbolic link to    `/myHome/.cabal/bin/clausifier`; if
the command `clausifier` is not found you have to add the directory `/myHome/.cabal/bin/` to
your `PATH` variable (or write the complete path to the command).



**Running**


The input formula must be written in a text file. A formula `F` is specified by the following syntax:

```console
  F :=     atom          // prop. variable 
        |  $false        // false
        |   ~F           // not 
        |  F & F         // and
        |  F | F         // or
        |  F => F        // implication
        |  F -> F        // implication
        |  F <=> F       // bi-implication
        |  F <-> F       // bi-implication	
```

Atoms starting with '$' are not allowed.
Examples of formulas:

```console
(a => b) | ( b => a )
a | (a -> b | ~b)
~ a | ~~a
( ((a1 <=> a2) =>  a1 & a2 & a3) & ((a2 <=> a3)  =>  a1 & a2 & a3)  & (( a3 <-> a1)  => a1 & a2 & a3 ) )  =>  a1 & a2 & a3  
```

You can also use the [TPTP syntax](http://tptp.cs.miami.edu/TPTP/QuickGuide/Problems.html).
For some examples of formulas (also in the TPTP syntax), see  the `.p` files in the directory `formulas`.



Assume that the input formula &alpha; is written in the file `alpha.p`.
To search for a countermodel for &alpha;,  you have to clausify  &alpha; as explained above.

You can run the command:


```console
 clausifier alfa.p
```

The command prints the ASP-encoding of the sequent corresponding to the formula &alpha;.
You can redirect the output in a file, e.g. `sigma.lp`


```console
 clausify form.p > sigma.lp
```

Now, the file `sigma.lp` can be used as input file for the generator:


```console
clingo generator.lp  sigma.lp
```

**Remarks**

1. The new atoms generated during the clausificatio have prefix '$' and the right atom of the sequent  is '$goal'.
    To guarantee the soundness, it is crucial that the input formula  &alpha; does not contain atoms starting with $.
2. The clausifier also semplifies the input formula &alpha;, thus  &alpha; can be any formula.
   
3. We have implemented different clausification procedures, use the help option `-h` for an account;
*weak clausificatio* is  close to the procedure discussed in the paper, with slight differences.
   
     