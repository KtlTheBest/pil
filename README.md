# Pil - Programming Language based on Kazakh

## Why?

For fun. The original project "til" was written in Go, but I don't like Go, so I decided to rewrite it in OCaml.

## How to run?
I tried to go without external dependencies, but there are some that you'll need:
```
opam install ocamlbuild
opam install batteries
opam install menhir
ln -s ~/.opam/default/lib/ocaml/threads/threads.cmxa ~/.opam/default/lib/ocaml/threads.cmxa
ln -s ~/.opam/default/lib/ocaml/threads/threads.a    ~/.opam/default/lib/ocaml/threads.a
```
You'll also need to patch your filesystem a bit, with soft-links.
You can do it yourself.
There should be a better way, but I'll look for it later.

After this you just need to make and run:
```
make
./main.native
```

I didn't add any features as to supply filename in the args.
The goal was to get running as fast as possible.

## What's next?
Don't know.
There are some known issues, for example, the language doesn't parse floats, even though there's a support for it.
Plus typing in Kazakh layout is a pain in general, so maybe I'll work on that instead.

## Contributing

Have fun with the language.
Feel free to contribute, make your own forks, improve on the language, or even overtake this one in terms of quality.

This project was a Proof-of-Concept (PoC).
Trying to make it a serious language will require some thinking and effort.

## Acknowledgements

This project is based on a idea shamelessly taken from a friend of mine, changed according to my flawed vision and rewritten in OCaml, because I don't know how to code in Go.

## Example
```
сан фибоначчи(сан н) функциясы
басы
    егер н тең 0 болса
        0 қайтар
    немесе н тең 1 болса
        1 қайтар
    әйтпесе
        (н-1)фибоначчи + (н-2)фибоначчи қайтар
    істе
аяғы

физзбазз() функциясы
басы
    айнымалы а = 0 бастап 100 дейін үшін
    басы
        егер а % 3 тең 0 болса
            ("физз")шығар
        немесе а % 5 тең 0 болса
            ("базз")шығар
        істе
    аяғы 
аяғы

айнымалы а = 10
айнымалы б = 20
айнымалы в = а + б

// шығар деген built-in print функциясы
(в)шығар

сан айнымалы он = 10
логикалық айнымалы булШын = шын
логикалық айнымалы булЖалған = жалған
сөз айнымалы сәлем = "гүлденген әлем, World, сәлем"
әріп айнымалы әӘріпі = 'ә'
сан[] айнымалы сандар = [ 1, 2, 3, 4 ] 
//сан[] айнымалы бөлшектер = [ 1.0, 2.0, 3.0, 4.0 ]

[1]сандар = 1

// greater than operator
егер 11 көбірек 10 болса
    ("он бір оннан көп")шығар
істеу

// less than operator
егер 10 кішірек 11 болса
    ("он он бірден кіші")шығар
істеу

// greater than or equal operator
егер 11 көпНеТең 10 болса
    ("он бір оннан көп не тең")шығар
істеу

// less than or equal operator
егер 10 кішіНеТең 11 болса
    ("он он бірден кіші не тең")шығар
істеу

// struct мысалы
Координата құрылымы
басы
    сан х
    сан у 
аяғы 

// басында (0, 0) болады
айнымалы нүкте = жаңа Координата
нүкте.х = 15
нүкте.у = 25

консольденОқуМысалы() функциясы
басы
    айнымалы жазу = ()консольденОқы
    ("Мен "+жазу+" деп жаздым")шығар
аяғы

// мүмкін консольденОқуМысалы() деп шығаруға мүмкіндік беру керек болу жөн болар 
()консольденОқуМысалы
```
