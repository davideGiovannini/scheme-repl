# Scheme Interpreter
A Scheme interpreter written in Haskell following this tutorial: [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)



## Todos

#### Parsing
* Support [escaped characters](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.5) in strings (ex. \n, \r, \t, \\, ...).
* Change parseNumber to support the Scheme standard for [different bases](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4).
* Support [character literals](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4).
* Add support for [floats](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4).
* Add data types and parsers to support the full range of [Scheme numeric types](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1).
* add more tests

#### Evaluation
* Implement the [cond](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_idx_106) and [case](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_idx_114) expressions.
