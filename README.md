# Map-Closure 0.4

The `map-closure` construct provides a mechanism for performing a non-standard interpretation.  This distribution contains an implementation the construct inside a simple custom Scheme interpreter, along with a variety of examples of its use, written to support a paper published in POPL 2007.  This was done for academic and expository purposes, so clarity and ease of implementation were the primary design goals.

## Installation
 
To compile Map-Closure you first need to install Chicken. On Debian do:
```sh
# apt install chicken-bin
```
To compile Map-Closure do:
```sh
$ cd source
$ make install
```
This takes about 25 seconds on an FX-60.

To run the examples do:
```sh
$ cd examples
% ./run
```
This takes under a second on an FX-60.

The output we get is in [examples/run.text](examples/run.text) in case you don't care to run it yourself.

## References

This distribution is associated with the following paper:

* Jeffrey Mark Siskind and Barak A. Pearlmutter, *First-Class Nonstandard Interpretations by Opening Closures*, in Proceedings of the 2007 Symposium on Principles of Programming Languages (POPL), January 2007, doi:10.1145/1190216.1190230, http://barak.pearlmutter.net/papers/popl2007-map-closure.pdf

```bibtex
@inproceedings{siskind-pearlmutter-popl-2007b,
 author={Jeffrey Mark Siskind and Barak A. Pearlmutter},
 title={First-Class Nonstandard Interpretations by Opening Closures},
 booktitle={Proceedings of the 2007 Symposium on Principles of Programming Languages},
 address={Nice, France},
 month=jan,
 year=2007,
 pages={71-6},
 doi={10.1145/1190216.1190230},
}
```

## Authors

written by:
*  **Jeffrey Mark Siskind**  
   School of Electrical and Computer Engineering  
   Purdue University  
   465 Northwestern Avenue  
   Lafayette IN 47907-2035 USA  
   voice: +1 765 496-3197  
   FAX:   +1 765 494-6440  
   qobi@purdue.edu  
   ftp://ftp.ecn.purdue.edu/qobi  
   http://www.ece.purdue.edu/~qobi
*  **Barak A. Pearlmutter**  
   Department of Computer Science  
   Maynooth University  
   Co. Kildare  
   Ireland  
   voice: +353 1 7086100  
   FAX:   +353 1 7086269  
   barak@pearlmutter.net  
   http://barak.pearlmutter.net

## Copyright

Copyright 2006 Purdue University and National University of Ireland Maynooth. All rights reserved.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
