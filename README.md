Erlang fgrep(1)
===============

```
usage: efgrep [-ln][-m max] string [file ...]

-l              list files with matching lines
-m max          max. number of pattern match errors
-n              numbers the output lines
```

This is intended more of an Erlang learning exercise than anything really practical.

Description
-----------

The paper on [Approximate Boyer-Moore String Matching][TU90] uses the [Boyer-Moore-Horspool][WKHORS] algorithm to implement two approximate string matching alogrithms for k-mismatches and k-differences.  However, [Daniel Sunday's][DMS90] Boyer-Moore [Quick Search][SUNQS] variant is slightly more efficient than [Horspool's][HORSPOOL].

This program implements a generalised Boyer-Moore-Sunday approximate string matching for k-mismatches.  For k=0, the program performs exact string searching.  The implemntation turns out to be slightly easier than the Horspool version presented by Tarhio & Ukkonen or [Kuei-Hao Chen's slides][KHC1].

The Sunday alogrithm noted that the text character one past the pattern window will factor into the next round of comparisions and can be used to determine the offset of the next pattern window.  Whereas Horspool relied on the text character at the end of the current pattern window to compute the offset of the next window.  Sunday precomputes a bad-character shift table where every character not in the pattern is assigned the pattern length plus one and every character in the pattern is assigned its offset from the right of its right most occurence plus one.  

So for the pattern "AGCT", m (the pattern length) is 4 and for k = 0 mismatches, the shift table looks like:

        k  A C G T *
        -------------
        0: 4 2 3 1 5

The shift table for k > 0 mismatches works simply by shortening the pattern length by k from the right.  So the shift table for k = 1, looks like:

        k  A C G T *
        -------------
        0: 4 2 3 1 5
        1: 3 1 2 4 4

The pattern is compared with the text from left-to-right.  If a mismatch occurs, the text character just right of the pattern window is used to determine the pattern window shift; otherwise the pattern matches and the position in the text can be reported.  Note the Sunday algorithm can do the character comparasions in any order, unlike regular Boyer-Moore which is strictly right-to-left order.


Examples
--------

These examples show how the pattern window shifts each iteration for values of 0 <= k < m.  A dot (.) indicates a mismatch and equals (=) a match.  


* k=0 m=4 pat=AGCT

        k  A C G T *
        ------------
        0: 4 2 3 1 5
        
        T T A A C G T A A T G C A G C T A
            ^   ^ ^       ^   ^
        A G C T
        .
                2 = shift[0][C]
        
            A G C T
            = .
                    1 = shift[0][T]
        
              A G C T
              = .
                      4 = shift[0][A]
        
                      A G C T
                      = .
                              2 = shift[0][C]
        
                          A G C T
                          .
                                  3 = shift[0][G]
        
                                A G C T
                                = = = =


* k=1 m=4 pat=AGCT

        k  A C G T *
        -------------
        0: 4 2 3 1 5
        1: 3 1 2 4 4
        
        T T A A C G T A A T G C A G C T A
        
        A G C T
        . .
              3 2    min(shift[1][A], shift[0][C])
        
            A G C T
            = . = .
                  2 1    min(shift[1][G], shift[0][T])
        
              A G C T
              = . .
                    4 4    min(shift[1][T], shift[0][A])
        
                      A G C T
                      = . .
                            2 2    min(shift[1][G], shift[0][C])
        
                          A G C T
                          . = = .
                                3 3    min(shift[1][A], shift[0][G])
        
                                A G C T
                                = = = =


* k=2 m=4 pat=AGCT

        k  A C G T *
        -------------
        0: 4 2 3 1 5
        1: 3 1 2 4 4
        2: 2 3 1 3 3
        
        T T A A C G T A A T G C A G C T A
        
        A G C T
        . . .
            2 3 2   min(shift[2][A], shift[1][A], shift[0][C])
        
            A G C T
            = . = .
        

* k=3 m=4 pat=AGCT

        k  A C G T *
        -------------
        0: 4 2 3 1 5
        1: 3 1 2 4 4
        2: 2 3 1 3 3
        3: 1 2 2 2 2
        
        T T A A C G T A A T G C A G C T A
        
        A G C T
        . . . .
          2 2 3 2    min(shift[3][T], shift[2][A], shift[1][A], shift[0][C])
        
            A G C T
            = . = .
        

* k=0 m=8 pat=GCAGAGAG

        k  A C G T *
        -------------
        0: 2 7 1 9 9

        G C A T C G C A G A G C G T A T G C A G A G A G

        G C A G A G A G
        = = = .
                        1
          G C A G A G A G
          .
                          2
              G C A G A G A G
              .
                              7
                            G C A G A G A G
                            = = .
                                            2
                                G C A G A G A G
                                = .
                                                2
                                    G C A G A G A G
                                    .
                                                    2
                                        G C A G A G A G
                                        = = = = = = = =


* k=1 m=8 pat=GCAGAGAG

        k  A C G T *
        -------------
        0: 2 7 1 9 9
        1: 1 6 2 8 8
        
        G C A T C G C A G A G C G T A T G C A G A G A G
        
        G C A G A G A G
        = = = . .
                      1 1
          G C A G A G A G
          . .
                        2 2
              G C A G A G A G
              . = .
                            2 7
                  G C A G A G A G
                  = = = = = = . =


References
----------

"A very fast substring search algorithm";  
Daniel M. Sunday; Communications ofthe ACM; August 1990;  
<https://csclub.uwaterloo.ca/~pbarfuss/p132-sunday.pdf>

"Approximate Boyer-Moore String Matching";  
Jorma Tarhio And Esko Ukkonen; 1990;  
<https://www.cs.hut.fi/u/tarhio/papers/abm.pdf>

"Approximate Boyer-Moore String Matching" Explained;  
Presention by Kuei-hao Chen;  
<http://t2.ecp168.net/webs@73/cyberhood/Approximate_String_Matching/BHM_approximate_string_Algorithm.ppt>

"Exact String Matching Algorithms";  
Thierry Lecroq;  
<http://www-igm.univ-mlv.fr/~lecroq/string/index.html>

Horspool on Wikipedia;  
<https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm>

Horspool Explained;  
Presention by Kuei-hao Chen;  
<http://alg.csie.ncnu.edu.tw/course/StringMatching/Horspool.ppt>

[DMS90]: https://csclub.uwaterloo.ca/~pbarfuss/p132-sunday.pdf

[TU90]: https://www.cs.hut.fi/u/tarhio/papers/abm.pdf

[KHC1]: http://t2.ecp168.net/webs@73/cyberhood/Approximate_String_Matching/BHM_approximate_string_Algorithm.ppt

[KHC2]: http://alg.csie.ncnu.edu.tw/course/StringMatching/Horspool.ppt

[LECROQ]: http://www-igm.univ-mlv.fr/~lecroq/string/index.html

[HORSPOOL]: http://www-igm.univ-mlv.fr/~lecroq/string/node18.html

[SUNQS]: http://www-igm.univ-mlv.fr/~lecroq/string/node19.html

[WKHORS]: https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm


Copyright
---------

Copyright 2017 by Anthony Howe.  All rights reserved.


MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
