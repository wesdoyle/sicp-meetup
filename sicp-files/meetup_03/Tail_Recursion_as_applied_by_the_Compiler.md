# Tail Recursion as Applied in the Compiler
I was inspired to explore this topic by the note in SICP section 1.2, detailing the (assumed?) difference between compilation of Scheme and a language such as C:
> One reason that the distinction between process and procedure may be confusing is that most implementations of common languages (including Ada, Pascal, and C) are designed in such a way that the interpretation of any recursive procedure consumes an amount of memory that grows with the number of procedure calls, even when the process described is, in principle, iterative. As a consequence, these languages can describe iterative processes only by resorting to special-purpose “looping constructs” such as do, repeat, until, for, and while. The implementation of Scheme we shall consider in Chapter 5 does not share this defect. It will execute an iterative process in constant space, even if the iterative process is described by a recursive procedure. An implementation with this property is called tail-recursive. With a tail-recursive implementation, iteration can be expressed using the ordinary procedure call mechanism, so that special iteration constructs are useful only as syntactic sugar.

(Also of note, the footnote applied to this block says Scheme must be tail-recursive per its IEEE standard). 

I am searching for the "evidence" of tail recursion being applied to Scheme procedures by looking at the bytecode produced by the Guile compiler (version 3.x). 

## Factorial in Scheme

### Recursive, Unoptimized

Starting with a recursive factorial in _fact-scheme-recursive.scm_

	(define (fact n)
	   (if (= n 1)
	   1
	   (fact (- n 1))))

Compiled with the _guild compile_ command, using -O0 flag to turn off all optimization:

    $ guild compile -o fib-actheme-recursive.go -O0 fact-scheme-recursive.scm

Then we can look at the virtual machine instruction set output with _guild disassemble_:

    $ guild disassemble fact-scheme-recursive.go
    Disassembly of <unnamed function> at #xe8:
		...             
    
    Disassembly of fact at #x138:
    
       0    (instrument-entry 16366)                            at fact-scheme-recursive.scm:1:0
       2    (assert-nargs-ee/locals 2 18)   ;; 20 slots (1 arg)
       3    (static-ref 0 16325)            ;; #f                 at fact-scheme-recursive.scm:2:4
       5    (immediate-tag=? 0 7 0)         ;; heap-object?
       7    (je 9)                          ;; -> L1
       8    (static-ref 0 16298)            ;; #f
      10    (static-ref 1 16328)            ;; =
      12    (call-scm<-scm-scm 0 0 1 111)   
      14    (static-set! 0 16314)           ;; #f
    L1:
      16    (scm-ref/immediate 14 0 1)      
      17    (mov 13 18)                                           at fact-scheme-recursive.scm:2:11
      18    (make-immediate 12 6)           ;; 1                  at fact-scheme-recursive.scm:2:13
      19    (handle-interrupts)                                   at fact-scheme-recursive.scm:2:8
      20    (call 5 3)                      
      22    (receive 2 5 20)                
      24    (immediate-tag=? 17 3839 4)     ;; false?             at fact-scheme-recursive.scm:2:4
      26    (jne 62)                        ;; -> L5
      27    (static-ref 0 16313)            ;; #f                 at fact-scheme-recursive.scm:4:5
      29    (immediate-tag=? 0 7 0)         ;; heap-object?
      31    (je 9)                          ;; -> L2
      32    (static-ref 0 16274)            ;; #f
      34    (static-ref 1 16316)            ;; *
      36    (call-scm<-scm-scm 0 0 1 111)   
      38    (static-set! 0 16302)           ;; #f
    L2:
      40    (scm-ref/immediate 17 0 1)      
      41    (mov 16 18)                                           at fact-scheme-recursive.scm:4:7
      42    (static-ref 0 16310)            ;; #f    $ guild compile -o fact-scheme-recursive-optimized.go -O2 fact-scheme-recursive.scm             at fact-scheme-recursive.scm:4:10
      44    (immediate-tag=? 0 7 0)         ;; heap-object?
      46    (je 9)                          ;; -> L3
      47    (static-ref 0 16259)            ;; #f
      49    (static-ref 1 16271)            ;; fact
      51    (call-scm<-scm-scm 0 0 1 111)   
      53    (static-set! 0 16299)           ;; #f
    L3:
      55    (scm-ref/immediate 12 0 1)      
      56    (static-ref 0 16298)            ;; #f                 at fact-scheme-recursive.scm:4:16
      58    (immediate-tag=? 0 7 0)         ;; heap-object?
      60    (je 9)                          ;; -> L4
      61    (static-ref 0 16245)            ;; #f
      63    (static-ref 1 16301)            ;; -
      65    (call-scm<-scm-scm 0 0 1 111)   
      67    (static-set! 0 16287)           ;; #f
    L4:
      69    (scm-ref/immediate 8 0 1)       
      70    (mov 7 18)                                            at fact-scheme-recursive.scm:4:18
      71    (make-immediate 6 6)            ;; 1                  at fact-scheme-recursive.scm:4:20
      72    (handle-interrupts)                                   at fact-scheme-recursive.scm:4:15
      73    (call 11 3)                     
      75    (receive 8 11 20)               
      77    (handle-interrupts)                                   at fact-scheme-recursive.scm:4:9
      78    (call 7 2)                      
      80    (receive 4 7 20)                
      82    (mov 19 17)                                           at fact-scheme-recursive.scm:4:4
      83    (mov 18 16)                     
      84    (mov 17 15)                     
      85    (reset-frame 3)                 ;; 3 slots
      86    (handle-interrupts)             
      87    (tail-call)                     
    L5:
      88    (make-immediate 19 6)           ;; 1                  at fact-scheme-recursive.scm:3:4
      89    (reset-frame 1)                 ;; 1 slot
      90    (handle-interrupts)             
      91    (return-values)                 
    
    
    Disassembly of <unnamed function> at #x2a8:
		...          

As best I can tell, here's a (potentially oversimplified) summary of what happens:
 - Lines 3-14: Load the "=" procedure
 - Lines 16-24: Evaluate the _if_ expression. Jump to line 88 if n is 1.
 - Lines 27-41: Load the "*" procedure
 - Lines 42-55: Load the "fact" procedure
 - Lines 56-72: Load the "-" procedure
 - Line 73: Call the "-" procedure. This [call](https://www.gnu.org/software/guile/manual/html_node/Call-and-Return-Instructions.html) creates a new frame with space for 3 slots: 1 for the "-" procedure, and 2 for the operands.
 - Line 78: Call the "fact" procedure. This call creates a new frame with space for 2 slots: 1 for the "fact" procedure, and 1 for the operand. **Here is the "evidence" of the recursive process, with stack space growing linearly with the input size.**
 - Lines 82-87: Perform the exiting tail call of the overall procedure, in this case the "*" procedure plus 2 operands.
 - Lines 88-91: Return 1 if n was 1.

### Recursive, Optimized

Re-compiled with the guild command, this time using -O2 for all optimizations:

    $ guild compile -o fact-scheme-recursive-optimized.go -O2 fact-scheme-recursive.scm 

VM instruction output:

    $ guild disassemble fact-scheme-recursive-optimized.go
    Disassembly of <unnamed function> at #xe8:
		...               
    
    Disassembly of fact at #x128:
    
       0    (instrument-entry 16330)                              at fact-scheme-recursive.scm:1:0
       2    (assert-nargs-ee/locals 2 0)    ;; 2 slots (1 arg)
       3    (mov 1 0)                       
       4    (reset-frame 1)                 ;; 1 slot
       5    (handle-interrupts)             
       6    (tail-call-label 2)             ;; fact at #x7f7c600e1148
    
    
    Disassembly of fact at #x148:
    
       0    (instrument-entry 16328)                              at fact-scheme-recursive.scm:1:0
       2    (alloc-frame 5)                 ;; 5 slots
       3    (make-immediate 3 6)            ;; 1                  at fact-scheme-recursive.scm:2:13
       4    (=? 4 3)                                              at fact-scheme-recursive.scm:2:8
       5    (je 14)                         ;; -> L1
       6    (call-scm<-scm-uimm 0 4 1 3)                          at fact-scheme-recursive.scm:4:15
       8    (handle-interrupts)                                   at fact-scheme-recursive.scm:4:9
       9    (call-label 4 1 -9)             ;; fact at #x7f7c600e1148
      12    (receive 1 4 5)                 
      14    (call-scm<-scm-scm 4 4 3 4)                           at fact-scheme-recursive.scm:4:4
      16    (reset-frame 1)                 ;; 1 slot
      17    (handle-interrupts)             
      18    (return-values)                 
    L1:
      19    (mov 4 3)                                             at fact-scheme-recursive.scm:3:4
      20    (reset-frame 1)                 ;; 1 slot
      21    (handle-interrupts)             
      22    (return-values)                 
    
    
    Disassembly of <unnamed function> at #x1a4:
		...            

As best I can tell, here's what happens:
 - First Group Lines 0-6: Some kind of frame setup, then a call to the second chunk
 - Second Group Lines 3-5: Evaluate the _if_ expression. Jump to line 19 if n is 1.
 - Line 6: Call pre-compiled C routine for subtraction
 - Line 9:  Call the "fact" procedure again. **_[Call-label](https://www.gnu.org/software/guile/manual/html_node/Call-and-Return-Instructions.html)_ appears to create a new frame as _call_ does, so the linear recursion has not been optimized away.**
 - Lines 14-18: Call pre-compiled C routine for multiplication and return.
 - Lines 19-22: Return 1 if n was 1.

### Iterative, Optimized
_fact-scheme-iterative.scm_

    (define (factorial n) 
      (fact-iter 1 1 n))
    
    (define (fact-iter product counter max-count)
      (if (> counter max-count)
          product
          (fact-iter (* counter product)
                     (+ counter 1)
                     max-count)))

Compiled:

    $ guild compile -o fact-scheme-iterative-optimized.go -O2 fact-scheme-iterative.scm

Disassembled:

    $ guild disassemble fact-scheme-iterative-optimized.go
    Disassembly of <unnamed function> at #xe8:
	    ...          
    
    Disassembly of factorial at #x144:
    
       0    (instrument-entry 16337)                              at fact-scheme-iterative.scm:1:0
       2    (assert-nargs-ee/locals 2 1)    ;; 3 slots (1 arg)
       3    (make-immediate 2 6)            ;; 1                  at fact-scheme-iterative.scm:2:13
       4    (mov 0 1)                                             at fact-scheme-iterative.scm:2:2
       5    (mov 1 2)                       
       6    (handle-interrupts)             
       7    (tail-call-label 2)             ;; fact-iter at #x7f1df34d0168
    
    
    Disassembly of fact-iter at #x168:
    
       0    (instrument-entry 16334)                              at fact-scheme-iterative.scm:4:0
       2    (<? 0 1)                                              at fact-scheme-iterative.scm:5:6
       3    (jl 8)                          ;; -> L1
       4    (call-scm<-scm-scm 2 1 2 4)                           at fact-scheme-iterative.scm:7:17
       6    (call-scm<-scm-uimm 1 1 1 1)                          at fact-scheme-iterative.scm:8:17
       8    (handle-interrupts)                                   at fact-scheme-iterative.scm:7:6
       9    (tail-call-label -9)            ;; fact-iter at #x7f1df34d0168
    L1:
      11    (reset-frame 1)                 ;; 1 slot
      12    (handle-interrupts)             
      13    (return-values)                 
    
   
    Disassembly of fact-iter at #x1a0:
    
       0    (instrument-entry 16326)                              at fact-scheme-iterative.scm:4:0
       2    (assert-nargs-ee/locals 4 0)    ;; 4 slots (3 args)
       3    (mov 3 2)                       
       4    (mov 2 1)                       
       5    (mov 1 0)                       
       6    (reset-frame 3)                 ;; 3 slots
       7    (handle-interrupts)             
       8    (tail-call-label -22)           ;; fact-iter at #x7f1df34d0168
    
    
    Disassembly of <unnamed function> at #x1c8:
		...              

My interpretation of the first group for fact-iter:
 - Lines 0-3: Evaluate whether counter is greater than max-count. If so, jump to line 11 and return.
 - Line 4: Call to pre-compiled C routine for multiplication
 - Line 6: Call to pre-compiled C routine for addition
 - Line 9: [Tail-recursive](https://www.gnu.org/software/guile/manual/html_node/Call-and-Return-Instructions.html) call back to the start of the fact-iter routine, so as _not_ to create a new frame. **Here is the promised tail-recursion!**
- Line 11-13: Return product value.

## Considered but not (yet?) done

 - If we try to write the iterative version of the factorial in C (via recursion only, without a for loop), does a modern compiler use tail recursion?

