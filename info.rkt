#lang info
(define collection "typing-tests")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/typing-tests.scrbl" ())))
(define pkg-desc "Use the command line to test your typing speed on paragraphs about a particular topic.")
(define version "0.0")
(define pkg-authors '(hin))
(define license '(Apache-2.0 OR MIT))
(define raco-commands '((typing
			 (submod typing-tests main)
			 "Use the command line to test your typing speed on paragraphs about a particular topic."
			 #f)))
