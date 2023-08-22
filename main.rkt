#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require racket/string racket/generator racket/runtime-path)

(define-runtime-path default "./sample_paragraphs.txt")

;;punctuation
(define punctuation #px"\\p{P}+")
(define (remove-punctuation str)
  ;;remove punctuation characters from STR.
  (string-replace str punctuation ""))

(module+ test  
  (check-true (string=? "cat" (remove-punctuation "cat.")))
  (check-true (string=? "Its" (remove-punctuation "It's"))))

;;paragraphs and topics
(define (topic? str)
  ;;ensure that STR is a topic.
  (and (string=? str (string-downcase str)) (not (regexp-match punctuation str))))
(define (with-topics? para tops)
  ;;check whether PARA contains one of the topics in TOPS.
  (let/cc break
    (for ((seg (in-list (string-split para))))
      (for ((top (in-list tops)))
        (cond ((string-ci=? top (remove-punctuation seg))
               (break #t)))))
    #f))
(define (make-paragraph-generator seq choose)
  ;;initialize a generator which `yield`s elements in SEQ for which CHOOSE returns `#t`.
  (generator
   ()
   (for ((p seq))
     (cond ((choose p) (yield p))))
   #f))

(module+ test
  (check-true (topic? "cat"))
  (check-false (or (topic? "Cat") (topic? "cat.")))
  
  (define gen (make-paragraph-generator (in-list (list "a" "ab" "abcde")) (lambda (s) (<= (string-length s) 3))))
  (check-true (string=? (gen) "a"))
  (check-true (string=? (gen) "ab"))
  (check-false (gen))

  (check-true (with-topics? "Cat." '("cat")))
  (check-false (with-topics? "Cats." '("cat"))))

;;accuracy
(define (accuracy typed reference)
  ;;return the accuracy (percentage of words typed correctly) of TYPED when compared to the prefix of REFERENCE that was typed.
  (let* ((typed-words (string-split typed))
         (reference-words (string-split reference))
         (tl (length typed-words))
         (rl (length reference-words)))
    (cond ((and (zero? tl) (zero? rl)) 100.0)
          ((or (zero? tl) (zero? rl)) 0.0)
          (else
           (* 100.0
              (/
               (for/fold ((n 0)) ((w1 (in-list typed-words)) (w2 (in-list reference-words)))
                 (if (string=? w1 w2) (add1 n) n))
               tl))))))

(module+ test
  (check-true (= 0.0 (accuracy "cute dogs" "Cute Dogs")))
  (check-true (= 50.0 (accuracy "cute dogs" "cute dogs.")))
  (check-true (= 0.0 (accuracy "I love cute dogs" "cute dogs")))
  (check-true (= 0.0 (accuracy "" "a")))
  (check-true (= 0.0 (accuracy "a" "")))
  (check-true (= 100.0 (accuracy "" ""))))

;;console IO
(define (universal-read-line)
  ;;just an alias
  (read-line (current-input-port) 'any))
(define (format-and-displayln template . segments)
  ;;print formatted strings
  (displayln (apply format template segments)))
(define (input-speed)
  ;;return the typed string and the words-per-minute (WPM) of the typed string.
  (call-with-values (lambda () (time-apply universal-read-line null))
                    (lambda (lst cpu real gc)
                      (define typed (car lst))
                      (values typed (/ (/ (string-length typed) 5.0) (/ real 60000.0))))))
(define (make-paragraph-logging-handler topics)
  ;;when one argument is provided, it creates a procedure that prints the maximum WPM and accuracy.
  ;;when no argument is provided, it creates a procedure in terms of GEN that asks for input, and then computes and print the accuracy and WPM.
  (define max-WPM (box 0.0))
  (define max-accuracy (box 0.0))
  (case-lambda
    (() (format-and-displayln "maximum WPM: ~a" (unbox max-WPM))
        (format-and-displayln "maximum accuracy: ~a" (unbox max-accuracy)))
    ((reference)
     (cond (reference
            (displayln "Type the following paragraph and then press enter/return.")
            (displayln "If you only type part of it, you will be scored only on that part.")
            (newline)
            (displayln reference)
            (newline)
            (define-values (typed WPM) (input-speed))
            (define accur (accuracy typed reference))
            (cond ((> accur (unbox max-accuracy)) (set-box! max-accuracy accur)))
            (cond ((> WPM (unbox max-WPM)) (set-box! max-WPM WPM)))
            (format-and-displayln "WPM: ~a" WPM)
            (format-and-displayln "accuracy: ~a" accur))
           (else (format-and-displayln "No more paragraphs about ~s are available." topics))))))
(define (make-main-loop output gen)
  ;;run the given OUTPUT and ask whether we are going to proceed.
  (let loop ()
    (output (gen))
    (newline)
    (displayln "Press enter/return for the next paragraph or type q to quit.")
    (if (string=? "q" (universal-read-line))
        (output)
        (loop))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline raco/command-name racket/contract)
  (define database-box (box default))
  (command-line
    #:program (short-program+command-name)
    #:once-any
    [("-d" "--database")
     d
     "Specify the database"
     (set-box! database-box d)]
    #:once-any
    [("-t" "--topics")
     =>
     (lambda (_ . lst)
       (contract (listof topic?) lst 'command-line 'topics)
       (call-with-input-file
         (unbox database-box)
         (lambda (in)
           (define gen (make-paragraph-generator (in-lines in) (lambda (l) (with-topics? l lst))))
           (define handler (make-paragraph-logging-handler lst))
           (make-main-loop handler gen))))
     '("Specify the topics")]))
