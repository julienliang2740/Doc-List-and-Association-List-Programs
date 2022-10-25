;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; A doc-list (DL) is one of:
;; * empty
;; * (cons Str DL)
;; Requires: each doc (i.e. Str) only occurs once in the doc-list
;; the doc-list is in lexicographic order

;; An Inverted List (IL) is one of:
;; * empty
;; * (cons (list Str DL) IL)
;; Requires: each key (i.e. Str) only occurs once in the IL.
;; the keys occur in lexicographic order in the IL.

;;
;; a)
;;

;; (both DL1 DL2) consumes two DLs and produces the strings that occurs in both DLs

;; Examples:
(check-expect (both (list ) (list )) (list ))
(check-expect (both (list "a.txt" "b.txt") (list "b.txt" "c.txt")) (list "b.txt"))
(check-expect (both (list "c.txt") (list "c.txt")) (list "c.txt"))

;; both: DL DL -> DL
(define (both DL1 DL2)
  (cond
    [(or (empty? DL1) (empty? DL2)) empty]
    [(cons? DL1) (cond
            [(find (first DL1) DL2) (cons (first DL1) (both (rest DL1) DL2))]
            [(not (find (first DL1) DL2)) (both (rest DL1) DL2)])]))

;; (find a-string DL) consumes a string and a DL and determines whether said string is in the DL
;; find: Str DL -> Bool
;; examples:
(check-expect (find "a" (list "a" "b" "c")) true)
(check-expect (find "f" (list "a" "b" "c")) false)

(define (find a-string DL)
  (cond
    [(empty? DL) false]
    [(string=? a-string (first DL)) true]
    [else (find a-string (rest DL))]))

;; Tests:
(check-expect (both (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt")
                    (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt"))
              (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt"))
(check-expect (both (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt")
                    (list "a.txt" "b.txt" "c.txt"))
              (list "a.txt" "b.txt" "c.txt"))
(check-expect (both (list "b.txt" "d.txt" "e.txt")
                    (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt"))
              (list "b.txt" "d.txt" "e.txt"))
(check-expect (both (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt")
                    (list ))
              (list ))

;;
;; b)
;;

;; (exclude DL1 DL2) consumes two DLs and produces the DL that occurs
;; in the first DL but not the second one

;; Examples:
(check-expect (exclude (list ) (list )) (list ))
(check-expect (exclude (list "a.txt" "b.txt") (list "b.txt" "c.txt")) (list "a.txt"))
(check-expect (exclude (list "a.txt") (list "a.txt")) (list ))

;; exclude: DL DL -> DL
(define (exclude DL1 DL2)
  (cond
    [(empty? DL1) empty]
    [(cons? DL1) (cond
                   [(find (first DL1) DL2) (exclude (rest DL1) DL2)]
                   [(not (find (first DL1) DL2)) (cons (first DL1) (exclude (rest DL1) DL2))])]))

;; Tests:
(check-expect (exclude (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt")
                    (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt"))
              (list ))
(check-expect (exclude (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt")
                    (list "a.txt" "b.txt" "c.txt"))
              (list "d.txt" "e.txt"))
(check-expect (exclude (list "b.txt" "d.txt" "e.txt")
                    (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt"))
              (list ))
(check-expect (exclude (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt")
                    (list ))
              (list "a.txt" "b.txt" "c.txt" "d.txt" "e.txt"))

;;
;; c)
;;

;; (keys-retrieve doc an-il) consumes a Str and an IL and produces a (listof Str)
;; with lexicographic ordering

;; Examples:
(check-expect (keys-retrieve "a.txt" (list (list "" (list ))))
              (list ))
(check-expect (keys-retrieve "a.txt" (list (list "barks" (list "b.txt"))
                                           (list "cat" (list "a.txt" "c.txt"))
                                           (list "chases" (list "c.txt"))
                                           (list "dog" (list "b.txt" "c.txt"))
                                           (list "sleeps" (list "a.txt"))
                                           (list "suddenly" (list "c.txt"))
                                           (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "cat" "sleeps" "the"))
(check-expect (keys-retrieve "b.txt" (list (list "barks" (list "b.txt"))
                                           (list "cat" (list "a.txt" "c.txt"))
                                           (list "chases" (list "c.txt"))
                                           (list "dog" (list "b.txt" "c.txt"))
                                           (list "sleeps" (list "a.txt"))
                                           (list "suddenly" (list "c.txt"))
                                           (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "barks" "dog" "the"))

;; keys-retrieve: Str IL -> (listof Str)
(define (keys-retrieve doc an-il)
  (cond
    [(empty? an-il) empty]
    [(cons? an-il) (cond
                     [(find doc (first (rest (first an-il))))
                      (cons (first (first an-il)) (keys-retrieve doc (rest an-il)))]
                     [(not (find doc (first (rest (first an-il)))))
                      (keys-retrieve doc (rest an-il))])]))

;; Tests:
(check-expect (keys-retrieve "a.txt" (list (list "barks" (list ))
                                           (list "cat" (list ))
                                           (list "chases" (list ))
                                           (list "dog" (list ))
                                           (list "sleeps" (list ))
                                           (list "suddenly" (list ))
                                           (list "the" (list ))))
              (list ))
(check-expect (keys-retrieve "a.txt" (list (list "abandon" (list "a.txt"))
                                           (list "book" (list "a.txt"))
                                           (list "car" (list "a.txt"))
                                           (list "dog" (list "a.txt"))
                                           (list "echo" (list "a.txt"))
                                           (list "foxtrot" (list "a.txt"))
                                           (list "golf" (list "a.txt"))))
              (list "abandon" "book" "car" "dog" "echo" "foxtrot" "golf"))
(check-expect (keys-retrieve "z.txt" (list (list "abandon" (list "a.txt"))
                                           (list "book" (list "a.txt"))
                                           (list "car" (list "a.txt"))
                                           (list "dog" (list "a.txt"))
                                           (list "echo" (list "a.txt"))
                                           (list "foxtrot" (list "a.txt"))
                                           (list "golf" (list "a.txt"))))
              (list ))

;;
;; d)
;;

;; (search operation str2 str2 an-il) consumes a Sym (either 'both or 'exclude), two Strs, and an IL
;; and produces a DL
;; When the Sym is 'both it will produce a DL containing the documents present in both keys'
;; (str1 and str2 from an-il) associated DLs
;; When the Sym is 'exclude it will produce a Dl containing the documents present in DL associated
;; the key str1 but not key str2

;; Examples:
(check-expect (search 'both "barks" "dog" (list (list "barks" (list "b.txt"))
                                                (list "cat" (list "a.txt" "c.txt"))
                                                (list "chases" (list "c.txt"))
                                                (list "dog" (list "b.txt" "c.txt"))
                                                (list "sleeps" (list "a.txt"))
                                                (list "suddenly" (list "c.txt"))
                                                (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "b.txt"))
(check-expect (search 'exclude "cat" "dog" (list (list "barks" (list "b.txt"))
                                                (list "cat" (list "a.txt" "c.txt"))
                                                (list "chases" (list "c.txt"))
                                                (list "dog" (list "b.txt" "c.txt"))
                                                (list "sleeps" (list "a.txt"))
                                                (list "suddenly" (list "c.txt"))
                                                (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt"))

;; search: Sym Str Str IL -> DL
;; Requires: operation to either be 'both or 'exclude, both str1 and str2 to be valid keys
(define (search operation str1 str2 an-il)
  (cond
    [(symbol=? 'both operation) (both (DL-extracter str1 an-il) (DL-extracter str2 an-il))]
    [(symbol=? 'exclude operation) (exclude (DL-extracter str1 an-il) (DL-extracter str2 an-il))]
    ))

;; (DL-extracter a-string an-il) consumes a string and an IL and produces the DL of the string
;; example:
(check-expect (DL-extracter "cat" (list (list "barks" (list "b.txt"))
                                        (list "cat" (list "a.txt" "c.txt"))
                                        (list "chases" (list "c.txt"))
                                        (list "dog" (list "b.txt" "c.txt"))
                                        (list "sleeps" (list "a.txt"))
                                        (list "suddenly" (list "c.txt"))
                                        (list "the" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt" "c.txt"))
;; DL-extracter: Str IL -> DL

(define (DL-extracter a-string an-il)
  (cond
    [(empty? an-il) empty]
    [(string=? a-string (first (first an-il))) (first (rest (first an-il)))]
    [else (DL-extracter a-string (rest an-il))]))

;; Tests:
(check-expect (search 'both "abandon" "book" (list (list "abandon" (list "a.txt" "b.txt" "c.txt"))
                                           (list "book" (list "a.txt"))
                                           (list "car" (list "a.txt" "b.txt"))
                                           (list "dog" (list "a.txt" "c.txt"))
                                           (list "echo" (list "a.txt" "b.txt"))
                                           (list "foxtrot" (list "a.txt"))
                                           (list "golf" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt"))
(check-expect (search 'both "abandon" "golf" (list (list "abandon" (list "a.txt" "b.txt" "c.txt"))
                                           (list "book" (list "a.txt"))
                                           (list "car" (list "a.txt" "b.txt"))
                                           (list "dog" (list "a.txt" "c.txt"))
                                           (list "echo" (list "a.txt" "b.txt"))
                                           (list "foxtrot" (list "a.txt"))
                                           (list "golf" (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt" "b.txt" "c.txt"))
(check-expect (search 'both "car" "monkey" (list (list "abandon" (list "a.txt" "b.txt" "c.txt"))
                                           (list "book" (list "a.txt"))
                                           (list "car" (list "a.txt" "b.txt"))
                                           (list "dog" (list "a.txt" "c.txt"))
                                           (list "echo" (list "a.txt" "b.txt"))
                                           (list "foxtrot" (list "a.txt"))
                                           (list "golf" (list "a.txt" "b.txt" "c.txt"))
                                           (list "monkey" (list "c.txt"))))
              (list ))
(check-expect (search 'exclude "abandon" "golf" (list (list "abandon" (list "a.txt" "b.txt" "c.txt"))
                                           (list "book" (list "a.txt"))
                                           (list "car" (list "a.txt" "b.txt"))
                                           (list "dog" (list "a.txt" "c.txt"))
                                           (list "echo" (list "a.txt" "b.txt"))
                                           (list "foxtrot" (list "a.txt"))
                                           (list "golf" (list "a.txt" "b.txt" "c.txt"))))
              (list ))
(check-expect (search 'exclude "abandon" "book" (list (list "abandon" (list "a.txt" "b.txt" "c.txt"))
                                           (list "book" (list "a.txt"))
                                           (list "car" (list "a.txt" "b.txt"))
                                           (list "dog" (list "a.txt" "c.txt"))
                                           (list "echo" (list "a.txt" "b.txt"))
                                           (list "foxtrot" (list "a.txt"))
                                           (list "golf" (list "a.txt" "b.txt" "c.txt"))))
              (list "b.txt" "c.txt"))
(check-expect (search 'both "abandon" "book" (list (list "abandon" (list ))
                                           (list "book" (list ))
                                           (list "car" (list ))
                                           (list "dog" (list ))
                                           (list "echo" (list ))
                                           (list "foxtrot" (list ))
                                           (list "golf" (list ))))
              (list ))
(check-expect (search 'exclude "abandon" "book" (list (list "abandon" (list ))
                                           (list "book" (list ))
                                           (list "car" (list ))
                                           (list "dog" (list ))
                                           (list "echo" (list ))
                                           (list "foxtrot" (list ))
                                           (list "golf" (list ))))
              (list ))
