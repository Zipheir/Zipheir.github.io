;;; $Id: csv.sls,v 1.35 2025/12/24 18:57:52 wcm Exp wcm $
;;;
;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;;
;;; SPDX-License-Identifier: EUPL-1.2-or-later
;;;
;;; Copyright © 2025 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; Licensed under the terms of the EUPL version 1.2 or later.
;;;
;;; Thanks to John Woldemar Cowan for review & suggestions.
;;;
;;; Thanks to Yuval Langer for proofreading.
;;;
;;;
;;; Small, fairly strict CSV parser.
;;;
;;; Basic principle of design: Spend lines of code on intelligent
;;; error-handling & feedback, not on weird CSV variants.
;;;
;;; Notes & gotchas:
;;;
;;; This parser follows the RFC 4180-bis draft in requiring every
;;; record (including the last) to be terminated by a CRLF or LF.
;;;
;;; The parser skips empty lines instead of treating them as records
;;; with a single, empty field.

(library (csv)
  (export make-csv-parser
	  parse-csv
	  parser-error?
	  condition-line-position
	  condition-char-position
          condition-input-char
	  )
  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
	  (rnrs exceptions)
          (rnrs io ports))

  ;; This definition may need tweaking.
  ;; Should the position information be part of an &irritants
  ;; condition?  Should there be fields for expected & actual
  ;; inputs?
  (define-condition-type &parser &i/o-port
    make-parser-error
    parser-error?
    (line-position condition-line-position)
    (char-position condition-char-position)
    (input-char    condition-input-char))

  (define (make-csv-parser csv-port)
    ;; State variables: the parser's current input line & character.
    (define line-position 1)
    (define char-position 0)
    ;; Shorthand
    (define (peek) (lookahead-char csv-port))

    (define parser-error
      (case-lambda
        ((message line-pos char-pos)
         (parser-error message line-pos char-pos #f))
        ((message line-pos char-pos input-char)
         (raise-continuable
          (condition (make-parser-error csv-port
                                        line-pos
                                        char-pos
                                        input-char)
	             (make-message-condition message))))))

    (define (parser-error/current-position message . optionals)
      (apply parser-error
             message
             line-position
             char-position
             optionals))

    (define (reset-char-position!)
      (set! char-position 1))

    (define (consume c)
      (let ((in (get-char csv-port)))
        (set! char-position (+ char-position 1))
        (unless (eqv? c in)
          (parser-error/current-position "Unexpected input" in))))

    ;; Consume a line-ending [CR]LF.
    (define (consume-terminator)
      (when (eqv? #\return (peek))
        (consume #\return))
      (consume #\newline)
      (set! line-position (+ line-position 1))
      (reset-char-position!))

    (define (lex-csv-record)
      (define (lex-iter fields)
        (let ((look (peek)))
          (case look
            ((#\return #\newline)
             (consume-terminator)
             (reverse fields))  ; end of record
            ((#\,)  ; new field begins
             (consume #\,)
             ;; Examine first char of next field to determine whether
             ;; it's an escaped field.
             (case (peek)
               ((#\")  ; opening quote of escaped field
                (lex-iter (cons (lex-escaped-field) fields)))
               (else   ; ordinary field
                (lex-iter (cons (lex-field) fields)))))
            (else
             (parser-error/current-position "Unexpected input" look)))))

      (lex-iter (list (lex-first-field))))

    ;; Lex the first field of a record.
    (define (lex-first-field)
      (case (peek)
        ((#\,) "")  ; empty initial field
        ((#\") (lex-escaped-field))
        (else (lex-field))))

    ;; Lex an ordinary (unescaped) field.
    (define (lex-field)
      (define (accum-loop rev-chars)
        (let ((look (peek)))
          (when (eof-object? look)
            (parser-error/current-position
             "unterminated field (reached EOF)"))
          (case look
            ((#\, #\newline)
             (list->string (reverse rev-chars)))
            ((#\return)
             ;; Check that this is the first half of a CRLF.
             (consume #\return)
             (if (eqv? #\newline (peek))
                 (list->string (reverse rev-chars))
                 (parser-error/current-position "invalid field character"
                                                #\return)))
            ((#\")
             (parser-error/current-position "invalid field character"
                                            look))
            (else
             (consume look)
             (accum-loop (cons look rev-chars))))))

      (accum-loop '()))

    ;; Lex an escaped (double-quoted) field.
    (define (lex-escaped-field)
      ;; Record the position of the field's opening quote, for
      ;; help diagnosing unterminated-field errors.
      (define opening-line-position line-position)
      (define opening-char-position char-position)

      (define (accum-loop rev-chars)
        (let ((look (peek)))
          (when (eof-object? look)
            (parser-error "unterminated record (reached EOF)"
                          opening-line-position
                          opening-char-position))
          (case look
            ((#\")  ; escape or end of field
             (consume #\")
             (cond ((check-escaped)  ; escaped double quote?
                    (consume #\")
                    (accum-loop (cons #\" rev-chars)))
                   (else  ; end of field
                    (list->string (reverse rev-chars)))))
            (else
             (consume look)
             (accum-loop (cons look rev-chars))))))

      (consume #\")  ; opening quote
      (accum-loop '()))

    ;; Look for the second half of an escaped double quote on csv-port
    ;; & return #t if it is found.  Return #f if the next char signals,
    ;; instead, the beginning of a new field or the end of the record.
    (define (check-escaped)
      (let ((look (peek)))
        (when (eof-object? look)
          (parser-error/current-position
           "unterminated record (reached EOF)"))
        (case look
          ((#\") #t)
          ((#\, #\newline) #f)  ; new field begins
          ((#\return) ; check that this is the first half of a CRLF.
           (consume #\return)
           (if (eqv? #\newline (peek))
               #f     ; end of record
               (parser-error/current-position
                "invalid field character"
                #\return)))
          (else
           (parser-error "invalid escaped character"
                         line-position
                         (+ char-position 1) ; pos after dquote
                         look)))))

    ;;; Parser entry point

    (assert (textual-port? csv-port))
    (assert (input-port? csv-port))
    (lambda (record-handler . inits)
      (define (record-loop seeds)
        (let ((look (peek)))
          (cond ((eof-object? look) (apply values seeds)) ; done
                ((eqv? look #\return)  ; empty line
                 (consume-terminator)
                 (record-loop seeds))
                (else
                 (let-values (((continue . seeds*)
                               (apply record-handler
                                      (lex-csv-record)
                                      seeds)))
                   (if continue
                       (record-loop seeds*)
                       (apply values seeds*)))))))  ; early exit

      (record-loop inits)))

  ;; Parser driver.
  (define (parse-csv parser record-handler . inits)
    (assert (procedure? parser))
    (assert (procedure? record-handler))
    (apply parser record-handler inits))

  )
