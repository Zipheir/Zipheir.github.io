;;; $Id: csv.sld,v 1.2 2026/08/18 14:18:13 wcm Exp wcm $
;;;
;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;;
;;; SPDX-License-Identifier: EUPL-1.2-or-later
;;;
;;; Copyright © 2025--2026 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; Licensed under the terms of the EUPL version 1.2 or later.
;;;
;;; Thanks to John Woldemar Cowan for review & suggestions.
;;;
;;; Thanks to Yuval Langer for proofreading.
;;;
;;;
;;; Small, fairly strict CSV parser.  (R7RS version)
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
;;;
;;; Error recovery resumes parsing at the next record, & the mechanism
;;; for skipping the erroneous record is simple: the next CRLF or LF
;;; ends the record, regardless of whether the parser was in a quoted
;;; field when the error occurred.

(define-library (csv)
  (export make-csv-parser
          parse-csv
          parser-error?
          recoverable-parser-error?
          condition-line-position
          condition-char-position
          condition-input-char
          condition-skip-record
          )
  (import (scheme base)
          (scheme case-lambda)
          (prefix (srfi 35) s35:)
          )
  (begin

    ;;; R6RS shims

    (define-syntax assert
      (syntax-rules ()
        ((assert expr)
         (unless expr
           (error "assertion violation" 'expr)))))

    (define (make-message-condition msg)
      (s35:make-condition s35:&message 'message msg))

    ;;; Condition types

    ;; This definition may need tweaking.
    ;; Should the position information be part of an &irritants
    ;; condition?  Should there be fields for expected & actual
    ;; inputs?
    (s35:define-condition-type &parser s35:&error
      parser-error?
      (port          condition-port)
      (line-position condition-line-position)
      (char-position condition-char-position)
      (input-char    condition-input-char))

    (define (make-parser-error port line-pos char-pos input-char)
      (s35:make-condition &parser
                          'port port
                          'line-position line-pos
                          'char-position char-pos
                          'input-char input-char))

    (s35:define-condition-type &parser-recoverable &parser
      recoverable-parser-error?)

    (define (make-recoverable-parser-error port
                                           line-pos
                                           char-pos
                                           input-char)
      (s35:make-condition &parser-recoverable
                          'port port
                          'line-position line-pos
                          'char-position char-pos
                          'input-char input-char))

    ;; This condition type is not an interface feature.  It is used
    ;; solely to attach a continuation to a recoverable error when
    ;; the appropriate recovery continuation isn't available at the
    ;; point at which the exception is triggered.
    (s35:define-condition-type &skip-record-recovery s35:&error
      skip-record-recovery-condition?
      (skip-record condition-skip-record))

    (define (make-skip-record-recovery thunk)
      (s35:make-condition &skip-record-recovery 'skip-record thunk))

    ;; Drop chars from *port* up to & including the next [CR]LF.
    ;; Problem: The parser may be in an escaped field when this is
    ;; called.
    (define (skip-record port)
      (define (skip-loop c)
        (cond ((eqv? c #\newline)
               (read-char port)) ; done
              (else
               (read-char port)
               (skip-loop (peek-char port)))))

      (skip-loop (peek-char port)))

    (define (make-csv-parser csv-port)
      ;; State variables: the parser's current input line & character.
      (define line-position 1)
      (define char-position 0)
      ;; Shorthand
      (define (peek) (peek-char csv-port))

      (define parser-error
        (case-lambda
          ((message line-pos char-pos)
           (parser-error message line-pos char-pos #f))
          ((message line-pos char-pos input-char)
           (raise-continuable
            (s35:make-compound-condition
             (make-parser-error csv-port
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

      (define recoverable-parser-error
        (case-lambda
          ((message line-pos char-pos)
           (recoverable-parser-error message line-pos char-pos #f))
          ((message line-pos char-pos input-char)
           (raise-continuable
            (s35:make-compound-condition
             (make-recoverable-parser-error csv-port
                                            line-pos
                                            char-pos
                                            input-char)
             (make-message-condition message))))))

      (define (recoverable-parser-error/current-position message
                . optionals)
        (apply recoverable-parser-error
               message
               line-position
               char-position
               optionals))

      ;; Calls *thunk*, guarding for recoverable parser errors.
      ;; If one is caught, then it is re-raised along with a recovery
      ;; condition which allows the *thunk* call to be retried in the
      ;; original parser continuation.
      (define (with-retry-guard thunk)
        (call-with-current-continuation
         (lambda (k)
           (guard (con
                   ((recoverable-parser-error? con)
                    (raise-continuable
                     (s35:make-compound-condition
                      con
                      (make-skip-record-recovery
                       (lambda ()
                         (skip-record csv-port)
                         (set! line-position (+ line-position 1))
                         (reset-char-position!)
                         (k (thunk)))))))
                   (else (raise-continuable con)))
             (thunk)))))

      (define (reset-char-position!)
        (set! char-position 1))

      (define (consume c)
        (let ((in (read-char csv-port)))
          (set! char-position (+ char-position 1))
          (unless (eqv? c in)
            (recoverable-parser-error/current-position
             "unexpected input"
             in))))

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
               (recoverable-parser-error/current-position
                "unexpected input"
                look)))))

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
                   (recoverable-parser-error/current-position
                    "invalid field character"
                    #\return)))
              ((#\")
               (recoverable-parser-error/current-position
                "invalid field character"
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

      ;; Look for the second half of an escaped double quote on
      ;; csv-port & return #t if it is found.  Return #f if the next
      ;; char signals, instead, the beginning of a new field or the end
      ;; of the record.
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
                 (recoverable-parser-error/current-position
                  "invalid field character"
                  #\return)))
            (else
             (recoverable-parser-error "invalid escaped character"
                                       line-position
                                       (+ char-position 1) ; after dquote
                                       look)))))

      ;;; Parser entry point

      (assert (textual-port? csv-port))
      (assert (input-port? csv-port))
      (lambda (record-handler . inits)
        (define (record-loop seeds)
          (with-retry-guard
           (lambda ()
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
                            (apply values seeds*)))))))))  ; early exit

        (record-loop inits)))

    ;; Parser driver.
    (define (parse-csv parser record-handler . inits)
      (assert (procedure? parser))
      (assert (procedure? record-handler))
      (apply parser record-handler inits))

  ))
