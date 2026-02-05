;;;; getting-started.lisp
;;;; Copy-paste examples to try cl-test-hardening

;;; Installation: (ql:quickload "cl-test-hardening/all")

;;; PROPERTY-BASED TESTING
(use-package :th.property)
(use-package :th.gen)

(defproperty reverse-twice-is-identity (list)
  (equal list (reverse (reverse list))))

(check-property 'reverse-twice-is-identity
                :generator (gen:lists (gen:integers))
                :num-tests 100)

;;; See docs/tutorials/property-based-testing.md for more examples
