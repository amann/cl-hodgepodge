;;;; -*- outline-regexp:";;;;[*]+ +" -*-

(oam::define-project-package #:ch.amann-wolowyk.concurrency-test #:aop-test)
(in-package #:ch.amann-wolowyk.concurrency-test-system)

;;;;** Test Event Channel
;;;;

(multiple-value-bind (fetch-events add-event)
    (make-event-channel)
  (oam:fbind ()))