(in-package #:dwcalgorithms)

(5am:def-suite stack-suite :description "Stack Test Suite")
(5am:in-suite stack-suite)

(5am:test test-stack
  (let ((the-stack (make-instance 'stack)))
    (dwcalgorithms:push the-stack 500)
    (dwcalgorithms:push the-stack 'foo)
    (5am:is (eq 'foo (dwcalgorithms:pop the-stack)))
    (5am:is (= 500 (dwcalgorithms:pop the-stack)))
    (5am:is (null (peek the-stack)))
    (5am:is (null (dwcalgorithms:pop the-stack)))))

