#lang scheme

(require "serializer.ss"
         rackunit)


(check-equal? (srl:sxml->xml `(*TOP* (p))) "<p />")

