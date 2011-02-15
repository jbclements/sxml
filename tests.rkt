#lang scheme

(require "serializer.ss"
         (planet schematics/schemeunit:3))


(check-equal? (srl:sxml->xml `(*TOP* (p))) "<p />")

