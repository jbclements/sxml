(module info (lib "infotab.ss" "setup")
  (define name "sxml")
  (define blurb
    (list "Collection of tools for processing markup documents "
          "in the form of S-expressions"))
  (define primary-file "sxml.ss")
  (define doc.txt "doc.txt")
  (define homepage "http://modis.ispras.ru/Lizorkin/sxml-tutorial.html")
  (define categories '(xml))
  (define compile-omit-paths '("tests.ss"))
  (define repositories '("4.x"))
  (define release-notes (list "changed name of compile-omit-file to tests.ss"))
  )
