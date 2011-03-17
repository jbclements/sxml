#lang racket

(require "../ssax.rkt"
         rackunit)

(define (test str namespace-assig expected-res)
  (let ((result
         (call-with-input-string str
                                 (lambda (port)
                                   (ssax:xml->sxml port namespace-assig)))))
    (check-equal? result expected-res)))

(test " <BR/>" '() '(*TOP* (BR)))
(test "<BR></BR>" '() '(*TOP* (BR)))
(test " <BR CLEAR='ALL'\nCLASS='Class1'/>" '()
      '(*TOP* (BR (@ (CLEAR "ALL") (CLASS "Class1")))))
(test "   <A HREF='URL'>  link <I>itlink </I> &amp;amp;</A>" '()
      '(*TOP* (A (@ (HREF "URL")) "  link " (I "itlink ") " &amp;")))
(test "   <A HREF='URL' xml:space='preserve'>  link <I>itlink </I> &amp;amp;</A>" '()
      '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
                 "  link " (I "itlink ") " &amp;")))
(test "   <A HREF='URL' xml:space='preserve'>  link <I xml:space='default'>itlink </I> &amp;amp;</A>" '()
      '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
                 "  link " (I (@ (xml:space "default"))
                              "itlink ") " &amp;")))
(test " <P><?pi1  p1 content ?>?<?pi2 pi2? content? ??></P>" '()
      '(*TOP* (P (*PI* pi1 "p1 content ") "?"
                 (*PI* pi2 "pi2? content? ?"))))
(test " <P>some text <![CDATA[<]]>1\n&quot;<B>strong</B>&quot;\r</P>"
      '()
      `(*TOP* (P "some text <1\n\""
                 (B "strong") "\"\n")))
(test " <P><![CDATA[<BR>\n<![CDATA[<BR>]]&gt;]]></P>" '()
      `(*TOP* (P "<BR>\n<![CDATA[<BR>]]>")))
;    (test "<T1><T2>it&apos;s\r\nand   that\n</T2>\r\n\r\n\n</T1>" '()
;	  '(*TOP* (T1 (T2 "it's\nand   that\n") "\n\n\n")))
(test "<T1><T2>it&apos;s\r\nand   that\n</T2>\r\n\r\n\n</T1>" '()
      `(*TOP* (T1 (T2 "it's\nand   that\n"))))
(test "<T1><T2>it&apos;s\rand   that\n</T2>\r\n\r\n\n</T1>" '()
      `(*TOP* (T1 (T2 "it's\nand   that\n"))))
(test "<!DOCTYPE T SYSTEM 'system1' ><!-- comment -->\n<T/>" '()
      '(*TOP* (T)))
(test "<?xml version='1.0'?>\n<WEIGHT unit=\"pound\">\n<NET certified='certified'> 67 </NET>\n<GROSS> 95 </GROSS>\n</WEIGHT>" '()
      '(*TOP* (*PI* xml "version='1.0'") (WEIGHT (@ (unit "pound"))
                                                 (NET (@ (certified "certified")) " 67 ")
                                                 (GROSS " 95 "))
              ))
;     (test "<?xml version='1.0'?>\n<WEIGHT unit=\"pound\">\n<NET certified='certified'> 67 </NET>\n<GROSS> 95 </GROSS>\n</WEIGHT>" '()
; 	  '(*TOP* (*PI* xml "version='1.0'") (WEIGHT (@ (unit "pound"))
;                "\n" (NET (@ (certified "certified")) " 67 ")
;                "\n" (GROSS " 95 ") "\n")
; 		  ))
(test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>" '()
      '(*TOP* (URI1:DIV (@ (URI1:B "A") (B "B")) (URI1:P (BR)))))
(test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>" '((UA . "URI1"))
      '(*TOP* (@ (*NAMESPACES* (UA "URI1")))
              (UA:DIV (@ (UA:B "A") (B "B")) (UA:P (BR)))))

; A few tests from XML Namespaces Recommendation
(test (string-append
       "<x xmlns:edi='http://ecommerce.org/schema'>"
       "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
       "<lineItem edi:taxClass='exempt'>Baby food</lineItem>" nl
       "</x>") '()
               '(*TOP* 
                 (x (lineItem
                     (@ (http://ecommerce.org/schema:taxClass "exempt"))
                     "Baby food"))))
(test (string-append 
       "<x xmlns:edi='http://ecommerce.org/schema'>"
       "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
       "<lineItem edi:taxClass='exempt'>Baby food</lineItem>"
       "</x>") '((EDI . "http://ecommerce.org/schema"))
               '(*TOP*
                 (@ (*NAMESPACES* (EDI "http://ecommerce.org/schema")))
                 (x (lineItem
                     (@ (EDI:taxClass "exempt"))
                     "Baby food"))))

(test (string-append
       "<bk:book xmlns:bk='urn:loc.gov:books' "
       "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
       "<bk:title>Cheaper by the Dozen</bk:title>"
       "<isbn:number>1568491379</isbn:number></bk:book>")
      '()
      '(*TOP* (urn:loc.gov:books:book
               (urn:loc.gov:books:title "Cheaper by the Dozen")
               (urn:ISBN:0-395-36341-6:number "1568491379"))))

(test (string-append
       "<!-- initially, the default namespace is 'books' -->"
       "<book xmlns='urn:loc.gov:books' "
       "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
       "<title>Cheaper by the Dozen</title>"
       "<isbn:number>1568491379</isbn:number>"
       "<notes>"
       "<!-- make HTML the default namespace for some commentary -->"
       "<p xmlns='urn:w3-org-ns:HTML'>"
       "This is a <i>funny</i> book!"
       "</p>"
       "</notes>"
       "</book>") '()
                  '(*TOP* (urn:loc.gov:books:book
                           (urn:loc.gov:books:title "Cheaper by the Dozen")
                           (urn:ISBN:0-395-36341-6:number "1568491379")
                           (urn:loc.gov:books:notes
                            (urn:w3-org-ns:HTML:p 
                             "This is a " (urn:w3-org-ns:HTML:i "funny")
                             " book!")))))

(test (string-append
       "<Beers>"
       "<!-- the default namespace is now that of HTML -->"
       "<table xmlns='http://www.w3.org/TR/REC-html40'>"
       "<th><td>Name</td><td>Origin</td><td>Description</td></th>"
       "<tr>"
       "<!-- no default namespace inside table cells -->"
       "<td><brandName xmlns=\"\">Huntsman</brandName></td>"
       "<td><origin xmlns=''>Bath, UK</origin></td>"
       "<td>"
       "<details xmlns=''><class>Bitter</class><hop>Fuggles</hop>"
       "<pro>Wonderful hop, light alcohol, good summer beer</pro>"
       "<con>Fragile; excessive variance pub to pub</con>"
       "</details>"
       "</td>"
       "</tr>"
       "</table>"
       "</Beers>")
      '((html . "http://www.w3.org/TR/REC-html40"))
      '(*TOP*
        (@ (*NAMESPACES* (html "http://www.w3.org/TR/REC-html40")))
        (Beers (html:table
                (html:th (html:td "Name")
                         (html:td "Origin")
                         (html:td "Description"))
                (html:tr (html:td (brandName "Huntsman"))
                         (html:td (origin "Bath, UK"))
                         (html:td 
                          (details 
                           (class "Bitter")
                           (hop "Fuggles")
                           (pro "Wonderful hop, light alcohol, good summer beer")
                           (con "Fragile; excessive variance pub to pub"))))))))

(test (string-append
       "<!-- 1 --><RESERVATION xmlns:HTML='http://www.w3.org/TR/REC-html40'>"
       "<!-- 2 --><NAME HTML:CLASS=\"largeSansSerif\">Layman, A</NAME>"
       "<!-- 3 --><SEAT CLASS='Y' HTML:CLASS=\"largeMonotype\">33B</SEAT>"
       "<!-- 4 --><HTML:A HREF='/cgi-bin/ResStatus'>Check Status</HTML:A>"
       "<!-- 5 --><DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>")
      '((HTML . "http://www.w3.org/TR/REC-html40"))
      '(*TOP*
        (@ (*NAMESPACES* (HTML "http://www.w3.org/TR/REC-html40")))
        (RESERVATION
         (NAME (@ (HTML:CLASS "largeSansSerif")) "Layman, A")
         (SEAT (@ (HTML:CLASS "largeMonotype") (CLASS "Y")) "33B")
         (HTML:A (@ (HREF "/cgi-bin/ResStatus")) "Check Status")
         (DEPARTURE "1997-05-24T07:55:00+1"))))
; Part of RDF from the XML Infoset
(test (apply 
       string-append (list-intersperse '(
                                         "<?xml version='1.0' encoding='utf-8' standalone='yes'?>"
                                         "<!-- this can be decoded as US-ASCII or iso-8859-1 as well,"
                                         "  since it contains no characters outside the US-ASCII repertoire -->"
                                         "<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'"
                                         "         xmlns:rdfs='http://www.w3.org/2000/01/rdf-schema#'"
                                         "          xmlns='http://www.w3.org/2001/02/infoset#'>"
                                         "<rdfs:Class ID='Boolean'/>"
                                         "<Boolean ID='Boolean.true'/>"
                                         "<Boolean ID='Boolean.false'/>"
                                         "<!--Info item classes-->"
                                         "<rdfs:Class ID='InfoItem'/>"
                                         "<rdfs:Class ID='Document' rdfs:subClassOf='#InfoItem'/>"
                                         "<rdfs:Class ID='Element' rdfs:subClassOf='#InfoItem'/>"
                                         "<rdfs:Class ID='Attribute' rdfs:subClassOf='#InfoItem'/>"
                                         "<rdfs:Class ID='InfoItemSet'
      rdfs:subClassOf='http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag'/>"
                                         "<rdfs:Class ID='AttributeSet' rdfs:subClassOf='#InfoItemSet'/>"
                                         "<!--Info item properties-->"
                                         "<rdfs:Property ID='allDeclarationsProcessed'>"
                                         "<rdfs:domain resource='#Document'/>"
                                         "<rdfs:range resource='#Boolean'/></rdfs:Property>"
                                         "<rdfs:Property ID='attributes'>"
                                         "<rdfs:domain resource='#Element'/>"
                                         "<rdfs:range resource='#AttributeSet'/>"
                                         "</rdfs:Property>"
                                         "</rdf:RDF>")
                                       (string #\newline)))
      '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        (RDFS . "http://www.w3.org/2000/01/rdf-schema#")
        (ISET . "http://www.w3.org/2001/02/infoset#"))
      '(*TOP* (@ (*NAMESPACES*
                  (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                  (RDFS "http://www.w3.org/2000/01/rdf-schema#")
                  (ISET "http://www.w3.org/2001/02/infoset#")))
              (*PI* xml "version='1.0' encoding='utf-8' standalone='yes'")
              (RDF:RDF
               (RDFS:Class (@ (ID "Boolean")))
               (ISET:Boolean (@ (ID "Boolean.true")))
               (ISET:Boolean (@ (ID "Boolean.false")))
               (RDFS:Class (@ (ID "InfoItem")))
               (RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Document")))
               (RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Element")))
               (RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Attribute")))
               (RDFS:Class
                (@ (RDFS:subClassOf
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")
                   (ID "InfoItemSet")))
               (RDFS:Class
                (@ (RDFS:subClassOf "#InfoItemSet") (ID "AttributeSet")))
               (RDFS:Property
                (@ (ID "allDeclarationsProcessed"))
                (RDFS:domain (@ (resource "#Document")))
                (RDFS:range (@ (resource "#Boolean"))))
               (RDFS:Property
                (@ (ID "attributes"))
                (RDFS:domain (@ (resource "#Element")))
                (RDFS:range (@ (resource "#AttributeSet")))))))

; Part of RDF from RSS of the Daemon News Mall
(test (apply
       string-append
       (list-intersperse '(
                           "<?xml version='1.0'?><rdf:RDF "
                           "xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#' "
                           "xmlns='http://my.netscape.com/rdf/simple/0.9/'>"
                           "<channel>"
                           "<title>Daemon News Mall</title>"
                           "<link>http://mall.daemonnews.org/</link>"
                           "<description>Central source for all your BSD needs</description>"
                           "</channel>"
                           "<item>"
                           "<title>Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95</title>"
                           "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=880</link>"
                           "</item>"
                           "<item>"
                           "<title>The Design and Implementation of the 4.4BSD Operating System $54.95</title>"
                           "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=912&amp;category_id=1761</link>"
                           "</item>"
                           "</rdf:RDF>")
                         (string #\newline)
                         ))
      '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        (RSS . "http://my.netscape.com/rdf/simple/0.9/")
        (ISET . "http://www.w3.org/2001/02/infoset#"))
      '(*TOP* (@ (*NAMESPACES*
                  (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                  (RSS "http://my.netscape.com/rdf/simple/0.9/")
                  (ISET "http://www.w3.org/2001/02/infoset#")))
              (*PI* xml "version='1.0'")
              (RDF:RDF (RSS:channel
                        (RSS:title "Daemon News Mall")
                        (RSS:link "http://mall.daemonnews.org/")
                        (RSS:description "Central source for all your BSD needs"))
                       (RSS:item
                        (RSS:title
                         "Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95")
                        (RSS:link
                         "http://mall.daemonnews.org/?page=shop/flypage&product_id=880"))
                       (RSS:item
                        (RSS:title
                         "The Design and Implementation of the 4.4BSD Operating System $54.95")
                        (RSS:link
                         "http://mall.daemonnews.org/?page=shop/flypage&product_id=912&category_id=1761")))))

(test (apply
       string-append
       (list-intersperse 
        '("<Forecasts TStamp='958082142'>"
          "<TAF TStamp='958066200' LatLon='36.583, -121.850' BId='724915'"
          "  SName='KMRY, MONTEREY PENINSULA'>"
          "<VALID TRange='958068000, 958154400'>111730Z 111818</VALID>"
          "<PERIOD TRange='958068000, 958078800'>"
          "<PREVAILING>31010KT P6SM FEW030</PREVAILING>"
          "</PERIOD>"
          "<PERIOD TRange='958078800, 958104000' Title='FM2100'>"
          "<PREVAILING>29016KT P6SM FEW040</PREVAILING>"
          "</PERIOD>"
          "<PERIOD TRange='958104000, 958154400' Title='FM0400'>"
          "<PREVAILING>29010KT P6SM SCT200</PREVAILING>"
          "<VAR Title='BECMG 0708' TRange='958114800, 958118400'>VRB05KT</VAR>"
          "</PERIOD></TAF>"
          "</Forecasts>")
        (string #\newline)
        ))
      '()
      '(*TOP* (Forecasts
               (@ (TStamp "958082142"))
               (TAF (@ (TStamp "958066200")
                       (SName "KMRY, MONTEREY PENINSULA")
                       (LatLon "36.583, -121.850")
                       (BId "724915"))
                    (VALID (@ (TRange "958068000, 958154400")) "111730Z 111818")
                    (PERIOD (@ (TRange "958068000, 958078800"))
                            (PREVAILING "31010KT P6SM FEW030"))
                    (PERIOD (@ (Title "FM2100") (TRange "958078800, 958104000"))
                            (PREVAILING "29016KT P6SM FEW040"))
                    (PERIOD (@ (Title "FM0400") (TRange "958104000, 958154400"))
                            (PREVAILING "29010KT P6SM SCT200")
                            (VAR (@ (Title "BECMG 0708")
                                    (TRange "958114800, 958118400"))
                                 "VRB05KT"))))))
  