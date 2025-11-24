       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENCODE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-MESSAGE-LENGTH         PIC 9(3).
       01 WS-CURRENT-CHAR           PIC X.
       01 WS-INDEX                  PIC 9(3) VALUE 1.
       01 WS-ENCODED-CHAR           PIC X.
       01 WS-VOWEL-CHECK            PIC X.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-MESSAGE PIC X(100).
           05 RESULT PIC X(100).

      * 
      * Write a function that takes a message, and encodes in such a
      * way that it swaps case of all letters, replaces all vowels in
      * the message with the letter that appears 2 places ahead of that
      * vowel in the english alphabet.
      * Assume only letters.
      * 
      * Examples:
      * >>> encode('test')
      * 'TGST'
      * >>> encode('This is a message')
      * 'tHKS KS C MGSSCGG'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCEDURE.
           MOVE FUNCTION LENGTH(L-MESSAGE) TO WS-MESSAGE-LENGTH
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-MESSAGE-LENGTH
               MOVE L-MESSAGE(WS-INDEX:1) TO WS-CURRENT-CHAR
               PERFORM ENCODE-CHARACTER
               MOVE WS-ENCODED-CHAR TO RESULT(WS-INDEX:1)
           END-PERFORM.

       ENCODE-CHARACTER.
           IF WS-CURRENT-CHAR IS ALPHABETIC
               IF WS-CURRENT-CHAR IS LOWER-CASE
                   IF WS-CURRENT-CHAR = 'a' OR WS-CURRENT-CHAR = 'e' OR
                      WS-CURRENT-CHAR = 'i' OR WS-CURRENT-CHAR = 'o' OR
                      WS-CURRENT-CHAR = 'u'
                       COMPUTE WS-ENCODED-CHAR = FUNCTION CHAR(NUMERIC-VALUE(WS-CURRENT-CHAR) + 2)
                   ELSE
                       MOVE FUNCTION UPPER-CASE(WS-CURRENT-CHAR) TO WS-ENCODED-CHAR
                   END-IF
               ELSE
                   IF WS-CURRENT-CHAR = 'A' OR WS-CURRENT-CHAR = 'E' OR
                      WS-CURRENT-CHAR = 'I' OR WS-CURRENT-CHAR = 'O' OR
                      WS-CURRENT-CHAR = 'U'
                       COMPUTE WS-ENCODED-CHAR = FUNCTION CHAR(NUMERIC-VALUE(WS-CURRENT-CHAR) + 2)
                   ELSE
                       MOVE FUNCTION LOWER-CASE(WS-CURRENT-CHAR) TO WS-ENCODED-CHAR
                   END-IF
               END-IF
           ELSE
               MOVE WS-CURRENT-CHAR TO WS-ENCODED-CHAR
           END-IF.

       GOBACK.
       END PROGRAM ENCODE.
