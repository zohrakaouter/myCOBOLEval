       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENCRYPT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01  WS-COUNTERS.
           05  WS-I PIC 9(3).
           05  WS-LEN PIC 9(3).
       01  WS-CHAR PIC X.
       01  WS-NUM PIC 9(3).
       01  WS-NEW-NUM PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 RESULT PIC X(100).

      * Create a function encrypt that takes a string as an argument and
      * returns a string encrypted with the alphabet being rotated.
      * The alphabet should be rotated in a manner such that the letters
      * shift down by two multiplied to two places.
      * For example:
      * encrypt('hi') returns 'lm'
      * encrypt('asdfghjkl') returns 'ewhjklnop'
      * encrypt('gf') returns 'kj'
      * encrypt('et') returns 'ix'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE 0 TO WS-I
           INSPECT L-S TALLYING WS-LEN FOR CHARACTERS

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE L-S(WS-I:1) TO WS-CHAR

               IF WS-CHAR IS ALPHABETIC
                   IF WS-CHAR >= 'A' AND WS-CHAR <= 'Z'
                       COMPUTE WS-NUM = FUNCTION ORD(WS-CHAR) - 64
                       COMPUTE WS-NEW-NUM = FUNCTION MOD(WS-NUM + 4, 26)
                       IF WS-NEW-NUM = 0
                           MOVE 'Z' TO RESULT(WS-I:1)
                       ELSE
                           COMPUTE WS-NEW-NUM = WS-NEW-NUM + 64
                           MOVE FUNCTION CHAR(WS-NEW-NUM) TO RESULT(WS-I:1)
                       END-IF
                   ELSE
                       COMPUTE WS-NUM = FUNCTION ORD(WS-CHAR) - 96
                       COMPUTE WS-NEW-NUM = FUNCTION MOD(WS-NUM + 4, 26)
                       IF WS-NEW-NUM = 0
                           MOVE 'z' TO RESULT(WS-I:1)
                       ELSE
                           COMPUTE WS-NEW-NUM = WS-NEW-NUM + 96
                           MOVE FUNCTION CHAR(WS-NEW-NUM) TO RESULT(WS-I:1)
                       END-IF
                   END-IF
               ELSE
                   MOVE WS-CHAR TO RESULT(WS-I:1)
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM ENCRYPT.
