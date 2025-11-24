       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENCRYPT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-ALPHABET PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
       01 WS-ROTATED-ALPHABET PIC X(26).

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

       DECLARE ENCRYPT-EXIT.
       PERFORM INITIALIZE

       PERFORM ENCODE
            VARYING IDX FROM 1 BY 1
            UNTIL IDX > LENGTH OF L-S OR L-S (IDX:1) = SPACE

       MOVE RESULT TO L-S
       . 
       GOBACK.

       INITIALIZE.
           PERFORM VARYING IDX FROM 1 BY 1
                    UNTIL IDX > 26
               COMPUTE ROTATED-IDX = MOD(IDX + 8, 26)
               IF ROTATED-IDX = 0
                   PERFORM COMPUTE-FIRST-ALPHABET
               ELSE
                   MOVE WS-ALPHABET(IDX:1) TO 
                    WS-ROTATED-ALPHABET(ROTATED-IDX:1)
               END-IF
           END-PERFORM
       .

       ENCODE.
           IF L-S(IDX:1) SPACE OR L-S(IDX:1) = LOW-VALUE THEN
              EXIT PERFORM.
           ELSE
              COMPUTE CHAR-POS = FUNCTION REVERSE (
                   FUNCTION INDEX (WS-ALPHABET, L-S(IDX:1)))
              MOVE WS-ROTATED-ALPHABET(CHAR-POS:1) TO RESULT(IDX:1)
           END-IF
       .

       END PROGRAM ENCRYPT.
