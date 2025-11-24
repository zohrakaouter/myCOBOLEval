       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-UPPER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-VOWELS PIC X(10) VALUE "AEIOU".
       01 INDEX          PIC 9(03) VALUE 0.
       01 STRING-LENGTH  PIC 9(03) VALUE 0.
       01 CHAR-AT        PIC X.
       01 I              PIC 9(03) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 RESULT PIC S9(10).

      * 
      * Given a string s, count the number of uppercase vowels in even indices.
      * 
      * For example:
      * count_upper('aBCdEf') returns 1
      * count_upper('abcdefg') returns 0
      * count_upper('dBBE') returns 0
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
       BEGIN.
           MOVE ZERO TO RESULT.
           MOVE FUNCTION LENGTH( L-S ) TO STRING-LENGTH.
           
           PERFORM TEST VARYING I FROM 1 BY 1 UNTIL I > STRING-LENGTH
               IF FUNCTION MOD(I, 2) = 1
               THEN
                   COMPUTE INDEX = I
                   MOVE L-S (INDEX:1) TO CHAR-AT
                   PERFORM CHECK-UPPER-VOWEL
               END-IF
           END-PERFORM.

       END-PROCEDURE.

       CHECK-UPPER-VOWEL.
           IF CHAR-AT = WS-VOWELS (1:1) OR
              CHAR-AT = WS-VOWELS (2:1) OR
              CHAR-AT = WS-VOWELS (3:1) OR
              CHAR-AT = WS-VOWELS (4:1) OR
              CHAR-AT = WS-VOWELS (5:1)
           THEN
               ADD 1 TO RESULT
           END-IF.

       GOBACK.
       END PROGRAM COUNT-UPPER.
