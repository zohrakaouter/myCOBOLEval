       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-UPPER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-COUNT PIC 9(3) VALUE 0.
       01 WS-VOWELS PIC X(5) VALUE 'AEIOU'.
       01 WS-CHAR PIC X.
       01 WS-VOWEL-FLAG PIC X VALUE 'N'.

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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > LENGTH OF L-S
               COMPUTE WS-I = WS-I - 1
               IF FUNCTION MOD(WS-I, 2) = 0
                   MOVE L-S(WS-I + 1:1) TO WS-CHAR
                   PERFORM VARYING WS-VOWEL-FLAG FROM 1 BY 1
                       UNTIL WS-VOWEL-FLAG > LENGTH OF WS-VOWELS
                           OR WS-CHAR = WS-VOWELS(WS-VOWEL-FLAG:1)
                   END-PERFORM
                   IF WS-CHAR = WS-VOWELS(WS-VOWEL-FLAG:1)
                       ADD 1 TO WS-COUNT
                   END-IF
               END-IF
               COMPUTE WS-I = WS-I + 1
           END-PERFORM

           MOVE WS-COUNT TO RESULT
           GOBACK.
       END PROGRAM COUNT-UPPER.
