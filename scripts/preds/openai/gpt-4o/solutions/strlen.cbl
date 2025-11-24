       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRLEN.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-TRIMMED-STRING PIC X(100).
       01 WS-LENGTH          PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 RESULT PIC S9(10).

      * Return length of given string. Ignore leading and trailing spaces.
      * >>> strlen('')
      * 0
      * >>> strlen('abc')
      * 3
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE FUNCTION TRIM(L-STRING) TO WS-TRIMMED-STRING
           COMPUTE WS-LENGTH = FUNCTION LENGTH(WS-TRIMMED-STRING)
           MOVE WS-LENGTH TO RESULT
           GOBACK.
       END PROGRAM STRLEN.
