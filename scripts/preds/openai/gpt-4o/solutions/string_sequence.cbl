       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-SEQUENCE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 NUMERIC-STRING PIC X(100).
       01 INDEX          PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC X(100).

      * Return a string containing space-delimited numbers starting from 0 upto n inclusive.
      * >>> string_sequence(0)
      * '0'
      * >>> string_sequence(5)
      * '0 1 2 3 4 5'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING INDEX FROM 0 BY 1 UNTIL INDEX > L-N
               STRING 
                   INDEX WITH DELIMITED BY SPACE
                   INTO NUMERIC-STRING
               END-STRING
               IF INDEX < L-N
                   STRING 
                       SPACE DELIMITED BY SIZE
                       INTO NUMERIC-STRING
                   END-STRING
               END-IF
           END-PERFORM

           MOVE NUMERIC-STRING TO RESULT.

           GOBACK.
           
       END PROGRAM STRING-SEQUENCE.
