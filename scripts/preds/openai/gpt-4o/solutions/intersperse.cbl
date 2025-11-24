       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTERSPERSE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 N-COUNT        PIC S9(10) VALUE 3.
       77 INDEX          PIC S9(10) VALUE 1.
       77 RESULT-INDEX   PIC S9(10) VALUE 1.
       77 TEMP-NUMBER    PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 L-DELIMETER PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * Insert a number 'delimeter' between every two consecutive elements of input list `numbers'
      * >>> intersperse([2, 2, 2], 4)
      * [2, 4, 2, 4, 2]
      * >>> intersperse([1, 2, 3], 4)
      * [1, 4, 2, 4, 3]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
      * Initialize Result array to zeros.
           PERFORM VARYING NJ FROM 1 BY 1
                   UNTIL NJ > 100
               MOVE 0 TO RESULT(NJ)
           END-PERFORM.

      * Begin intersperse operation
           SET NI TO 1
           PERFORM UNTIL NI > N-COUNT
               MOVE L-NUMBERS(NI) TO RESULT(RESULT-INDEX)
               ADD 1 TO RESULT-INDEX
               
               IF NI < N-COUNT
                   MOVE L-DELIMETER TO RESULT(RESULT-INDEX)
                   ADD 1 TO RESULT-INDEX
               END-IF
               
               ADD 1 TO NI
           END-PERFORM.
           
      * Display the Result array for verification.
           PERFORM VARYING NJ FROM 1 BY 1
                   UNTIL NJ > RESULT-INDEX
               DISPLAY RESULT(NJ)
           END-PERFORM.

       GOBACK.
       END PROGRAM INTERSPERSE.
