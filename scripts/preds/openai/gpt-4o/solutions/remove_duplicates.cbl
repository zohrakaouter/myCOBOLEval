       IDENTIFICATION DIVISION.
       PROGRAM-ID. REMOVE-DUPLICATES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS.
           05 I                  PIC 9(02) VALUE 0.
           05 J                  PIC 9(02) VALUE 0.
           05 K                  PIC 9(02) VALUE 0.
           05 UNIQUE-FLAG        PIC X VALUE 'N'.
           05 COUNT_ARRAY        OCCURS 100 TIMES PIC 9(03) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * From a list of integers, remove all elements that occur more than once.
      * Keep order of elements left the same as in the input.
      * >>> remove_duplicates([1, 2, 3, 2, 4])
      * [1, 3, 4]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PARA.

           PERFORM POST-DUPLICATES
           
           DISPLAY "RESULT: "
           PERFORM VARYING K FROM 1 BY 1 UNTIL NJ(K) > 0
               DISPLAY RESULT(K)
           END-PERFORM
           
           GOBACK.
        
       POST-DUPLICATES.

           PERFORM VARYING I FROM 1 BY 1 UNTIL NI(I) > 0
               ADD 1 TO COUNT_ARRAY(L-NUMBERS(I))
           END-PERFORM
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL NI(I) > 0
               IF COUNT_ARRAY(L-NUMBERS(I)) = 1
                   ADD 1 TO J
                   MOVE L-NUMBERS(I) TO RESULT(J)
               END-IF
           END-PERFORM.

       END PROGRAM REMOVE-DUPLICATES.
