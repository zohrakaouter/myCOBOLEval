       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-ARRAY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 ARRAYS.
           05 UNSORTED-ARRAY      PIC S9(10) OCCURS 5 TIMES.
           05 SORTED-ARRAY        PIC S9(10) OCCURS 5 TIMES.
           05 SORTED-BIN-ARRAY    PIC S9(10) OCCURS 5 TIMES.

       01 TEMP-VAR.
           05 TEMP-VALUE          PIC S9(10).

       01 TEMP-BIN-VAR.
           05 TEMP-BIN            PIC 9(10).

       01 COUNTS.
           05 ONE-COUNT           PIC 9(2) OCCURS 5 TIMES.
           05 TEMP-COUNT          PIC 9(2).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * 
      * In this Kata, you have to sort an array of non-negative integers according to
      * number of ones in their binary representation in ascending order.
      * For similar number of ones, sort based on decimal value.
      * 
      * It must be implemented like this:
      * >>> sort_array([1, 5, 2, 3, 4]) == [1, 2, 3, 4, 5]
      * >>> sort_array([-2, -3, -4, -5, -6]) == [-6, -5, -4, -3, -2]
      * >>> sort_array([1, 0, 2, 3, 4]) [0, 1, 2, 3, 4]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM INITIALIZE-ARRAYS
           PERFORM CALCULATE-ONES
           PERFORM SORT-ARRAYS
           PERFORM MOVE-SORTED-TO-RESULT
           GOBACK.
           
       INITIALIZE-ARRAYS.

           MOVE 1 TO UNSORTED-ARRAY(1)
           MOVE 5 TO UNSORTED-ARRAY(2)
           MOVE 2 TO UNSORTED-ARRAY(3)
           MOVE 3 TO UNSORTED-ARRAY(4)
           MOVE 4 TO UNSORTED-ARRAY(5).

       CALCULATE-ONES.

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               MOVE UNSORTED-ARRAY(NI) TO TEMP-VALUE
               MOVE FUNCTION NUM-OF-BITS (TEMP-VALUE) TO ONE-COUNT(NI)
           END-PERFORM.

       SORT-ARRAYS.

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               PERFORM VARYING NJ FROM 1 BY 1 UNTIL NJ > 5
                   IF ONE-COUNT(NI) < ONE-COUNT(NJ) OR 
                     (ONE-COUNT(NI) = ONE-COUNT(NJ) AND UNSORTED-ARRAY(NI) < UNSORTED-ARRAY(NJ))
                       THEN
                           MOVE UNSORTED-ARRAY(NI) TO TEMP-VALUE
                           MOVE UNSORTED-ARRAY(NJ) TO UNSORTED-ARRAY(NI)
                           MOVE TEMP-VALUE TO UNSORTED-ARRAY(NJ)

                           MOVE ONE-COUNT(NI) TO TEMP-COUNT
                           MOVE ONE-COUNT(NJ) TO ONE-COUNT(NI)
                           MOVE TEMP-COUNT TO ONE-COUNT(NJ)
                   END-IF
               END-PERFORM
           END-PERFORM.

       MOVE-SORTED-TO-RESULT.

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               MOVE UNSORTED-ARRAY(NI) TO RESULT(NI)
           END-PERFORM.

       FUNCTION NUM-OF-BITS
           INPUT-VALUE TEMP-VALUE
           RETURN-COUNT TEMP-BIN
           .
