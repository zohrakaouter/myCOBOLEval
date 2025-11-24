       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIAN.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 L-SORTED OCCURS 5 TIMES PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT COMP-2.

      * Return median of elements in the list l.
      * >>> median([3, 1, 2, 4, 5])
      * 3
      * >>> median([-10, 4, 6, 1000, 10])
      * 6
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       * Copy the elements in L into L-SORTED for sorting
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               MOVE L-L(NI) TO L-SORTED(NI)
           END-PERFORM

       * Sort L-SORTED
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               PERFORM VARYING NI2 FROM NI + 1 BY 1 UNTIL NI2 > 5
                   IF L-SORTED(NI) > L-SORTED(NI2)
                       THEN
                          ADD L-SORTED(NI) TO L-SORTED(NI2) GIVING TEMPCOMP
                          SUBTRACT L-SORTED(NI2) FROM TEMPCOMP GIVING L-SORTED(NI2)
                          SUBTRACT L-SORTED(NI) FROM TEMPCOMP GIVING L-SORTED(NI)
                   END-IF
               END-PERFORM
           END-PERFORM

       * Find the median value after sorting
           MOVE L-SORTED(3) TO RESULT

       GOBACK.
