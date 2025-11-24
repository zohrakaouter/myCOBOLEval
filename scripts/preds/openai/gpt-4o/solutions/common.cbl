       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMMON.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 INPUT-TABLE-1.
           05 I1-ITEMS OCCURS 4 TIMES PIC 9(2) VALUE ZEROS.

       01 INPUT-TABLE-2.
           05 I2-ITEMS OCCURS 4 TIMES PIC 9(2) VALUE ZEROS.

       01 WS-COUNTERS.
           05 I          PIC 9(2) VALUE 1.
           05 J          PIC 9(2) VALUE 1.
           05 RESULT-COUNT PIC 9(2) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L1 OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 L-L2 OCCURS 4 TIMES INDEXED BY NJ PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NK PIC S9(10).

      * Return sorted unique common elements for two lists.
      * >>> common([1, 4, 3, 34], [5, 7])
      * []
      * >>> common([5, 3, 2, 8], [3, 2])
      * [2, 3]
      * 
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               MOVE I1-ITEMS(NI) TO L-L1(NI)
           END-PERFORM

           PERFORM VARYING NJ FROM 1 BY 1 UNTIL NJ > 4
               MOVE I2-ITEMS(NJ) TO L-L2(NJ)
           END-PERFORM

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               IF I1-ITEMS(NI) NOT EQUAL 0 THEN
                   PERFORM VARYING NJ FROM 1 BY 1 UNTIL NJ > 4
                       IF I1-ITEMS(NI) EQUAL I2-ITEMS(NJ)
                       THEN
                           PERFORM CHECK-AND-ADD
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

           PERFORM SORT-RESULT
           DISPLAY 'Sorted Unique Common Elements: ' RESULT (1:RESULT-COUNT)
           GOBACK.

       CHECK-AND-ADD.
           MOVE 0 TO J
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > RESULT-COUNT
               IF RESULT(J) EQUAL I1-ITEMS(NI)
               THEN
                   MOVE 1 TO J
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF J EQUAL 0
           THEN
               ADD 1 TO RESULT-COUNT
               MOVE I1-ITEMS(NI) TO RESULT(RESULT-COUNT)
           END-IF
           .

       SORT-RESULT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RESULT-COUNT - 1
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > RESULT-COUNT
                   IF RESULT(I) GREATER THAN RESULT(J)
                   THEN
                       MOVE RESULT(I) TO RESULT(100)
                       MOVE RESULT(J) TO RESULT(I)
                       MOVE RESULT(100) TO RESULT(J)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       END PROGRAM COMMON.
