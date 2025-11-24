       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBFIB.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 FIBFIB-ARRAY.
           05 FIBFIB-ELEMENT OCCURS 1000 TIMES INDEXED BY IDX.
               10 FIBFIB-VALUE PIC S9(10) USAGE IS COMPUTATIONAL.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * The FibFib number sequence is a sequence similar to the Fibbonacci sequnece that's defined as follows:
      * fibfib(0) == 0
      * fibfib(1) == 0
      * fibfib(2) == 1
      * fibfib(n) == fibfib(n-1) + fibfib(n-2) + fibfib(n-3).
      * Please write a function to efficiently compute the n-th element of the fibfib number sequence.
      * >>> fibfib(1)
      * 0
      * >>> fibfib(5)
      * 4
      * >>> fibfib(8)
      * 24
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
       MAIN-LOGIC.
           IF L-N = 0
               MOVE 0 TO RESULT
           ELSE IF L-N = 1
               MOVE 0 TO RESULT
           ELSE IF L-N = 2
               MOVE 1 TO RESULT
           ELSE
               PERFORM INITIALIZE-ARRAY
               PERFORM CALCULATE-FIBFIB
               MOVE FIBFIB-VALUE(IDX) TO RESULT
           END-IF.

       INITIALIZE-ARRAY.
           MOVE 0 TO FIBFIB-ELEMENT(1)
           MOVE 0 TO FIBFIB-ELEMENT(2)
           MOVE 1 TO FIBFIB-ELEMENT(3).

       CALCULATE-FIBFIB.
           PERFORM VARYING IDX FROM 4 BY 1 UNTIL IDX > L-N
               COMPUTE FIBFIB-VALUE(IDX) = 
                   FIBFIB-VALUE(IDX - 1) +
                   FIBFIB-VALUE(IDX - 2) +
                   FIBFIB-VALUE(IDX - 3)
           END-PERFORM.

       GOBACK.
