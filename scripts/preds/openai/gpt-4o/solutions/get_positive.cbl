       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-POSITIVE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 CURR-INDEX PIC S9(10) VALUE 1.
       01 CURR-NUMBER PIC S9(10) VALUE 0.
       01 CURR-POS-POSITION PIC S9(10) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * Return only positive numbers in the list.
      * >>> get_positive([-1, 2, -4, 5, 6])
      * [2, 5, 6]
      * >>> get_positive([5, 3, -5, 2, -3])
      * [5, 3, 2]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

      * Start processing each element in the given list
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               MOVE L-L(NI) TO CURR-NUMBER
      * Check if the current number is positive
               IF CURR-NUMBER GREATER THAN 0 THEN
      * If positive, store in the RESULT array
                   MOVE CURR-NUMBER TO RESULT(CURR-POS-POSITION)
                   ADD 1 TO CURR-POS-POSITION
               END-IF
           END-PERFORM.

      * This marks the end of the program logic
           GOBACK.

       END PROGRAM GET-POSITIVE.
