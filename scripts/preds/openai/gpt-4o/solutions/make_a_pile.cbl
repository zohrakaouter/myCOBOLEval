       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAKE-A-PILE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-NUMBER PIC S9(10).
       01 INDEX       PIC S9(10) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * 
      * Given a positive integer n, you have to make a pile of n levels of stones.
      * The first level has n stones.
      * The number of stones in the next level is:
      * - the next odd number if n is odd.
      * - the next even number if n is even.
      * Return the number of stones in each level in a list, where element at index
      * i represents the number of stones in the level (i+1).
      * 
      * Examples:
      * >>> make_a_pile(3)
      * [3, 5, 7]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           MOVE 0 TO TEMP-NUMBER
           SET NI TO 1

           PERFORM VARYING INDEX FROM 1 BY 1 UNTIL INDEX > L-N
               IF INDEX EQUAL 1
                   MOVE L-N TO RESULT(NI)
               ELSE
                   ADD 2 TO TEMP-NUMBER
                   ADD TEMP-NUMBER TO RESULT(NI)
               END-IF
               SET NI UP BY 1
           END-PERFORM

           GOBACK.
