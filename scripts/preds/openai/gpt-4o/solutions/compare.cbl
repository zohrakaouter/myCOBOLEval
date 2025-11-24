       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPARE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 COMPARE-RESULT.
           05 GUESS-DIFF PIC S9(10) OCCURS 6 TIMES.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-GAME OCCURS 6 TIMES INDEXED BY NI PIC S9(10).
           05 L-GUESS OCCURS 6 TIMES INDEXED BY NJ PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NK PIC S9(10).

      * I think we all remember that feeling when the result of some long-awaited
      * event is finally known. The feelings and thoughts you have at that moment are
      * definitely worth noting down and comparing.
      * Your task is to determine if a person correctly guessed the results of a number of matches.
      * You are given two arrays of scores and guesses of equal length, where each index shows a match.
      * Return an array of the same length denoting how far off each guess was. If they have guessed correctly,
      * the value is 0, and if not, the value is the absolute difference between the guess and the score.
      * 
      * 
      * example:
      * 
      * compare([1,2,3,4,5,1],[1,2,3,4,2,-2]) -> [0,0,0,0,3,3]
      * compare([0,5,0,0,0,4],[4,1,1,0,0,-2]) -> [4,4,1,0,0,6]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 6
               IF L-GAME(NI) = L-GUESS(NI)
                   MOVE 0 TO GUESS-DIFF(NI)
               ELSE
                   COMPUTE GUESS-DIFF(NI) = FUNCTION ABS(L-GAME(NI) - L-GUESS(NI))
               END-IF
           END-PERFORM.

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 6
               MOVE GUESS-DIFF(NI) TO RESULT(NI)
           END-PERFORM.

           GOBACK.
       END PROGRAM COMPARE.
