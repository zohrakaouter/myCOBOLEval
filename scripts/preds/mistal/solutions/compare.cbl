       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPARE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 I PIC S9(10) VALUE 0.
       01 DIFFERENCE PIC S9(10) VALUE 0.

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

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               SET NI TO I
               SET NJ TO I
               SET NK TO I
               COMPUTE DIFFERENCE = L-GAME(NI) - L-GUESS(NJ)
               IF DIFFERENCE < 0
                   COMPUTE DIFFERENCE = DIFFERENCE * -1
               END-IF
               MOVE DIFFERENCE TO RESULT(NK)
           END-PERFORM

           GOBACK.
       END PROGRAM COMPARE.
