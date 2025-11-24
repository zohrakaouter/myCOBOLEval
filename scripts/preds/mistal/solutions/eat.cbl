       IDENTIFICATION DIVISION.
       PROGRAM-ID. EAT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-VARIABLES.
           05 WS-TOTAL-EATEN PIC S9(10).
           05 WS-CARROTS-LEFT PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBER PIC S9(10).
           05 L-NEED PIC S9(10).
           05 L-REMAINING PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * 
      * You're a hungry rabbit, and you already have eaten a certain number of carrots,
      * but now you need to eat more carrots to complete the day's meals.
      * you should return an array of [ total number of eaten carrots after your meals,
      * the number of carrots left after your meals ]
      * if there are not enough remaining carrots, you will eat all remaining carrots, but will still be hungry.
      * 
      * Example:
      * * eat(5, 6, 10) -> [11, 4]
      * * eat(4, 8, 9) -> [12, 1]
      * * eat(1, 10, 10) -> [11, 0]
      * * eat(2, 11, 5) -> [7, 0]
      * 
      * Variables:
      * @number : integer
      * the number of carrots that you have eaten.
      * @need : integer
      * the number of carrots that you need to eat.
      * @remaining : integer
      * the number of remaining carrots thet exist in stock
      * 
      * Constrain:
      * * 0 <= number <= 1000
      * * 0 <= need <= 1000
      * * 0 <= remaining <= 1000
      * 
      * Have fun :)
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           COMPUTE WS-TOTAL-EATEN = L-NUMBER + FUNCTION MIN(L-NEED, L-REMAINING)
           COMPUTE WS-CARROTS-LEFT = L-REMAINING - FUNCTION MIN(L-NEED, L-REMAINING)

           MOVE WS-TOTAL-EATEN TO RESULT(1)
           MOVE WS-CARROTS-LEFT TO RESULT(2)

           GOBACK.
