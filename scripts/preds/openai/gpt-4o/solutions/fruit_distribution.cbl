       IDENTIFICATION DIVISION.
       PROGRAM-ID. FRUIT-DISTRIBUTION.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-APPLE-COUNT PIC S9(10) VALUE 0.
       01 WS-ORANGE-COUNT PIC S9(10) VALUE 0.
       01 WS-TOTAL-FRUITS PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * In this task, you will be given a string that represents a number of apples and oranges
      * that are distributed in a basket of fruit this basket contains
      * apples, oranges, and mango fruits. Given the string that represents the total number of
      * the oranges and apples and an integer that represent the total number of the fruits
      * in the basket return the number of the mango fruits in the basket.
      * for examble:
      * fruit_distribution("5 apples and 6 oranges", 19) ->19 - 5 - 6 = 8
      * fruit_distribution("0 apples and 1 oranges",3) -> 3 - 0 - 1 = 2
      * fruit_distribution("2 apples and 3 oranges", 100) -> 100 - 2 - 3 = 95
      * fruit_distribution("100 apples and 1 oranges",120) -> 120 - 100 - 1 = 19
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-LOGIC SECTION.

           PERFORM PARSE-FRUIT-COUNTS

           SUBTRACT WS-APPLE-COUNT FROM L-N GIVING WS-TOTAL-FRUITS
           SUBTRACT WS-ORANGE-COUNT FROM WS-TOTAL-FRUITS GIVING RESULT
           
           DISPLAY "Result: " RESULT

           GOBACK.

       PARSE-FRUIT-COUNTS SECTION.
       
           UNSTRING L-S DELIMITED BY " "
                     INTO IGNORE
                          WS-APPLE-COUNT
                          IGNORE
                          WS-ORANGE-COUNT
                          IGNORE
                     DELIMITER COUNT IN IGNORE
                     ON OVERFLOW
                        DISPLAY "Error in parsing string."
                     NOT ON OVERFLOW
                        CONTINUE
                     END-UNSTRING.
           
       END PROGRAM FRUIT-DISTRIBUTION.
