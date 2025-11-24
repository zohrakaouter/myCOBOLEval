       IDENTIFICATION DIVISION.
       PROGRAM-ID. INT-TO-MINI-ROMAN.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 NUM-REMAINDER PIC S9(10).
       01 ROMAN-RESULT PIC X(100) VALUE SPACES.
       01 ROMAN-MAPPINGS.
           05 ROMAN-VALUES OCCURS 13 TIMES.
               10 NUMERIC-VALUE  PIC 9(4).
               10 ROMAN-LITERAL  PIC X(2).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBER PIC S9(10).
           05 RESULT PIC X(100).

      * 
      * Given a positive integer, obtain its roman numeral equivalent as a string,
      * and return it in lowercase.
      * Restrictions: 1 <= num <= 1000
      * 
      * Examples:
      * >>> int_to_mini_roman(19) == 'xix'
      * >>> int_to_mini_roman(152) == 'clii'
      * >>> int_to_mini_roman(426) == 'cdxxvi'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
       INITIALIZE
           NUMERIC-VALUE (1)  TO 1000   ROMAN-LITERAL (1)  TO "m"
           NUMERIC-VALUE (2)  TO 900    ROMAN-LITERAL (2)  TO "cm"
           NUMERIC-VALUE (3)  TO 500    ROMAN-LITERAL (3)  TO "d"
           NUMERIC-VALUE (4)  TO 400    ROMAN-LITERAL (4)  TO "cd"
           NUMERIC-VALUE (5)  TO 100    ROMAN-LITERAL (5)  TO "c"
           NUMERIC-VALUE (6)  TO 90     ROMAN-LITERAL (6)  TO "xc"
           NUMERIC-VALUE (7)  TO 50     ROMAN-LITERAL (7)  TO "l"
           NUMERIC-VALUE (8)  TO 40     ROMAN-LITERAL (8)  TO "xl"
           NUMERIC-VALUE (9)  TO 10     ROMAN-LITERAL (9)  TO "x"
           NUMERIC-VALUE (10) TO 9      ROMAN-LITERAL (10) TO "ix"
           NUMERIC-VALUE (11) TO 5      ROMAN-LITERAL (11) TO "v"
           NUMERIC-VALUE (12) TO 4      ROMAN-LITERAL (12) TO "iv"
           NUMERIC-VALUE (13) TO 1      ROMAN-LITERAL (13) TO "i".
           
       MOVE L-NUMBER TO NUM-REMAINDER.
       PERFORM VARYING VARY-INDEX FROM 1 BY 1 UNTIL VARY-INDEX > 13
           EVALUATE TRUE
               WHEN NUM-REMAINDER >= NUMERIC-VALUE (VARY-INDEX)
                   MOVE ROMAN-LITERAL (VARY-INDEX) TO ROMAN-RESULT (NUM-REG, 4)
                   ADD NUMERIC-VALUE (VARY-INDEX) TO NUM-REMAINDER
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
       END-PERFORM.

       MOVE FUNCTION TRIM(ROMAN-RESULT) TO RESULT.
   
       GOBACK.
       END PROGRAM INT-TO-MINI-ROMAN.
