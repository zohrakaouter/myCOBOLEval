       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPECIALFILTER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 I            PIC 9(1) VALUE 1.
       01 NUMBER       PIC S9(10).
       01 DIGITS       PIC X(10) VALUE SPACES.
       01 FIRST-DIGIT  PIC 9.
       01 LAST-DIGIT   PIC 9.
       01 ELEMENT-COUNT PIC 9(4) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMS OCCURS 6 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * Write a function that takes an array of numbers as input and returns
      * the number of elements in the array that are greater than 10 and both
      * first and last digits of a number are odd (1, 3, 5, 7, 9).
      * For example:
      * specialFilter([15, -73, 14, -15, 8, 9]) => 1
      * specialFilter([33, -2, -3, 45, 21, 109]) => 2
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
       BEGIN.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 6
               MOVE L-NUMS(NI) TO NUMBER
               IF NUMBER > 10 THEN
                   MOVE FUNCTION NUMVAL-C(FUNCTION REVERSE(FUNCTION NUMERIC-EDIT(NUMBER, Z))) TO DIGITS
                   MOVE FUNCTION NUMVAL-D(DIGITS(1:1)) TO LAST-DIGIT
                   MOVE FUNCTION NUMVAL-D(DIGITS(LENGTH OF FUNCTION REVERSE(FUNCTION NUMERIC-EDIT(NUMBER, Z)):1)) TO FIRST-DIGIT
                   IF FUNCTION MOD(FIRST-DIGIT, 2) = 1 AND FUNCTION MOD(LAST-DIGIT, 2) = 1 THEN
                       ADD 1 TO ELEMENT-COUNT
                   END-IF
               END-IF
           END-PERFORM
           MOVE ELEMENT-COUNT TO RESULT
           GOBACK.

       END PROGRAM SPECIALFILTER.
