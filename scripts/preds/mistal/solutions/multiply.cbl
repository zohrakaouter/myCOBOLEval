       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTIPLY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-UNIT-A PIC S9.
       01 WS-UNIT-B PIC S9.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 L-B PIC S9(10).
           05 RESULT PIC S9(10).

      * Complete the function that takes two integers and returns
      * the product of their unit digits.
      * Assume the input is always valid.
      * Examples:
      * multiply(148, 412) should return 16.
      * multiply(19, 28) should return 72.
      * multiply(2020, 1851) should return 0.
      * multiply(14,-15) should return 20.
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           COMPUTE WS-UNIT-A = FUNCTION MOD(L-A, 10)
           COMPUTE WS-UNIT-B = FUNCTION MOD(L-B, 10)
           COMPUTE RESULT = WS-UNIT-A * WS-UNIT-B
           GOBACK.
       END PROGRAM MULTIPLY.
