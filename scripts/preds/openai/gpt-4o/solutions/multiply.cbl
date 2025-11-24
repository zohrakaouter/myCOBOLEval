       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTIPLY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-A PIC 9.
       01 TEMP-B PIC 9.

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

           * Extract the unit digit of L-A
           COMPUTE TEMP-A = FUNCTION MOD(ABS(L-A), 10).

           * Extract the unit digit of L-B
           COMPUTE TEMP-B = FUNCTION MOD(ABS(L-B), 10).

           * Multiply the unit digits and store the result
           COMPUTE RESULT = TEMP-A * TEMP-B.

           * End of the program
           GOBACK.

       END PROGRAM MULTIPLY.
