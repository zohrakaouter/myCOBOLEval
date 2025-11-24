       IDENTIFICATION DIVISION.
       PROGRAM-ID. DO-ALGEBRA.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01  I PIC 9(2).
       01  TEMP-RESULT PIC S9(10).
       01  FIRST-OPERAND PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-OPERATOR OCCURS 3 TIMES INDEXED BY NI PIC X(3).
           05 L-OPERAND OCCURS 3 TIMES INDEXED BY NJ PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * Given two lists operator, and operand. The first list has basic algebra operations, and
      * the second list is a list of integers. Use the two given lists to build the algebric
      * expression and return the evaluation of this expression.
      * 
      * The basic algebra operations:
      * Addition ( + )
      * Subtraction ( - )
      * Multiplication ( * )
      * Floor division ( // )
      * Exponentiation ( ** )
      * 
      * Example:
      * operator['+', '*', '-']
      * array = [2, 3, 4, 5]
      * result = 2 + 3 * 4 - 5
      * => result = 9
      * 
      * Note:
      * The length of operator list is equal to the length of operand list minus one.
      * Operand is a list of of non-negative integers.
      * Operator list has at least one operator, and operand list has at least two operands.
      * 
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           MOVE L-OPERAND(1) TO FIRST-OPERAND
           MOVE 1 TO I
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
               EVALUATE L-OPERATOR(I)
                   WHEN '+'
                       COMPUTE TEMP-RESULT = FIRST-OPERAND + L-OPERAND(I + 1)
                   WHEN '-'
                       COMPUTE TEMP-RESULT = FIRST-OPERAND - L-OPERAND(I + 1)
                   WHEN '*'
                       COMPUTE TEMP-RESULT = FIRST-OPERAND * L-OPERAND(I + 1)
                   WHEN '//'
                       DIVIDE FIRST-OPERAND BY L-OPERAND(I + 1)
                           GIVING TEMP-RESULT
                           REMAINDER I
                   WHEN '**'
                       COMPUTE TEMP-RESULT = FUNCTION POWER(FIRST-OPERAND, L-OPERAND(I + 1))
               END-EVALUATE
               MOVE TEMP-RESULT TO FIRST-OPERAND
           END-PERFORM

           MOVE FIRST-OPERAND TO RESULT

           GOBACK.
       END PROGRAM DO-ALGEBRA.
