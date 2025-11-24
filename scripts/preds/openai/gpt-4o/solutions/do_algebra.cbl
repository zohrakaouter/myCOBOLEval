       IDENTIFICATION DIVISION.
       PROGRAM-ID. DO-ALGEBRA.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 I PIC 9(1) VALUE 1.
       01 OPERAND1 PIC S9(10).
       01 OPERAND2 PIC S9(10).
       01 OPERATOR PIC X(3).
       01 TEMP-RESULT PIC S9(10).

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

       MAIN-PROCEDURE.
           MOVE L-OPERAND(1) TO TEMP-RESULT

           PERFORM EVALUATE-EXPRESSION
               VARYING I FROM 1 BY 1
               UNTIL I > 3

           MOVE TEMP-RESULT TO RESULT.

           GOBACK.
           
       EVALUATE-EXPRESSION.
           SET NI TO I
           SET NJ TO I + 1

           EVALUATE L-OPERATOR(NI)
               WHEN "+"
                   ADD L-OPERAND(NJ) TO TEMP-RESULT
               WHEN "-"
                   SUBTRACT L-OPERAND(NJ) FROM TEMP-RESULT
               WHEN "*"
                   MULTIPLY L-OPERAND(NJ) BY TEMP-RESULT
               WHEN "//"
                   DIVIDE TEMP-RESULT BY L-OPERAND(NJ) GIVING TEMP-RESULT
               WHEN "**"
                   COMPUTE TEMP-RESULT = TEMP-RESULT ** L-OPERAND(NJ)
               WHEN OTHER
                   DISPLAY "UNKNOWN OPERATOR"
                   MOVE ZERO TO TEMP-RESULT
           END-EVALUATE.

       END PROGRAM DO-ALGEBRA.
