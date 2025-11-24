       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATCH-PARENS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-STRING PIC X(200).
       01 OPEN-COUNT    PIC 9(04) VALUE 0.
       01 CLOSE-COUNT   PIC 9(04) VALUE 0.
       01 TOTAL-LENGTH  PIC 9(04).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 2 TIMES INDEXED BY NI PIC X(2).
           05 RESULT PIC X(100).

      * '''
      * You are given a list of two strings, both strings consist of open
      * parentheses '(' or close parentheses ')' only.
      * Your job is to check if it is possible to concatenate the two strings in
      * some order, that the resulting string will be good.
      * A string S is considered to be good if and only if all parentheses in S
      * are balanced. For example: the string '(())()' is good, while the string
      * '())' is not.
      * Return 'Yes' if there's a way to make a good string, and return 'No' otherwise.
      * 
      * Examples:
      * match_parens(['()(', ')']) == 'Yes'
      * match_parens([')', ')']) == 'No'
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
      * Concatenate the two strings in TEMP-STRING
           STRING L-LST(1) DELIMITED BY SIZE
                  L-LST(2) DELIMITED BY SIZE
                  INTO TEMP-STRING.
           
      * Count open and close parentheses
           INSPECT TEMP-STRING TALLYING OPEN-COUNT FOR ALL '('
                               TALLYING CLOSE-COUNT FOR ALL ')'.
           
      * Determine if parentheses are balanced
           ADD LENGTH OF L-LST(1) TO LENGTH OF L-LST(2) GIVING TOTAL-LENGTH.

           IF (OPEN-COUNT = CLOSE-COUNT) AND
              (SUBSTRING(TEMP-STRING, 1, TOTAL-LENGTH) OF '(' <= OPEN-COUNT)
              MOVE 'Yes' TO RESULT
           ELSE
              MOVE 'No' TO RESULT.

           GOBACK.
       END PROGRAM MATCH-PARENS.
