       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVEN-ODD-PALINDROME.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 ODD-COUNT         PIC 9(10) VALUE 0.
       01 EVEN-COUNT        PIC 9(10) VALUE 0.
       01 I                 PIC 9(10) VALUE 1.
       01 NUM-STRING        PIC X(10).
       01 REVERSE-STRING    PIC X(10).
       01 IS-PALINDROME     PIC 9 VALUE 0.
       01 TEMP-NUM          PIC 9(10).
       
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * 
      * Given a positive integer n, return a list that has the number of even and odd
      * integer palindromes that fall within the range(1, n), inclusive.
      * 
      * Example 1:
      * 
      * Input: 3
      * Output: [1, 2]
      * Explanation:
      * Integer palindrome are 1, 2, 3. one of them is even, and two of them are odd.
      * 
      * Example 2:
      * 
      * Input: 12
      * Output: [4, 6]
      * Explanation:
      * Integer palindrome are 1, 2, 3, 4, 5, 6, 7, 8, 9, 11. four of them are even, and 6 of them are odd.
      * 
      * Note:
      * 1. 1 <= n <= 10^3
      * 2. returned tuple has the number of even and odd integer palindromes respectively.
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > L-N
               MOVE I TO NUM-STRING
               MOVE SPACES TO REVERSE-STRING

               PERFORM REVERSE-NUM
               
               IF NUM-STRING = REVERSE-STRING
                   MOVE 1 TO IS-PALINDROME
               ELSE
                   MOVE 0 TO IS-PALINDROME
               END-IF

               IF IS-PALINDROME = 1
                   IF FUNCTION MOD(I 2) = 0
                       ADD 1 TO EVEN-COUNT
                   ELSE
                       ADD 1 TO ODD-COUNT
                   END-IF
               END-IF
           END-PERFORM

           SET NI TO 1
           MOVE EVEN-COUNT TO RESULT(NI)
           SET NI UP BY 1
           MOVE ODD-COUNT TO RESULT(NI)

           GOBACK.
           
       REVERSE-NUM.
           MOVE FUNCTION REVERSE(NUM-STRING(1:FUNCTION LENGTH(TRIMLEADING NUM-STRING)))
               TO REVERSE-STRING
           .

       END PROGRAM EVEN-ODD-PALINDROME.
