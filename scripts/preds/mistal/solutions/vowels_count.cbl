       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOWELS-COUNT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-VOWEL-COUNT PIC 9(10) VALUE 0.
           05 WS-INDEX PIC 9(3) VALUE 0.
           05 WS-LENGTH PIC 9(3) VALUE 0.
           05 WS-CHAR PIC X(1).
           05 WS-UPPER-CHAR PIC X(1).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 RESULT PIC S9(10).

      * Write a function vowels_count which takes a string representing
      * a word as input and returns the number of vowels in the string.
      * Vowels in this case are 'a', 'e', 'i', 'o', 'u'. Here, 'y' is also a
      * vowel, but only when it is at the end of the given word.
      * 
      * Example:
      * >>> vowels_count("abcde")
      * 2
      * >>> vowels_count("ACEDY")
      * 3
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE 0 TO WS-VOWEL-COUNT
           INSPECT FUNCTION UPPER-CASE(L-S) TALLYING WS-LENGTH FOR CHARACTERS

           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-LENGTH
               MOVE L-S(WS-INDEX:1) TO WS-CHAR
               MOVE FUNCTION UPPER-CASE(WS-CHAR) TO WS-UPPER-CHAR

               IF WS-UPPER-CHAR = 'A' OR 'E' OR 'I' OR 'O' OR 'U'
                   ADD 1 TO WS-VOWEL-COUNT
               END-IF

               IF WS-INDEX = WS-LENGTH AND WS-UPPER-CHAR = 'Y'
                   ADD 1 TO WS-VOWEL-COUNT
               END-IF
           END-PERFORM

           MOVE WS-VOWEL-COUNT TO RESULT
           GOBACK.
       END PROGRAM VOWELS-COUNT.
