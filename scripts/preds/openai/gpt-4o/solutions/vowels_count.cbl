       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOWELS-COUNT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  VOWELS    PIC X(6) VALUE 'AEIOU'.
       01  VOWEL-COUNT      PIC S9(10) VALUE 0.
       01  INDEX            PIC 9(3) VALUE 1.

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
       
           INSPECT FUNCTION UPPER-CASE(L-S) TALLYING VOWEL-COUNT 
               FOR ALL CHARACTERS IN VOWELS

           IF FUNCTION LENGTH(L-S) > 0 AND 
              FUNCTION LOWER-CASE(INSPECT L-S FUNCTION REVERSE
              TALLYING LEADING SPACES FOR ALL SPACE)
              = 'Y' THEN
               ADD 1 TO VOWEL-COUNT
           END-IF

           MOVE VOWEL-COUNT TO RESULT
           GOBACK.

       END PROGRAM VOWELS-COUNT.
