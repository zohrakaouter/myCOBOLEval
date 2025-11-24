       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-NUMBERS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 NUMBERS-ARRAY OCCURS 10 TIMES PIC X(5) VALUE SPACES.
       77 SORTED-ARRAY OCCURS 10 TIMES PIC X(5) VALUE SPACES.
       77 NUM-COUNT PIC 9(2) VALUE 0.

       01 WC-NUMBERS REDEFINES L-NUMBERS.
           05 WC-NUMBER-ITEM OCCURS 10 TIMES PIC X(10).
   
       77 NUM-MAP.
           88 ZR VALUE 'zero'.
           88 ON VALUE 'one'.
           88 TW VALUE 'two'.
           88 TH VALUE 'three'.
           88 FR VALUE 'four'.
           88 FV VALUE 'five'.
           88 SX VALUE 'six'.
           88 SV VALUE 'seven'.
           88 ET VALUE 'eight'.
           88 NN VALUE 'nine'.
   
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS PIC X(100).
           05 RESULT PIC X(100).

      * Input is a space-delimited string of numberals from 'zero' to 'nine'.
      * Valid choices are 'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight' and 'nine'.
      * Return the string with numbers sorted from smallest to largest
      * >>> sort_numbers('three one five')
      * 'one three five'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       BEGIN.
           PERFORM SPLIT-NUMBERS
           PERFORM STORE-NUMBERS
           PERFORM SORT-NUMBERS
           PERFORM COMBINE-SORTED-NUMBERS
           MOVE SORTED-STRING TO RESULT
           GOBACK.
   
       SPLIT-NUMBERS.
           UNSTRING L-NUMBERS DELIMITED BY SPACE
               INTO WC-NUMBER-ITEM (1)
                    WC-NUMBER-ITEM (2)
                    WC-NUMBER-ITEM (3)
                    WC-NUMBER-ITEM (4)
                    WC-NUMBER-ITEM (5)
                    WC-NUMBER-ITEM (6)
                    WC-NUMBER-ITEM (7)
                    WC-NUMBER-ITEM (8)
                    WC-NUMBER-ITEM (9)
                    WC-NUMBER-ITEM (10)
               WITH POINTER NUM-COUNT.
   
       STORE-NUMBERS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-COUNT
               IF WC-NUMBER-ITEM(I) = 'zero' THEN MOVE 0 TO NUMBERS-ARRAY(I)
               ELSE IF WC-NUMBER-ITEM(I) = 'one' THEN MOVE 1 TO NUMBERS-ARRAY(I)
               ELSE IF WC-NUMBER-ITEM(I) = 'two' THEN MOVE 2 TO NUMBERS-ARRAY(I)
               ELSE IF WC-NUMBER-ITEM(I) = 'three' THEN MOVE 3 TO NUMBERS-ARRAY(I)
               ELSE IF WC-NUMBER-ITEM(I) = 'four' THEN MOVE 4 TO NUMBERS-ARRAY(I)
               ELSE IF WC-NUMBER-ITEM(I) = 'five' THEN MOVE 5 TO NUMBERS-ARRAY(I)
               ELSE IF WC-NUMBER-ITEM(I) = 'six' THEN MOVE 6 TO NUMBERS-ARRAY(I)
               ELSE IF WC-NUMBER-ITEM(I) = 'seven' THEN MOVE 7 TO NUMBERS-ARRAY(I)
               ELSE IF WC-NUMBER-ITEM(I) = 'eight' THEN MOVE 8 TO NUMBERS-ARRAY(I)
               ELSE IF WC-NUMBER-ITEM(I) = 'nine' THEN MOVE 9 TO NUMBERS-ARRAY(I)
           END-PERFORM.
   
       SORT-NUMBERS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-COUNT
               PERFORM VARYING J FROM I BY 1 UNTIL J > NUM-COUNT
                   IF NUMBERS-ARRAY(I) > NUMBERS-ARRAY(J)
                   THEN
                       MOVE NUMBERS-ARRAY(I) TO TEMP
                       MOVE NUMBERS-ARRAY(J) TO NUMBERS-ARRAY(I)
                       MOVE TEMP TO NUMBERS-ARRAY(J)
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       COMBINE-SORTED-NUMBERS.
           MOVE SPACES TO SORTED-STRING
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > NUM-COUNT
               EVALUATE NUMBERS-ARRAY(K)
                   WHEN 0 MOVE 'zero ' TO SORTED-ARRAY(K)
                   WHEN 1 MOVE 'one ' TO SORTED-ARRAY(K)
                   WHEN 2 MOVE 'two ' TO SORTED-ARRAY(K)
                   WHEN 3 MOVE 'three ' TO SORTED-ARRAY(K)
                   WHEN 4 MOVE 'four ' TO SORTED-ARRAY(K)
                   WHEN 5 MOVE 'five ' TO SORTED-ARRAY(K)
                   WHEN 6 MOVE 'six ' TO SORTED-ARRAY(K)
                   WHEN 7 MOVE 'seven ' TO SORTED-ARRAY(K)
                   WHEN 8 MOVE 'eight ' TO SORTED-ARRAY(K)
                   WHEN 9 MOVE 'nine ' TO SORTED-ARRAY(K)
               END-EVALUATE
               STRING SORTED-STRING DELIMITED BY SPACE
                      SORTED-ARRAY(K) DELIMITED BY SPACE
                      INTO SORTED-STRING
           END-PERFORM.
           INSPECT SORTED-STRING
               REPLACING TRAILING SPACE BY SPACE.

       END PROGRAM SORT-NUMBERS.
