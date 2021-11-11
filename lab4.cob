       IDENTIFICATION DIVISION.
        PROGRAM-ID. LAB4.
        AUTHOR. Martin Funmaker.
      * LAB EXERCISE 4.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'DASCOURSE'.
           SELECT PRNT-FILE ASSIGN TO 'URSPRINT''.
       DATA DIVISION.
        FILE SECTION.
        FD  COURSEFILE
           RECORDING MODE IS F.
           LABEL RECORDS ARE STANDARD.
        01  PRINTREC            PIC X(132).
        WORKING-STORAGE SECTION.
        01  MISC.
           03  EOF              PIC X   VALUE 'N'.
                 88 ENDOFDATA           VALUE 'Y'.
           03 LINECT            PIC 99  VALUE 0.
                 03 TAKEN       PIC 99  VALUE 0.
      ******************************************************
      *         DESCRIPTION OF INPUT DATA LAYOUT         ***
      ******************************************************
       01 COURSEDATA.
           03 C-COURSE.
                05 C-ABB        PIC XXX.
                05 C-NUMB       PIC XXXX.
                05 C-SEC        PIC X(20).
           03 C-TITLE           PIC X(20).
           03 C-SEATSREMAINING  PIC S999.
           03 C-CLASSLIMIT      PIC 999.
           03 FILLER            PIC XXX.
           03 C-STARTINGTIME.
                05 C-STARTINGHOUR       PIC 99.
                05 C-STARTINGMIN        PIC 99.
           03 FILLER            PIC XX.
           03 C-DAYS            PIC X(6).
           03 C-LOCATION.
                05 C-BUILDING   PIC XX.
                05 C-ROOM       PIC XXX.
           03 FILLER            PIC X(24).
      ********************************************************
      *         DESCRIPTION OF HEADING PRINT LINES         ***
      ********************************************************
       01 HEADING1.
           03 FILLER            PIC X(10)       VALUE  SPACES.
           03 FILLER            PIC X(5)        VALUE 'CLASS'.
           03 FILLER            PIC X(11)       VALUE   SPACES.
           03 FILLER            PIC X(8)        VALUE   'LOCATION'.
           03 FILLER            PIC X(8)        VALUE   SPACES.
           03 FILLER            PIC X(4)        VALUE   'DAYS'.
           03 FILLER            PIC X(11)       VALUE   SPACES.
           03 FILLER            PIC X(4)        VALUE   'TIME'.
           03 FILLER            PIC X(10)       VALUE   SPACES.
           03 FILLER            PIC X(5)        VALUE   'CLASS'.
           03 FILLER            PIC X(7)        VALUE   SPACES.
           03 FILLER            PIC XXXX        VALUE   'OPEN'.
           03 FILLER            PIC X           VALUE   SPACES.
           03 FILLER            PIC X(5)        VALUE   'TAKEN'.
       01 HEADING2.
           03 FILLER            PIC X(71)       VALUE   SPACES.
           03 FILLER            PIC X(5)        VALUE   'LIMIT'.
           03 FILLER            PIC X(7)        VALUE   SPACES.
           03 FILLER            PIC X(5)        VALUE   'SEATS'.
      *************************************************************
      *         DESCRIPTION OF PRINT DATA LAYOUT                ***
      *************************************************************
       01 PRINTDATA.
           03 FILLER            PIC X(10)       VALUE   SPACES.
           03 PABB              PIC XXX.
           03 FILLER            PIC X           VALUE   SPACES.
           03 PNUMB             PIC XXXX.
           03 FILLER            PIC X           VALUE   SPACES.
           03 PSEC               PIC XXX.
           03 FILLER            PIC X(5)        VALUE   SPACES.
           03 PBUILDING         PIC XX.
           03 FILLER            PIC X(9)        VALUES  SPACES.
           03 PROOM             PIC Z(3).
           03 FILLER            PIC X(9)        VALUE   SPACES.
           03 PSTARTINGHOUR     PIC Z9.
           03 FILLER            PIC X           VALUE   ':'.
           03 PSTARTINGMIN      PIC 99.
           03 FILLER            PIC X(9)        VALUE SPACES.
           03 PCLASSLIMIT       PIC ZZ9.
           03 FILLER            PIC X(8)        VALUE   SPACES.
           03 PSEATS-REMAINING  PIC ZZ9-.
           03 FILLER            PIC X(9)        VALUE   SPACES.
           03 PTAKEN            PIC ZZ9.

       PROCEDURE DIVISION.
       000MAINLINE.
           OPEN INPUT COURSEFILE
                OUTPUT PRINTFILE.
           PERFORM 800READCOURSEFILE.
           PERFORM 225COURSEHEADINGS.
           PERFORM 100PROCESSLOOP
                UNTIL ENDOFDATA.
           CLOSE COURSEFILE
                PRINTFILE.
           STOP RUN.
      ********************************************************
      *         PRINT EACH CLASS                        *****
      ********************************************************
       100PROCESSLOOP
           IF LINCT > 45
                THEN
                PERFORM 225COURSEHEADINGS.
           MOVE C-ABB           TO PABB.
           MOVE C-NUMB          TO PNUMB.
           MOVE C-SEC           TO PSEC.
           MOVE C-BUILDING      TO PBUILDING.
           MOVE C-ROOM          TO PROOM.
           MOVE C-DAYS          TO PDAYS.
           MOVE C-STARTINGHOUR  TO PSTARTINGHOUR.
           MOVE C-STARTINGMIN   TO PSTARTINGMIN.
           MOVE C-SEATS-REMAINING       TO PSEATS-REMAINING.
           MOVE C-CLASSLIMIT    TO PCLASSLIMIT.
           SUBTRACT C-SEATS-REMAINING FROM C-CLASSLIMIT TO TAKEN.
           MOVE TAKEN           TO PTAKEN.
           INSPECT PDAYS REPLACING ALL ' ' BY '-'.
           WRITE PRINTREC FROM PRINTDATA
                AFTER ADVANCING 1 LINE.
           ADD 1 TO LINECT.
           PERFORM 800READCOURSEFILE.
      **********************************************************
      *                 PRINT HEADING LINE                   ***
      **********************************************************
       225COURSEHEADINGS.
           WRTIE PRINTREC FROM HEADING1
                AFTER ADVANCING PAGE.
           WRITE PRINTREC FROM HEADING2.
                AFTER ADVANCING 1.
           MOVE SPACES TO PRINTREC.
           WRITE PRINTREC
                AFTER ADVANCING 1.
           MOVE 0 TO LINECT.
      *********************************************************
      *         READ THE DATA FILE                      ******
      *********************************************************
       800READCOURSEFILE.
           READ COURSEFILE INTO COURSEDATA
                   AT END MOVE 'Y' TO EOF.

