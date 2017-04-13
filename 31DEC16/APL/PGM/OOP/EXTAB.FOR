LOCAL INCLUDE 'FORMAT.INC'
C
C   Variables in this common block are used in formatting the table
C   data for output.
C
C   BUFLEN    The length of the output buffer in characters
C   OFBUFF    The output buffer
C   OFPOS     Index of last  filled character position in the output
C             buffer
C
C   NUMCOL    The number of output columns
C
C   OFLUN     The AIPS LUN for the output text file.
C   OFFIND    The AIPS FTAB index for the output text file
C
C   OFSEP     The character that will be used to separated fields on
C             the same line of the output file
C
C   OFDPWD    The maximum width of a double-precision floating-point
C             field in characters
C   OFSPWD    The maximum width of a single-precision floating-point
C             field in characters
C   OFINWD    The maximum width of an integer field in characters
C   OFLGWD    The maximum width of a logical field in characters
C   OFTMWD    The maximum width of a time field
C
C   If the formats in OFWRDP, OFWRSP, OFWRIN, OFWRLG, or OFWRTM are
C   changed then OFDPWD, OFSPWD, OFINWD, OFLGWD and OFTMWD should be
C   changed accordingly.
C
      INTEGER   BUFLEN
      PARAMETER (BUFLEN = 500)
      CHARACTER OFBUFF*(BUFLEN)
      INTEGER   OFPOS
C
      INTEGER   NUMCOL
C
      INTEGER   OFLUN
      PARAMETER (OFLUN = 10)
      INTEGER   OFFIND
C
      CHARACTER OFSEP
C
      INTEGER   OFDPWD, OFSPWD, OFINWD, OFLGWD, OFTMWD
      PARAMETER (OFDPWD = 25)
      PARAMETER (OFSPWD = 25)
      PARAMETER (OFINWD = 20)
      PARAMETER (OFLGWD = 5)
      PARAMETER (OFTMWD = 14)
C
      COMMON /OFNUMS/ OFPOS, NUMCOL, OFFIND
      COMMON /OFCHAR/ OFBUFF, OFSEP
      SAVE /OFNUMS/
      SAVE /OFCHAR/
C
LOCAL END
      PROGRAM EXTAB
C-----------------------------------------------------------------------
C! Task to EXPORT contents of table extension files as text files.
C# Calibration EXT-appl EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1999
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Write selected data from an AIPS table to a text file using tab-
C   separated values. Convert array-valued columns to multiple output
C   columns while doing this.
C
C
C   Local variables:
C
C   TABLE     Name of TABLE object used to access input table
C   OUTFIL    Name of output file
C
C   MAXCOL    Maximum number of columns in a table
C   NCOLS     Number of columns selected from input table
C   COLS      List of columns selected from input table
C
C   BCOUNT    Number of first table row to be written
C   ECOUNT    Number of last table row to be written
C   XINC      Increment between row numbers to be written
C
C   DOHMS     Convert times to hh:mm:ss.s form?
C
C   NCOUNT    Number of array entries to be written from first block
C   BDROP     First array entry to be written from second block
C   EDROP     Last array entry to be written from second block
C
C   IRET      Return status
C   IRET2     Return status from TABCLO
C   DIEBUF    Scratch buffer for DIE
C
      CHARACTER TABLE*11, OUTFIL*48
      PARAMETER (TABLE = 'Input Table')
C
      INTEGER   MAXCOL, NCOLS
      PARAMETER (MAXCOL = 500)
      INTEGER   COLS(MAXCOL)
C
      INTEGER   BCOUNT, ECOUNT, XINC
      LOGICAL   DOHMS
      INTEGER   NCOUNT, BDROP, EDROP
C
      INTEGER   IRET, IRET2, DIEBUF(256)
C
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Initialize AIPS run-time and interpret input adverbs:
C
      CALL ETINIT (TABLE, MAXCOL, OUTFIL, NCOLS, COLS, BCOUNT, ECOUNT,
     *             XINC, DOHMS, NCOUNT, BDROP, EDROP, IRET)
C
C     If IRET is zero then all of the preconditions for calling ETPROC
C     are guaranteed to hold. If IRET is not zero then TABLE is either
C     undefined or closed.
C
      IF (IRET .EQ. 0) THEN
C
C        Copy the selected data to the output file:
C
         CALL ETPROC (TABLE, OUTFIL, NCOLS, COLS, BCOUNT, ECOUNT, XINC,
     *                DOHMS, NCOUNT, BDROP, EDROP, IRET)
C
C        If IRET is 0 or -1 then the selected data have been written to
C        the output file and the input table is still open. If IRET is
C        -1 then no unflagged rows were selected by the user.
C
         IF (IRET .EQ. -1) THEN
            IRET = 0
         END IF
C
C
         CALL TABCLO (TABLE, IRET2)
         IF (IRET2 .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET2
            CALL MSGWRT (9)
C
C           Only set IRET to indicate an error in closing the table
C           if there is no previous error code to be reported:
C
            IF (IRET .EQ. 0) THEN
               IRET = IRET2
            END IF
         END IF
      END IF
C
      CALL DIE (IRET, DIEBUF)
C-----------------------------------------------------------------------
 9000 FORMAT ('ERROR ', I4, ' ATTEMPTING TO CLOSE INPUT TABLE')
      END
      SUBROUTINE ETINIT (TABLE, MAXCOL, OUTFIL, NCOLS, COLS, BCOUNT,
     *                   ECOUNT, XINC, DOHMS, NCOUNT, BDROP, EDROP,
     *                   IRET)
C-----------------------------------------------------------------------
C   Read the input adverbs and initialize the AIPS run-time system.
C
C   Inputs:
C      TABLE  C*(*)      Name of TABLE object that will be used to read
C                        the input table. May not be all blanks.
C                        If IRET is zero on exit then this object is
C                        attached to the table specified by the values
C                        of the INNAME, INCLASS, INSEQ, INDISK, INEXT,
C                        and INVERS adverbs and this table is open
C                        for reading. If IRET is not zero then this
C                        object is either undefined or the table to
C                        which it is attached is closed.
C      MAXCOL I          Maximum number of input table columns that can
C                        be selected. Must be greater than or equal to
C                        zero.
C
C   Outputs:
C      OUTFIL C*48       The value of the OUTFILE adverb if IRET is
C                        zero; undefined otherwise. If IRET is zero
C                        then OUTFIL contains a logical name prefix
C                        that is a least one character long followed
C                        by a colon and at least one non-blank
C                        character.
C      NCOLS  I          If IRET is zero and the NCOLS is the number of
C                        input table columns that the user selected.
C                        That is, if the BOX adverb contained at
C                        least one positive entry that was no larger
C                        than the the number or columns in the input
C                        table then NCOLS is the number of entries in
C                        BOX that meet this condition otherwise NCOLS
C                        is the number of columns in the input table.
C                        If IRET is zero then 1 <= NCOLS <= MAXCOL.
C                        If IRET is not zero the NCOLS is undefined.
C      COLS   I(MAXCOL)  If IRET is zero then the first NCOLS entries
C                        list the selected columns in order or their
C                        selection. In other words, if the BOX adverb
C                        contained at least one positive entry that
C                        was no larger than the the number or columns
C                        in the input table then the first NCOLS
C                        entries in COLS are these entries from BOX
C                        in the order that they occurred otherwise
C                        the first NCOLS entries are 1, 2, ..., NCOLS.
C                        Note that there may be duplicate entries in
C                        COLS. If IRET is not zero then COLS is
C                        undefined.
C      BCOUNT I          If IRET is zero then BCOUNT is the minimum
C                        row number to be written out. If the BCOUNT
C                        adverb had a positive value then this is
C                        the value of BCOUNT otherwise it is 1. If
C                        IRET is not zero the BCOUNT is undefined.
C      ECOUNT I          If IRET is zero then ECOUNT is the maximum
C                        table row number to be written out. If the
C                        value of the ECOUNT adverb was positive and
C                        greater than the value of the BCOUNT adverb
C                        then this is the value of the ECOUNT adverb
C                        otherwise it is the highest row number in the
C                        table.
C      XINC   I          If IRET is zero then XINC is the row number
C                        increment. If the value if the XINC adverb
C                        was positive then this is the value of the
C                        XINC adverb otherwise it is 1. If IRET is not
C                        zero then XINC is undefined.
C      DOHMS  L          If IRET is zero then DOHMS indicates whether
C                        columns whose titles begin with 'TIME' should
C                        be output using hh:mm:ss.s representations:
C                        DOHMS is true if the value of the DOMHS adverb
C                        was positive and false otherwise. If IRET is
C                        not zero then DOHMS is undefined.
C      NCOUNT I          If IRET is zero then NCOUNT is the number of
C                        array indices to write in the first block of
C                        column values. If the NCOUNT adverb had a
C                        positive value then this is the value of the
C                        NCOUNT adverb otherwise it is 10000. If IRET
C                        not zero then NCOUNT is undefined.
C      BDROP  I          If IRET is zero then BDROP is the first array
C                        index to write in the second block of column
C                        values. If the BDROP adverb has a value greater
C                        than NCOUNT then this is the value of the BDROP
C                        adverb otherwise it is NCOUNT + 1. If IRET is
C                        not zero then BDROP is undefined.
C      EDROP  I          If IRET is zero then EDROP is the last array
C                        index to write in the second block of column
C                        values. If the EDROP adverb has a value greater
C                        than or equal to the value of the BDROP adverb
C                        then this is the value of the EDROP adverb
C                        otherwise it is 10000.
C                        If IRET is not zero then EDROP is undefined.
C      IRET   I          Status:
C                           0 - successful
C                           1 - failed to open input table
C                           2 - OUTFILE was blank or not formatted
C                               correctly
C                           3 - too many columns were selected
C                           4 - the AIPS run-time system was not
C                               initialized or the adverb values could
C                               not be read
C                           5 - the TABLE object could not be created
C                           6 - adverb values were not transferred to
C                               the table object
C                           999 - a logic error was detected
C
C   Notes:
C      - EDROP < BDROP indicates that the second block of column values
C        is empty (ie. only the first NDROP array indices will be
C        written to the output file).
C      - The ranges 1..NCOUNT and BDROP..EDROP are guaranteed not to
C        overlap.
C      - Error messages will already have been printed if IRET is not
C        zero on exit.
C      - It would be more intuitive if the default values for BDROP and
C        EDROP were interpreted to be a request for no second block of
C        array values. The current behaviour was chosen to be "bug-
C        compatible" with PRTAB.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      INTEGER   MAXCOL
      CHARACTER OUTFIL*48
      INTEGER   NCOLS, COLS(MAXCOL), BCOUNT, ECOUNT, XINC
      LOGICAL   DOHMS
      INTEGER   NCOUNT, BDROP, EDROP, IRET
C
      INCLUDE 'INCS:PAOOF.INC'
C
C     Local variables
C
C     INPUTS    Name of INPUTS object used to access adverbs (parameter)
C     TASK      Name of task (parameter)
C     NPARM     Number of input adverbs (parameter)
C     AVNAME    Input adverb names
C     AVTYPE    Input adverb type codes
C     AVDIM     Input adverb dimensions
C
C     NKEYS     Number of input adverbs to copy to TABLE object
C     INKEY     Adverbs to copy to TABLE object
C     OUTKEY    TABLE attributes to receive adverb values
C
C     HMS       Value of DOHMS adverb
C     BOX       Value of BOX adverb
C
C     TROW      Number of rows in table
C     TCOL      Number of columns in table
C
C     TYPE      Attribute type code
C     DIM       Attribute dimensionality
C     NDUMMY    Numeric placeholder
C     CDUMMY    Character placeholder
C
C     IRET2     Temporary holder for TABCLO return status
C
C     IDX       Array index
C
      CHARACTER INPUTS*6, TASK*6
      PARAMETER (INPUTS = 'Inputs')
      PARAMETER (TASK   = 'EXTAB ')
      INTEGER   NPARM
      PARAMETER (NPARM  = 15)
      CHARACTER AVNAME(NPARM)*8
      INTEGER   AVTYPE(NPARM), AVDIM(2, NPARM)
C
      INTEGER   NKEYS
      PARAMETER (NKEYS = 6)
      CHARACTER INKEY(NKEYS)*8, OUTKEY(NKEYS)*8
C
      INTEGER   HMS, BOX(40)
C
      INTEGER   TROW, TCOL
C
      INTEGER   TYPE, DIM(3), NDUMMY
      CHARACTER CDUMMY
C
      INTEGER   IDX
C
      INTEGER   IRET2
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA AVNAME / 'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *              'INEXT   ', 'INVERS  ', 'BCOUNT  ', 'ECOUNT  ',
     *              'XINC    ', 'OUTFILE ', 'DOHMS   ', 'NCOUNT  ',
     *              'BDROP   ', 'EDROP   ', 'BOX     ' /
      DATA AVTYPE / OOACAR, OOACAR, OOAINT, OOAINT,
     *              OOACAR, OOAINT, OOAINT, OOAINT,
     *              OOAINT, OOACAR, OOAINT, OOAINT,
     *              OOAINT, OOAINT, OOAINT /
      DATA AVDIM / 12, 1,   6, 1,   1,  1,   1, 1,
     *              2, 1,   1, 1,   1,  1,   1, 1,
     *              1, 1,  48, 1,   1,  1,   1, 1,
     *              1, 1,   1, 1,   4, 10 /
C
      DATA INKEY  / 'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *              'INEXT   ', 'INVERS  ' /
      DATA OUTKEY / 'NAME    ', 'CLASS   ', 'IMSEQ   ', 'DISK    ',
     *              'TBLTYPE ', 'VER     ' /
C-----------------------------------------------------------------------
C
C     Initialize the AIPS run-time system and read the adverb values:
C
      CALL AV2INP (TASK, NPARM, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
      IF (IRET .EQ. 0) THEN
C
C        Create a new TABLE object, transfer the adverb values that
C        specify the input table to it, and open the corresponding
C        table for reading:
C
         CALL TABCRE (TABLE, IRET)
         IF (IRET .EQ. 0) THEN
            CALL IN2OBJ (INPUTS, NKEYS, INKEY, OUTKEY, TABLE, IRET)
            IF (IRET .EQ. 0) THEN
               CALL TABOPN (TABLE, 'READ', IRET)
               IF (IRET .EQ. 0) THEN
C
C                 The table is now open for reading.
C
C                 Retrieve the output file name and check that it
C                 has the correct format:
C
                  CALL INGET (INPUTS, 'OUTFILE', TYPE, DIM, NDUMMY,
     *                        OUTFIL, IRET)
                  CALL CHECK ('ETINIT', 1,
     *                        (IRET .EQ. 0) .AND. (TYPE .EQ. OOACAR)
     *                        .AND. (DIM(1) .LE. 48)
     *                        .AND. (DIM(2) .EQ. 1)
     *                        .AND. (DIM(3) .EQ. 0), IRET)
                  IF (IRET .NE. 0) GO TO 999
                  IDX = INDEX (OUTFIL, ':')
                  IF ((IDX .GT. 1) .AND. (IDX .LT. ITRIM(OUTFIL))) THEN
C
C                    There is a least one character before the colon
C                    and at least one character after the colon so
C                    OUTFIL has the required format.
C
C                    Set up the first and last rows and the row
C                    increment:
C
                     CALL TABGET (TABLE, 'NROW', TYPE, DIM, TROW,
     *                            CDUMMY, IRET)
                     CALL CHECK ('ETINIT', 2,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) .EQ. 1)
     *                           .AND. (DIM(2) .EQ. 1)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
                     CALL INGET (INPUTS, 'BCOUNT', TYPE, DIM, BCOUNT,
     *                           CDUMMY, IRET)
                     CALL CHECK ('ETINIT', 3,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) .EQ. 1)
     *                           .AND. (DIM(2) .EQ. 1)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
                     IF (BCOUNT .LE. 0) THEN
                        BCOUNT = 1
                     END IF
                     CALL INGET (INPUTS, 'ECOUNT', TYPE, DIM, ECOUNT,
     *                           CDUMMY, IRET)
                     CALL CHECK ('ETINIT', 4,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) .EQ. 1)
     *                           .AND. (DIM(2) .EQ. 1)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
                     IF (ECOUNT .LT. BCOUNT) THEN
                        ECOUNT = TROW
                     END IF
                     CALL INGET (INPUTS, 'XINC', TYPE, DIM, XINC,
     *                           CDUMMY, IRET)
                     CALL CHECK ('ETINIT', 5,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) .EQ. 1)
     *                           .AND. (DIM(2) .EQ. 1)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
                     IF (XINC .LE. 0) THEN
                        XINC = 1
                     END IF
C
C                    Find out whether time columns should be printed
C                    using hh:mm:ss formats:
C
                     CALL INGET (INPUTS, 'DOHMS', TYPE, DIM, HMS,
     *                           CDUMMY, IRET)
                     CALL CHECK ('ETINIT', 6,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) .EQ. 1)
     *                           .AND. (DIM(2) .EQ. 1)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
                     DOHMS = HMS .GT. 0
C
C                    Get array index ranges:
C
                     CALL INGET (INPUTS, 'NCOUNT', TYPE, DIM, NCOUNT,
     *                           CDUMMY, IRET)
                     CALL CHECK ('ETINIT', 7,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) .EQ. 1)
     *                           .AND. (DIM(2) .EQ. 1)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
                     IF (NCOUNT .LE. 0) THEN
                        NCOUNT = 10000
                     END IF
                     CALL INGET (INPUTS, 'BDROP', TYPE, DIM, BDROP,
     *                           CDUMMY, IRET)
                     CALL CHECK ('ETINIT', 8,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) .EQ. 1)
     *                           .AND. (DIM(2) .EQ. 1)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
C
C                    Keep a temporary copy of BDROP to test EDROP
C                    against:
C
                     IDX = MAX (1, BDROP)
C
                     IF (BDROP .LE. NCOUNT) THEN
                        BDROP = NCOUNT + 1
                     END IF
                     CALL INGET (INPUTS, 'EDROP', TYPE, DIM, EDROP,
     *                           CDUMMY, IRET)
                     CALL CHECK ('ETINIT', 9,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) .EQ. 1)
     *                           .AND. (DIM(2) .EQ. 1)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
                     IF (EDROP .LT. IDX) THEN
                        EDROP = 10000
                     END IF
C
C                    Build the list of columns to be written:
C
                     CALL TABGET (TABLE, 'NCOL', TYPE, DIM, TCOL,
     *                            CDUMMY, IRET)
                     CALL CHECK ('ETINIT', 10,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) .EQ. 1)
     *                           .AND. (DIM(2) .EQ. 1)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
                     CALL INGET (INPUTS, 'BOX', TYPE, DIM, BOX, CDUMMY,
     *                           IRET)
                     CALL CHECK ('ETINIT', 11,
     *                           (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                           .AND. (DIM(1) * DIM(2) .LE. 40)
     *                           .AND. (DIM(3) .EQ. 0), IRET)
                     IF (IRET .NE. 0) GO TO 999
C
                     NCOLS = 0
                     IDX = 0
C
C                    Invariant: If IRET is zero then NCOLS is the number
C                               of entries in BOX(1:IDX) that lie in the
C                               range 1:TCOL and COLS(1:NCOLS) is a list
C                               of these values in the order that they
C                               occur in BOX; If IRET is 3 then there
C                               are more than MAXCOL entries in the
C                               range 1:TCOL in BOX(1:IDX).
C                    Bound: 40 - IDX
C
   10                IF ((IRET .EQ. 0) .AND. (IDX .NE. 40)) THEN
                        IDX = IDX + 1
                        IF ((1 .LE. BOX(IDX))
     *                      .AND. (BOX(IDX) .LE. TCOL)) THEN
                           IF (NCOLS .NE. MAXCOL) THEN
                              NCOLS = NCOLS + 1
                              COLS(NCOLS) = BOX(IDX)
                           ELSE
                              WRITE (MSGTXT, 9010) MAXCOL
                              CALL MSGWRT (9)
                              WRITE (MSGTXT, 9011)
                              CALL MSGWRT (9)
                              IRET = 3
                           END IF
                        END IF
                        GO TO 10
                     END IF
C
C                    NCOLS is now the number of columns selected using
C                    BOX. If this is zero then there is no explicit
C                    column selection so select all columns in the
C                    table (NB. IRET == 3 imples NCOLS /= 0):
C
                     IF (NCOLS .EQ. 0) THEN
                        IF (TCOL .LE. MAXCOL) THEN
                           NCOLS = TCOL
C
C                          Invariant: COLS(1:IDX) = 1:IDX
C
                           DO 20 IDX = 1, NCOLS
                              COLS(IDX) = IDX
   20                      CONTINUE
                        ELSE
                           WRITE (MSGTXT, 9020) MAXCOL
                           CALL MSGWRT (9)
                           WRITE (MSGTXT, 9011)
                           CALL MSGWRT (9)
                           IRET = 3
                        END IF
                     END IF
                  ELSE
                     WRITE (MSGTXT, 9021)
                     CALL MSGWRT (9)
                     IRET = 2
                  END IF
C
C                 Close the input table if errors have been detected
C
                  IF (IRET .NE. 0) THEN
                     CALL TABCLO (TABLE, IRET2)
C
C                    Report any problems closing the table but do not
C                    overwrite the existing error indicator:
C
                     IF (IRET2 .NE. 0) THEN
                        WRITE (MSGTXT, 9022) IRET2
                        CALL MSGWRT (9)
                     END IF
                  END IF
               ELSE
                  WRITE (MSGTXT, 9023)
                  CALL MSGWRT (9)
                  IRET = 1
               END IF
            ELSE
               WRITE (MSGTXT, 9024) IRET
               CALL MSGWRT (9)
               IRET = 6
            END IF
         ELSE
            WRITE (MSGTXT, 9025) IRET
            CALL MSGWRT (9)
            IRET = 5
         END IF
      ELSE
         WRITE (MSGTXT, 9026) IRET
         CALL MSGWRT (9)
         IRET = 4
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 9010 FORMAT ('MORE THAN ', I4, ' COLUMNS WERE SELECTED')
 9011 FORMAT ('USE BOX TO SELECT FEWER COLUMNS')
 9020 FORMAT ('TABLE HAS MORE THAN ', I4, 'COLUMNS')
 9021 FORMAT ('OUTFILE MUST HAVE THE FORM ''LOGICAL:FILENAME''')
 9022 FORMAT ('ERROR ', I6, ' CLOSING INPUT TABLE AFTER ERROR')
 9023 FORMAT ('COULD NOT OPEN INPUT TABLE: CHECK INPUTS')
 9024 FORMAT ('ERROR ', I6, ' COPYING INPUTS TO TABLE OBJECT')
 9025 FORMAT ('ERROR ', I6, ' CREATING NEW TABLE OBJECT')
 9026 FORMAT ('ERROR ', I6, ' INITIALIZING TASK')
      END
      SUBROUTINE ETPROC (TABLE, OUTFIL, NCOLS, COLS, BCOUNT, ECOUNT,
     *                   XINC, DOHMS, NCOUNT, BDROP, EDROP, IRET)
C-----------------------------------------------------------------------
C   Write selected data from TABLE to OUTFIL.
C
C   Inputs:
C      TABLE   C*(*)     Name of the TABLE object that refers to the
C                        input table. Must not be blank and must refer
C                        to a table that is open for reading and that
C                        has no more than 500 columns.
C      OUTFIL  C*(*)     Name of output file. Must not be blank and
C                        must contain a logical area prefix followed by
C                        a colon and then the file name.
C      NCOLS   I         The number of input table columns to be written
C                        to the output file. Must be greater than or
C                        equal to zero.
C      COLS    I(NCOLS)  The list of input table column numbers to be
C                        written to the output file. Each antry must
C                        be greater than 0 and less than or equal to
C                        the number of columns in TABLE.
C      BCOUNT  I         The first row to write to the output file.
C                        Must be greater than zero.
C      ECOUNT  I         The last row to write to the output file. Must
C                        not be greater than the number of rows in
C                        TABLE.
C      XINC    I         Row number increment. Must be greater than 0.
C      DOHMS   L         Should times be written as hh:mm:ss?
C      NCOUNT  I         Number of array indices in first block. Must
C                        be greater than or equal to 0.
C      BDROP   I         First array index in second block. Must be
C                        greater than NCOUNT.
C      EDROP   I         Last array index in second block.
C
C   Output:
C      IRET    I         Status:
C                         0 - All selected data was written to the
C                             output file.
C                        -1 - No rows were selected: output file
C                             contains no data
C                         1 - Could not open the output file
C                         2 - Failed to read data from input file
C                         3 - Failed to write data to output file
C                         4 - At least one table column had more
C                             array elements than could fit in the
C                             local buffers
C
C   Notes:
C      - If IRET is not zero on completion then an error message has
C        already been written.
C      - If ECOUNT < BCOUNT then no rows are selected
C      - If EDROP < BDROP then the second index block is empty
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OUTFIL*(*)
      INTEGER   NCOLS
      INTEGER   COLS(NCOLS)
      INTEGER   BCOUNT, ECOUNT, XINC
      LOGICAL   DOHMS
      INTEGER   NCOUNT, BDROP, EDROP, IRET
C
C     Local variables:
C
C     MAXCOL    Maximum number of columns in the input table
C     TCOL      Actual number of columns in the input table
C     COLLBL    Column labels
C     COLTYP    Column types
C     COLDIM    Column dimensions
C
C     TABDTM    Special type code for double-precision value to be
C               printed as hh:mm:ss time.
C     TABSTM    Special type code for single-precision value to be
C               printed as hh:mm:ss time.
C
C     MAXSTR    Maximum length of string-values columns
C     MAXIND    Maximum index for array-valued columns
C     DBLVAL    Buffer for double-precision valued columns
C     FLTVAL    Buffer for single-precision valued columns
C     CHRVAL    Buffer for character-valued columns
C     INTVAL    Buffer for integer-valued columns
C     LOGVAL    Buffer for logical-valued columns
C
C     ROWOUT    Number of rows written
C     ROW       Current row number
C     COL       Current column number
C
C     TYPE      Attribute type
C     DIM       Attribute dimensions
C     NDUMMY    Numerical place-holder
C     CDUMMY    Character place-holder
C
C     MSGSAV    Saved message threshold
C
C     I         Array index
C
      INTEGER   MAXCOL
      PARAMETER (MAXCOL = 500)
      INTEGER   TCOL
      CHARACTER COLLBL(MAXCOL)*24
      INTEGER   COLTYP(MAXCOL), COLDIM(MAXCOL)
C
      INTEGER   TABDTM, TABSTM
      PARAMETER (TABDTM = 11)
      PARAMETER (TABSTM = 12)
C
      INTEGER MAXSTR, MAXIND
      PARAMETER (MAXSTR = 1000)
      PARAMETER (MAXIND = 20000)
      DOUBLE PRECISION DBLVAL(MAXIND)
      REAL      FLTVAL(MAXIND)
      CHARACTER CHRVAL*(MAXSTR)
      INTEGER   INTVAL(MAXIND)
      LOGICAL   LOGVAL(MAXIND)
C
      INTEGER   ROWOUT, ROW, COL
C
      INTEGER   TYPE, DIM(3), NDUMMY
      CHARACTER CDUMMY
C
      INTEGER   MSGSAV
C
      INTEGER   I
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Read table metadata:
C
      CALL TABGET (TABLE, 'NCOL', TYPE, DIM, TCOL, CDUMMY, IRET)
      CALL CHECK ('ETPROC', 1, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *            .AND. (DIM(1) .EQ. 1) .AND. (DIM(2) .EQ. 1)
     *            .AND. (DIM(3) .EQ. 0), IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL TABGET (TABLE, 'COLABEL', TYPE, DIM, NDUMMY, COLLBL, IRET)
      CALL CHECK ('ETPROC', 2, (IRET .EQ. 0) .AND. (TYPE .EQ. OOACAR)
     *            .AND. (DIM(1) .LE. 24) .AND. (DIM(2) .LE. MAXCOL)
     *            .AND. (DIM(3) .EQ. 0), IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL TABGET (TABLE, 'COLTYPE', TYPE, DIM, COLTYP, CDUMMY, IRET)
      CALL CHECK ('ETPROC', 2, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *            .AND. (DIM(1) .LE. MAXCOL) .AND. (DIM(2) .EQ. 1)
     *            .AND. (DIM(3) .EQ. 0), IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL TABGET (TABLE, 'COLDIM', TYPE, DIM, COLDIM, CDUMMY, IRET)
      CALL CHECK ('ETPROC', 3, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *            .AND. (DIM(1) .LE. MAXCOL) .AND. (DIM(2) .EQ. 1)
     *            .AND. (DIM(3) .EQ. 0), IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Mark columns that should be presented as time strings rather
C     than floating-point numbers:
C
      IF (DOHMS) THEN
C
C        Invariant: for all i, 1 <= i <= COL, if COLTYP(i)^ = TABDBL
C                   and COLLBL(i)(1:4) = 'TIME' then COLTYP(i)' = TABDTM
C                   or if COLTYP(i)^ = TABFLT and
C                   COLLBL(I)(1:4) = 'TIME' then COLTYP(i)' = TABSTM
C                   otherwise COLTYP(i)' = COLTYP(i)^ (where ' indicates
C                   the current state and ^ the state before the loop).
C
         DO 10 COL = 1, TCOL
            IF (COLLBL(COL)(1:4) .EQ. 'TIME') THEN
               IF (COLTYP(COL) .EQ. TABDBL) THEN
                  COLTYP(COL) = TABDTM
               ELSE IF (COLTYP(COL) .EQ. TABFLT) THEN
                  COLTYP(COL) = TABSTM
               END IF
            END IF
   10    CONTINUE
      END IF
C
C     Check for columns that are too large for the local buffers:
C
      IRET = 0
      COL = 0
C
C     Invariant: IRET = 0 implies that for all i, 1 <= i <= COL,
C                COLTYP(COLS(i)) = TABSEL,
C                COLTYP(COLS(i)) = TABHOL and
C                COLDIM(COLS(i)) <= MAXSTR, or
C                COLTYP(COLS(i)) = TABDBL, TABFLT, TABINT, TABLOG,
C                TABSTM, TABDTM or TABBIT and
C                COLDIM(COLS(i)) <= MAXIND; IRET = 4 implies that there
C                is some i for which COLTYP(COLS(i)) = TABHOL and
C                COLDIM(COLS(i)) > MAXSTR or COLTYP(COLS(i)) = TABDBL,
C                TABFLT, TABIND, TABLOG, TABBIT, TABDTM, or TABSTM and
C                COLDIM(COLS(i)) > MAXIND
C     Bound: NCOLS - COL
C
   20 IF ((IRET .EQ. 0) .AND. (COL .NE. NCOLS)) THEN
         COL = COL + 1
C
C        Note that TABBIT and TABSEL column types are ignored.
C
         IF ((COLTYP(COLS(COL)) .NE. TABHOL)
     *       .AND. (COLTYP(COLS(COL)) .NE. TABSEL)
     *       .AND. (COLDIM(COLS(COL)) .GT. MAXIND)) THEN
            WRITE (MSGTXT, 9020) COLS(COL), MAXIND
            CALL MSGWRT (9)
            IRET = 4
         ELSE IF ((COLTYP(COLS(COL)) .EQ. TABHOL)
     *            .AND. (COLDIM(COLS(COL)) .GT. MAXSTR)) THEN
            WRITE (MSGTXT, 9021) COLS(COL), MAXSTR
            CALL MSGWRT (9)
            IRET = 4
         END IF
         GO TO 20
      END IF
C
C     If IRET is zero then all of the selected columns fit in the local
C     buffers.
C
      IF (IRET .EQ. 0) THEN
C
C        Set up the output file:
C
         CALL OFINIT (OUTFIL, COLLBL, COLTYP, COLDIM, NCOLS, COLS,
     *                NCOUNT, BDROP, EDROP, IRET)
         IF (IRET .EQ. 0) THEN
C
C           Process each selected row:
C
            ROWOUT = 0
            ROW = BCOUNT
C
C           Invariant: IRET = 0 implies that all selected and unflagged
C                      rows in the range BCOUNT:ROW have been written
C                      to the output file and that NUMROW is the number
C                      of rows that have been written
C           Bound: MAX ((ECOUNT + XINC - ROW) / XINC, 0)
C
   30       IF ((IRET .EQ. 0) .AND. (ROW .LE. ECOUNT)) THEN
               CALL OFSROW (IRET)
               IF (IRET .EQ. 0) THEN
C
C              Suppress messages from TABDGT:
C
                  MSGSAV = MSGSUP
                  MSGSUP = 32000
                  COL = 0
C
C                 Invariant: IRET = 0 implies that the first COL
C                            selected columns from row ROW have been
C                            written to the output file
C                 Bound: NCOLS - COL
C
   40             IF ((IRET .EQ. 0) .AND. (COL .NE. NCOLS)) THEN
                     COL = COL + 1
                     IF (COLTYP(COLS(COL)) .EQ. TABDBL) THEN
                        CALL TABDGT (TABLE, ROW, COLS(COL), TYPE, DIM,
     *                               DBLVAL, CDUMMY, IRET)
                        IF (IRET .EQ. 0) THEN
                           CALL OFWRDP (DBLVAL, COLDIM(COLS(COL)),
     *                                  NCOUNT, BDROP, EDROP, IRET)
                           IF (IRET .NE. 0) THEN
                              IRET = 3
                           END IF
                        ELSE IF (IRET .NE. -1) THEN
                           IRET = 2
                        END IF
                     ELSE IF (COLTYP(COLS(COL)) .EQ. TABFLT) THEN
                        CALL TABDGT (TABLE, ROW, COLS(COL), TYPE, DIM,
     *                               FLTVAL, CDUMMY, IRET)
                        IF (IRET .EQ. 0) THEN
                           CALL OFWRSP (FLTVAL, COLDIM(COLS(COL)),
     *                                  NCOUNT, BDROP, EDROP, IRET)
                           IF (IRET .NE. 0) THEN
                              IRET = 3
                           END IF
                        ELSE IF (IRET .NE. -1) THEN
                           IRET = 2
                        END IF
                     ELSE IF (COLTYP(COLS(COL)) .EQ. TABHOL) THEN
                        CALL TABDGT (TABLE, ROW, COLS(COL), TYPE, DIM,
     *                               NDUMMY, CHRVAL, IRET)
                        IF (IRET .EQ. 0) THEN
                           CALL OFWRST (CHRVAL, COLDIM(COLS(COL)),
     *                                  IRET)
                           IF (IRET .NE. 0) THEN
                              IRET = 3
                           END IF
                        ELSE IF (IRET .NE. -1) THEN
                           IRET = 2
                        END IF
                     ELSE IF (COLTYP(COLS(COL)) .EQ. TABINT) THEN
                        CALL TABDGT (TABLE, ROW, COLS(COL), TYPE, DIM,
     *                               INTVAL, CDUMMY, IRET)
                        IF (IRET .EQ. 0) THEN
                           CALL OFWRIN (INTVAL, COLDIM(COLS(COL)),
     *                                  NCOUNT, BDROP, EDROP, IRET)
                           IF (IRET .NE. 0) THEN
                              IRET = 3
                           END IF
                        ELSE IF (IRET .NE. -1) THEN
                           IRET = 2
                        END IF
                     ELSE IF (COLTYP(COLS(COL)) .EQ. TABLOG) THEN
                        CALL TABDGT (TABLE, ROW, COLS(COL), TYPE, DIM,
     *                               LOGVAL, CDUMMY, IRET)
                        IF (IRET .EQ. 0) THEN
                           CALL OFWRLG (LOGVAL, COLDIM(COLS(COL)),
     *                                  NCOUNT, BDROP, EDROP, IRET)
                           IF (IRET .NE. 0) THEN
                              IRET = 3
                           END IF
                        ELSE IF (IRET .NE. -1) THEN
                           IRET = 2
                        END IF
                     ELSE IF (COLTYP(COLS(COL)) .EQ. TABBIT) THEN
                        CALL TABDGT (TABLE, ROW, COLS(COL), TYPE, DIM,
     *                               LOGVAL, CDUMMY, IRET)
                        IF (IRET .EQ. 0) THEN
                           CALL OFWRLG (LOGVAL, COLDIM(COLS(COL)),
     *                                  NCOUNT, BDROP, EDROP, IRET)
                           IF (IRET .NE. 0) THEN
                              IRET = 3
                           END IF
                        ELSE IF (IRET .NE. -1) THEN
                           IRET = 2
                        END IF
                     ELSE IF (COLTYP(COLS(COL)) .EQ. TABDTM) THEN
                        CALL TABDGT (TABLE, ROW, COLS(COL), TYPE, DIM,
     *                               DBLVAL, CDUMMY, IRET)
                        IF (IRET .EQ. 0) THEN
                           CALL OFWRTM (DBLVAL, COLDIM(COLS(COL)),
     *                                  NCOUNT, BDROP, EDROP, IRET)
                           IF (IRET .NE. 0) THEN
                              IRET = 3
                           END IF
                        ELSE IF (IRET .NE. -1) THEN
                           IRET = 2
                        END IF
                     ELSE IF (COLTYP(COLS(COL)) .EQ. TABSTM) THEN
                        CALL TABDGT (TABLE, ROW, COLS(COL), TYPE, DIM,
     *                               FLTVAL, CDUMMY, IRET)
                        IF (IRET .EQ. 0) THEN
C
C                          Convert values to double-precision:
C
                           DO 50 I = 1, COLDIM(COLS(COL))
                              IF (FLTVAL(I) .EQ. FBLANK) THEN
                                 DBLVAL(I) = DBLANK
                              ELSE
                                 DBLVAL(I) = DBLE (FLTVAL(I))
                              END IF
   50                      CONTINUE
C
                           CALL OFWRTM (DBLVAL, COLDIM(COLS(COL)),
     *                                  NCOUNT, BDROP, EDROP, IRET)
                           IF (IRET .NE. 0) THEN
                              IRET = 3
                           END IF
                        ELSE IF (IRET .NE. -1) THEN
                           IRET = 2
                        END IF
                     END IF
                     GO TO 40
                  END IF
C
C                 Stop suppressing error messages:
C
                  MSGSUP = MSGSAV
C
                  IF (IRET .EQ. 0) THEN
                     CALL OFFROW (IRET)
                     IF (IRET .EQ. 0) THEN
                        ROWOUT = ROWOUT + 1
                     ELSE
                        WRITE (MSGTXT, 9050) ROW
                        CALL MSGWRT (9)
                        IRET = 3
                     END IF
                  ELSE IF (IRET .EQ. -1) THEN
C
C                    Row was flagged, ignore error and write out the
C                    line buffer (assumed to be blank) so that the
C                    output file module is reset to a between-lines
C                    state:
C
                     CALL OFFROW (IRET)
                     IF (IRET .NE. 0) THEN
                        WRITE (MSGTXT, 9050) ROW
                        CALL MSGWRT (9)
                        IRET = 3
                     END IF
                  ELSE IF (IRET .EQ. 2) THEN
                     WRITE (MSGTXT, 9051) ROW
                     CALL MSGWRT (9)
                  ELSE IF (IRET .EQ. 3) THEN
                     WRITE (MSGTXT, 9050) ROW
                     CALL MSGWRT (9)
                  END IF
               ELSE IF (IRET .NE. 0) THEN
                  WRITE (MSGTXT, 9050) ROW
                  CALL MSGWRT (9)
                  IRET = 3
               END IF
               ROW = ROW + XINC
               GO TO 30
            END IF
C
C           Finish up:
C
            IF (IRET .EQ. 0) THEN
               CALL OFFINI (IRET)
               IF (IRET .NE. 0) THEN
                  IRET = 3
               END IF
               IF (ROWOUT .EQ. 0) THEN
                  WRITE (MSGTXT, 1050)
                  CALL MSGWRT (5)
C
C                 Note this unless an error condition has been flagged:
C
                  IF (IRET .EQ. 0) THEN
                     IRET = -1
                  END IF
               END IF
            END IF
         ELSE
            IRET = 1
         END IF
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('WARNING: no unflagged rows were selected')
 9020 FORMAT ('COLUMN ', I5, ' HAS TOO MANY ARRAY ELEMENTS (> ', I6,
     *        ')')
 9021 FORMAT ('COLUMN ', I5, ' IS TOO WIDE (> ', I6, ' CHARACTERS)')
 9050 FORMAT ('FAILED WHILE WRITING ROW NUMBER ', I6)
 9051 FORMAT ('FAILED WHILE READING ROW NUMBER ', I6)
      END
      SUBROUTINE OFINIT (OUTFIL, COLLBL, COLTYP, COLDIM, NCOLS, COLS,
     *                   NCOUNT, BDROP, EDROP, IRET)
C-----------------------------------------------------------------------
C   Initialize the output module, set the field separator to the
C   horizontal tabulation character  and check whether the requested
C   data will fit in the output buffer. If so open the output file and
C   add column headings.
C
C   Inputs:
C      OUTFIL  C*(*)      The name of the output file. Must comprise a
C                         logical area name followed by a colon and a
C                         filename.
C      COLLBL  C(*)*24    Column labels
C      COLTYP  I(*)       Column type codes:
C                          1 - double-precision
C                          2 - single-precision
C                          3 - character string
C                          4 - integer
C                          5 - logical
C                          7 - bit field
C                          9 - select flag
C                         11 - double-precision time
C                         12 - single-precision time
C      COLDIM   I(*)      Column dimensions
C      NCOLS    I         Number of requested columns; greater than or
C                         equal to zero.
C      COLS     I(NCOLS)  List of requested columns: each entry must
C                         be the index of defined entries in COLLBL,
C                         COLTYP, and COLDIM
C      NCOUNT   I         Number of array indices in first block;
C                         greater than or equal to 1
C      BDROP    I         First index in second block; greater than
C                         NCOUNT
C      EDROP    I         Last index in second block
C
C   Output:
C      IRET     I         Return status
C                         0 - success
C                         1 - table data overflows line buffer
C                         2 - no translation for logical area name
C                         3 - output file already exists
C                         4 - failed to open output file
C                         5 - failed to write to output file
C
C   Notes:
C      - If IRET is not zero on return then a message will have been
C        written to the AIPS message system.
C      - If EDROP < BDROP then there are no array indices in the
C        second block
C      - The first and second blocks of array indices do not overlap
C      - Bit field and selection flag columns are excluded from the
C        output file
C-----------------------------------------------------------------------
      CHARACTER OUTFIL*(*), COLLBL(*)*24
      INTEGER   COLTYP(*), COLDIM(*), NCOLS
      INTEGER   COLS(NCOLS), NCOUNT, BDROP, EDROP, IRET
C
C     Local variables:
C
C     COL       Column number
C     I         Array index
C     LASTIX    Last array index in block
C     HDRLEN    Cumulative length of output column headers
C     DATLEN    Cumulative width of output data columns
C
C     TABDTM    Special code for double-precision times
C     TABSTM    Special code for single-precision times
C
      INTEGER   COL, I, LASTIX, HDRLEN, DATLEN
C
      INTEGER   TABDTM, TABSTM
      PARAMETER (TABDTM = 11)
      PARAMETER (TABSTM = 12)
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
      CHARACTER ZSCHAR
      EXTERNAL  ZSCHAR
C
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FORMAT.INC'
C-----------------------------------------------------------------------
C
C     Calculate the number of output columns and their total width:
C
      NUMCOL = 0
      DATLEN = 0
      HDRLEN = 0
C
C     Invariant: NUMCOL is the number of output columns required for
C                the first COL input columns, HDRLEN is the sum of the
C                lengths of their titles and DATLEN is the sum of their
C                maximum data widths.
C
      DO 30 COL = 1, NCOLS
C
C        Ignore bit selection flags:
C
         IF (COLTYP(COLS(COL)) .NE. TABSEL) THEN
            IF ((COLDIM(COLS(COL)) .EQ. 1)
     *          .OR. ((COLTYP(COLS(COL)) .EQ. TABHOL)
     *                .AND. (COLDIM(COLS(COL)) .GT. 0))) THEN
C
C              Column has a scalar value and maps to a single output
C              column.
C
               NUMCOL = NUMCOL + 1
               HDRLEN = HDRLEN + ITRIM(COLLBL(COLS(COL)))
               IF (COLTYP(COLS(COL)) .EQ. TABDBL) THEN
                  DATLEN = DATLEN + OFDPWD
               ELSE IF (COLTYP(COLS(COL)) .EQ. TABFLT) THEN
                  DATLEN = DATLEN + OFSPWD
               ELSE IF (COLTYP(COLS(COL)) .EQ. TABHOL) THEN
                  DATLEN = DATLEN + COLDIM(COLS(COL))
               ELSE IF (COLTYP(COLS(COL)) .EQ. TABINT) THEN
                  DATLEN = DATLEN + OFINWD
               ELSE IF (COLTYP(COLS(COL)) .EQ. TABLOG) THEN
                  DATLEN = DATLEN + OFLGWD
               ELSE IF (COLTYP(COLS(COL)) .EQ. TABBIT) THEN
                  DATLEN = DATLEN + OFLGWD
               ELSE IF ((COLTYP(COLS(COL)) .EQ. TABDTM)
     *                  .OR. (COLTYP(COLS(COL)) .EQ. TABSTM)) THEN
                  DATLEN = DATLEN + OFTMWD
               END IF
            ELSE IF (COLDIM(COLS(COL)) .GT. 1) THEN
C
C              Column has an array value and maps to multiple output
C              columns. That array index is added to the output column
C              headings.
C
C              First block of array indices:
C
               LASTIX = MIN (NCOUNT, COLDIM(COLS(COL)))
C
C              Invariant: NUMCOL is the total number of output columns
C                         required for input columns 1 to COL-1 and
C                         indices 1 to I of column COL; HDRLEN is the
C                         sum of the lengths of their titles and DATLEN
C                         is the sum of their maximum widths
C
               DO 10 I = 1, LASTIX
                  NUMCOL = NUMCOL + 1
                  HDRLEN = HDRLEN + ITRIM (COLLBL(COLS(COL)))
     *                     + INT (LOG10 (REAL (I))) + 1 + 2
C
C                 Length of index is int(log10(i)) + 1 plus 2 for the
C                 parentheses.
                  IF (COLTYP(COLS(COL)) .EQ. TABDBL) THEN
                     DATLEN = DATLEN + OFDPWD
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABFLT) THEN
                     DATLEN = DATLEN + OFSPWD
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABHOL) THEN
                     DATLEN = DATLEN + COLDIM(COLS(COL))
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABINT) THEN
                     DATLEN = DATLEN + OFINWD
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABLOG) THEN
                     DATLEN = DATLEN + OFLGWD
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABBIT) THEN
                     DATLEN = DATLEN + OFLGWD
                  ELSE IF ((COLTYP(COLS(COL)) .EQ. TABDTM)
     *                     .OR. (COLTYP(COLS(COL)) .EQ. TABSTM)) THEN
                     DATLEN = DATLEN + OFTMWD
                  END IF
   10          CONTINUE
C
C              Second block of array indices
C
               LASTIX = MIN (EDROP, COLDIM(COLS(COL)))
C
C              Invariant: NUMCOL is the total number of output columns
C                         required for input columns 1 to COL-1 and
C                         indices 1 to I of column COL; HDRLEN is the
C                         sum of the lengths of their titles and DATLEN
C                         is the sum of their maximum widths
C
               DO 20 I = MIN (BDROP, COLDIM(COLS(COL)) + 1), LASTIX
                  NUMCOL = NUMCOL + 1
                  HDRLEN = HDRLEN + ITRIM (COLLBL(COLS(COL)))
     *                     + INT (LOG10 (REAL (I))) +1 + 2
C
C                 Length of index is int(log10(i)) +1 plus 2 for the
C                 parentheses.
                  IF (COLTYP(COLS(COL)) .EQ. TABDBL) THEN
                     DATLEN = DATLEN + OFDPWD
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABFLT) THEN
                     DATLEN = DATLEN + OFSPWD
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABHOL) THEN
                     DATLEN = DATLEN + COLDIM(COLS(COL))
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABINT) THEN
                     DATLEN = DATLEN + OFINWD
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABLOG) THEN
                     DATLEN = DATLEN + OFLGWD
                  ELSE IF (COLTYP(COLS(COL)) .EQ. TABBIT) THEN
                     DATLEN = DATLEN + OFLGWD
                  ELSE IF ((COLTYP(COLS(COL)) .EQ. TABDTM)
     *                     .OR. (COLTYP(COLS(COL)) .EQ. TABSTM)) THEN
                     DATLEN = DATLEN + OFTMWD
                  END IF
   20          CONTINUE
            END IF
         ELSE
            WRITE (MSGTXT, 1020) COLS(COL)
            CALL MSGWRT (5)
         END IF
   30 CONTINUE
C
C     DATLEN and HDRLEN do not include the separator characters.
C
      IF (((HDRLEN + NUMCOL - 1) .LE. BUFLEN)
     *    .AND. ((DATLEN + NUMCOL - 1) .LE. BUFLEN)) THEN
C
C        Titles and data will fit in the output buffer.
C
C        Set the separator character:
C
         CALL OFSTSP (ZSCHAR ('tab'))
C
C        Open the output file:
C
         CALL ZTXOPN ('WRIT', OFLUN, OFFIND, OUTFIL, .FALSE., IRET)
         IF (IRET .EQ. 0) THEN
C
C           Write the column titles:
C
            CALL OFTITL (NCOLS, COLS, NCOUNT, BDROP, EDROP, COLLBL,
     *                   COLTYP, COLDIM, IRET)
            IF (IRET .NE. 0) THEN
               IRET = 5
            END IF
         ELSE IF (IRET .EQ. 4) THEN
            WRITE (MSGTXT, 9030)
            CALL MSGWRT (8)
            IRET = 2
         ELSE IF (IRET .EQ. 5) THEN
            WRITE (MSGTXT, 9031)
            CALL MSGWRT (8)
            IRET = 3
         ELSE
            WRITE (MSGTXT, 9032) IRET
            CALL MSGWRT (8)
            IRET = 4
         END IF
      ELSE
         WRITE (MSGTXT, 9033)
         CALL MSGWRT (8)
         WRITE (MSGTXT, 9034)
         CALL MSGWRT (8)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 1020 FORMAT ('WARNING: ignoring selection flag column ', I6)
 9030 FORMAT ('COULD NOT TRANSLATE LOGICAL AREA NAME FOR OUTPUT FILE')
 9031 FORMAT ('I WILL NOT OVERWRITE AN EXISTING FILE')
 9032 FORMAT ('ERROR ', I4, ' OPENING OUTPUT FILE')
 9033 FORMAT ('OUTPUT ROWS WILL NOT FIT ON A SINGLE LINE')
 9034 FORMAT ('SELECT FEWER TABLE COLUMNS')
      END
      SUBROUTINE OFTITL (NCOLS, COLS, NCOUNT, BDROP, EDROP, COLLBL,
     *                   COLTYP, COLDIM, IRET)
C-----------------------------------------------------------------------
C   Write column titles to output file. Should only be called from
C   OFINIT after establishing that the output buffer can hold the
C   titles.
C
C   Inputs:
C      NCOLS    I         Number of requested columns; greater than or
C                         equal to zero.
C      COLS     I(NCOLS)  List of requested columns: each entry must
C                         be the index of defined entries in COLLBL,
C                         COLTYP, and COLDIM
C      NCOUNT   I         Number of array indices in first block;
C                         greater than or equal to 1
C      BDROP    I         First index in second block; greater than
C                         NCOUNT
C      EDROP    I         Last index in second block
C      COLLBL   C(*)*24   Column labels
C      COLTYP   I(*)      Column type codes:
C                          1 - double-precision
C                          2 - single-precision
C                          3 - character string
C                          4 - integer
C                          5 - logical
C                          7 - bit field
C                          9 - select flag
C                         11 - double-precision time
C                         12 - single-precision time
C      COLDIM  I(*)       Column dimensions
C
C   Output:
C      IRET    I          Status:
C                         0 - titles written
C                         1 - failed to write to output file and
C                             wrote error message
C-----------------------------------------------------------------------
      INTEGER   NCOLS
      INTEGER   COLS(NCOLS), NCOUNT, BDROP, EDROP
      CHARACTER COLLBL(*)*24
      INTEGER   COLTYP(*), COLDIM(*), IRET
C
C     Local variables:
C
C     COL       Input column
C     I         Array index
C     LASTIX    Last array index in current block
C     TITLE     Current column title, less index
C     TLEN      Number of characters in TITLE (without trailing spaces)
C     NUM       Formatted array index
C     NUMLEN    Length of formatted array index
C
      INTEGER   COL, I, LASTIX
      CHARACTER TITLE*24
      INTEGER   TLEN
      CHARACTER NUM*20
      INTEGER   NUMLEN
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FORMAT.INC'
C-----------------------------------------------------------------------
      OFPOS = 0
C
C     Invariant: the output buffer contains the titles for the output
C                columns corresponding to input columns 1 to COL
C
      DO 30 COL = 1, NCOLS
         TITLE = COLLBL (COLS(COL))
         TLEN = ITRIM (TITLE)
         IF (COLTYP(COLS(COL)) .NE. TABSEL) THEN
            IF ((COLDIM(COLS(COL)) .EQ. 1)
     *          .OR. ((COLTYP(COLS(COL)) .EQ. TABHOL)
     *                .AND. (COLDIM(COLS(COL)) .GT. 0))) THEN
C
C              Column has a scalar value and maps to a single output
C              column.
C
               IF (OFPOS .GT. 0) THEN
C
C                 A separator is needed.
C
                  OFPOS = OFPOS + 1
                  OFBUFF(OFPOS:OFPOS) = OFSEP
               END IF
               OFBUFF(OFPOS+1:OFPOS+TLEN) = TITLE(1:TLEN)
               OFPOS = OFPOS + TLEN
            ELSE IF (COLDIM(COLS(COL)) .GT. 1) THEN
C
C              Column maps to several output columns.
C
               LASTIX = MIN (NCOUNT, COLDIM(COLS(COL)))
C
C              Invariant: OFBUF contains separated titles for the output
C                         columns corresponding to table columns
C                         COLS(1:COL-1) plus array indices 1:I of
C                         table column COL.
C
               DO 10 I = 1, LASTIX
                  IF (OFPOS .GT. 0) THEN
C
C                    A separator is needed.
C
                     OFPOS = OFPOS + 1
                     OFBUFF(OFPOS:OFPOS) = OFSEP
                  END IF
                  OFBUFF(OFPOS+1:OFPOS+TLEN) = TITLE(1:TLEN)
                  OFPOS = OFPOS + TLEN
                  OFBUFF(OFPOS+1:OFPOS+1) = '('
                  OFPOS = OFPOS + 1
                  WRITE (NUM, 1000) I
                  CALL CHTRIM (NUM, LEN(NUM), NUM, NUMLEN)
                  OFBUFF(OFPOS+1:OFPOS+NUMLEN) = NUM(1:NUMLEN)
                  OFPOS = OFPOS + NUMLEN
                  OFBUFF(OFPOS+1:OFPOS+1) = ')'
                  OFPOS = OFPOS + 1
   10          CONTINUE
C
               LASTIX = MIN (EDROP, COLDIM(COLS(COL)))
C
C              Invariant: OFBUF contains separated titles for the output
C                         columns corresponding to table columns
C                         COLS(1:COL-1) plus array indices 1:I and
C                         MIN(COLDIM(COLS(COL))+1, BDROP):I of
C                         table column COL.
C
               DO 20 I = MIN (BDROP, COLDIM(COLS(COL)) + 1), LASTIX
                  IF (OFPOS .GT. 0) THEN
C
C                    A separator is needed.
C
                     OFPOS = OFPOS + 1
                     OFBUFF(OFPOS:OFPOS) = OFSEP
                  END IF
                  OFBUFF(OFPOS+1:OFPOS+TLEN) = TITLE(1:TLEN)
                  OFPOS = OFPOS + TLEN
                  OFBUFF(OFPOS+1:OFPOS+1) = '('
                  OFPOS = OFPOS + 1
                  WRITE (NUM, 1000) I
                  CALL CHTRIM (NUM, LEN(NUM), NUM, NUMLEN)
                  OFBUFF(OFPOS+1:OFPOS+NUMLEN) = NUM(1:NUMLEN)
                  OFPOS = OFPOS + NUMLEN
                  OFBUFF(OFPOS+1:OFPOS+1) = ')'
                  OFPOS = OFPOS + 1
   20          CONTINUE
            END IF
         END IF
   30 CONTINUE
C
C     Write titles to file:
C
      CALL ZTXIO ('WRIT', OFLUN, OFFIND, OFBUFF(1:OFPOS), IRET)
      IF (IRET .NE. 0) THEN
         WRITE (MSGTXT, 9030) IRET
         CALL MSGWRT (8)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 1000 FORMAT (I20)
 9030 FORMAT ('ERROR ', I5, ' WRITING COLUMN TITLES TO OUTPUT FILE')
      END
      SUBROUTINE OFFINI (IRET)
C-----------------------------------------------------------------------
C   Close the output file.
C
C   Output:
C      IRET     I        Status
C                        0 - new row started
C                        1 - failed to close output file and generated
C                            an error message.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FORMAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL ZTXCLS (OFLUN, OFFIND, IRET)
      IF (IRET .NE. 0) THEN
         WRITE (MSGTXT, 9000) IRET
         CALL MSGWRT (8)
         WRITE (MSGTXT, 9001)
         CALL MSGWRT (8)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('ERROR ', I4, ' CLOSING OUTPUT FILE')
 9001 FORMAT ('SOME DATA MAY HAVE BEEN LOST')
      END
      SUBROUTINE OFSTSP (NEWSEP)
C-----------------------------------------------------------------------
C   Set the separator character used in the output file.
C
C   Input
C      NEWSEP   C*1      New separator character
C-----------------------------------------------------------------------
      CHARACTER NEWSEP
C
      INCLUDE 'FORMAT.INC'
C-----------------------------------------------------------------------
      OFSEP = NEWSEP
      END
      SUBROUTINE OFSROW (IRET)
C-----------------------------------------------------------------------
C   Start a new output row. OFINIT must have been called.
C
C   Output:
C      IRET     I        Status
C                        0 - new row started
C                        1 - failed to write output file and generated
C                            an error message.
C
C   Notes:
C      - If you are adapting EXTAB to generate a new output format,
C        then this is the place to perform and tasks that need to be
C        be done at the start of a new row.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FORMAT.INC'
C-----------------------------------------------------------------------
C
C     Position output pointer at the start of the output buffer:
C
      OFPOS = 0
      IRET = 0
      END
      SUBROUTINE OFFROW (IRET)
C-----------------------------------------------------------------------
C   Finish an output row. This ensures that the row is written to the
C   output file unless it is empty, in which case it will be discarded
C
C   Output:
C      IRET     I        Status
C                        0 - new row started
C                        1 - failed to write to output file and
C                            generated an error message.
C
C   Notes:
C      - If you are adapting EXTAB to generate a new output format,
C        then this is the place to perform and tasks that need to be
C        be done at the end of a row of output.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FORMAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (OFPOS .GT. 0) THEN
         CALL ZTXIO ('WRIT', OFLUN, OFFIND, OFBUFF(1:OFPOS), IRET)
         IF (IRET .NE. 0) THEN
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (8)
            IRET = 1
         END IF
      ELSE
         IRET = 0
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('ERROR ', I4, ' WRITING A ROW TO THE OUTPUT FILE')
      END
      SUBROUTINE OFWRDP (VALUE, DIM, NCOUNT, BDROP, EDROP, IRET)
C-----------------------------------------------------------------------
C   Write out elements 1:MIN(NCOUNT, DIM) and
C   MIN(BDROP, DIM+1):MIN(EDROP, DIM) of double-precision array VALUE.
C
C   Inputs:
C      VALUE   D(DIM)    Value of double-precision column
C      DIM     I         Dimension of column value; greater than or
C                        equal to zero
C      NCOUNT  I         Number of array indices in first block;
C                        greater than or equal to zero
C      BDROP   I         First array index in second block; greater
C                        than NCOUNT
C      EDROP   I         Last array index in second block
C
C   Output:
C      IRET     I        Status
C                        0 - values written
C                        1 - failed to write to output file and
C                            generated an error message.
C-----------------------------------------------------------------------
      INTEGER   DIM
      DOUBLE PRECISION VALUE(DIM)
      INTEGER   NCOUNT, BDROP, EDROP, IRET
C
C     Local variables:
C
C     I         Array index
C     LAST      Last index in block
C
      INTEGER   I, LAST
C
      INCLUDE 'FORMAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C
C     Write first block:
C
      LAST = MIN (DIM, NCOUNT)
C
C     Invariant: array elements 1:I have been written out
C
      DO 10 I = 1, LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         IF (VALUE(I) .EQ. DBLANK) THEN
            OFBUFF(OFPOS+1:OFPOS+10) = 'indefinite'
            OFPOS = OFPOS + 10
         ELSE
            WRITE (OFBUFF(OFPOS+1:OFPOS+OFDPWD), 1000) VALUE(I)
            OFPOS = OFPOS + OFDPWD
         END IF
   10 CONTINUE
C
C     Write second block:
C
      LAST = MIN (DIM, EDROP)
C
C     Invariant: array elements MIN(BDROP, DIM+1):I have been written out
C
      DO 20 I = MIN(BDROP, DIM+1), LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         IF (VALUE(I) .EQ. DBLANK) THEN
            OFBUFF(OFPOS+1:OFPOS+10) = 'indefinite'
            OFPOS = OFPOS + 10
         ELSE
            WRITE (OFBUFF(OFPOS+1:OFPOS+OFDPWD), 1000) VALUE(I)
            OFPOS = OFPOS + OFDPWD
         END IF
   20 CONTINUE
C
      IRET = 0
C-----------------------------------------------------------------------
 1000 FORMAT (E25.17)
      END
      SUBROUTINE OFWRSP (VALUE, DIM, NCOUNT, BDROP, EDROP, IRET)
C-----------------------------------------------------------------------
C   Write out elements 1:MIN(NCOUNT, DIM) and
C   MIN(BDROP, DIM+1):MIN(EDROP, DIM) of single-precision array VALUE.
C
C   Inputs:
C      VALUE   R(DIM)    Value of double-precision column
C      DIM     I         Dimension of column value; greater than or
C                        equal to zero
C      NCOUNT  I         Number of array indices in first block;
C                        greater than or equal to zero
C      BDROP   I         First array index in second block; greater
C                        than NCOUNT
C      EDROP   I         Last array index in second block
C
C   Output:
C      IRET     I        Status
C                        0 - values written
C                        1 - failed to write to output file and
C                            generated an error message.
C-----------------------------------------------------------------------
      INTEGER   DIM
      REAL      VALUE(DIM)
      INTEGER   NCOUNT, BDROP, EDROP, IRET
C
C     Local variables:
C
C     I         Array index
C     LAST      Last index in block
C
      INTEGER   I, LAST
C
      INCLUDE 'FORMAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C
C     Write first block:
C
      LAST = MIN (DIM, NCOUNT)
C
C     Invariant: array elements 1:I have been written out
C
      DO 10 I = 1, LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         IF (VALUE(I) .EQ. FBLANK) THEN
            OFBUFF(OFPOS+1:OFPOS+10) = 'indefinite'
            OFPOS = OFPOS + 10
         ELSE
            WRITE (OFBUFF(OFPOS+1:OFPOS+OFSPWD), 1000) VALUE(I)
            OFPOS = OFPOS + OFSPWD
         END IF
   10 CONTINUE
C
C     Write second block:
C
      LAST = MIN (DIM, EDROP)
C
C     Invariant: array elements MIN(BDROP, DIM+1):I have been written
C                out
C
      DO 20 I = MIN(BDROP, DIM+1), LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         IF (VALUE(I) .EQ. FBLANK) THEN
            OFBUFF(OFPOS+1:OFPOS+10) = 'indefinite'
            OFPOS = OFPOS + 10
         ELSE
            WRITE (OFBUFF(OFPOS+1:OFPOS+OFSPWD), 1000) VALUE(I)
            OFPOS = OFPOS + OFSPWD
         END IF
   20 CONTINUE
C
      IRET = 0
C-----------------------------------------------------------------------
 1000 FORMAT (E25.17)
      END
      SUBROUTINE OFWRST (VALUE, DIM, IRET)
C-----------------------------------------------------------------------
C   Write character string VALUE to the output file provided that
C   DIM > 0.
C
C   Inputs
C      VALUE    C*(*)   Character string
C      DIM      I       Length of character string
C
C   Output:
C      IRET     I        Status
C                        0 - values written
C                        1 - failed to write to output file and
C                            generated an error message.
C-----------------------------------------------------------------------
      CHARACTER VALUE*(*)
      INTEGER   DIM, IRET
C
      INCLUDE 'FORMAT.INC'
C-----------------------------------------------------------------------
      IF (DIM .GT. 0) THEN
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         OFBUFF(OFPOS+1:OFPOS+DIM) = VALUE(1:DIM)
         OFPOS = OFPOS + DIM
      END IF
C
      IRET = 0
C
      END
      SUBROUTINE OFWRIN (VALUE, DIM, NCOUNT, BDROP, EDROP, IRET)
C-----------------------------------------------------------------------
C   Write out elements 1:MIN(NCOUNT, DIM) and
C   MIN(BDROP, DIM+1):MIN(EDROP, DIM) of integer array VALUE.
C
C   Inputs:
C      VALUE   I(DIM)    Value of double-precision column
C      DIM     I         Dimension of column value; greater than or
C                        equal to zero
C      NCOUNT  I         Number of array indices in first block;
C                        greater than or equal to zero
C      BDROP   I         First array index in second block; greater
C                        than NCOUNT
C      EDROP   I         Last array index in second block
C
C   Output:
C      IRET     I        Status
C                        0 - values written
C                        1 - failed to write to output file and
C                            generated an error message.
C-----------------------------------------------------------------------
      INTEGER   DIM
      INTEGER   VALUE(DIM)
      INTEGER   NCOUNT, BDROP, EDROP, IRET
C
C     Local variables:
C
C     I         Array index
C     LAST      Last index in block
C
      INTEGER   I, LAST
C
      INCLUDE 'FORMAT.INC'
C-----------------------------------------------------------------------
C
C     Write first block:
C
      LAST = MIN (DIM, NCOUNT)
C
C     Invariant: array elements 1:I have been written out
C
      DO 10 I = 1, LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         WRITE (OFBUFF(OFPOS+1:OFPOS+OFINWD), 1000) VALUE(I)
         OFPOS = OFPOS + OFINWD
   10 CONTINUE
C
C     Write second block:
C
      LAST = MIN (DIM, EDROP)
C
C     Invariant: array elements MIN(BDROP, DIM+1):I have been written
C                out
C
      DO 20 I = MIN(BDROP, DIM+1), LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         WRITE (OFBUFF(OFPOS+1:OFPOS+OFINWD), 1000) VALUE(I)
         OFPOS = OFPOS + OFINWD
   20 CONTINUE
C
      IRET = 0
C-----------------------------------------------------------------------
 1000 FORMAT (I20)
      END
      SUBROUTINE OFWRLG (VALUE, DIM, NCOUNT, BDROP, EDROP, IRET)
C-----------------------------------------------------------------------
C   Write out elements 1:MIN(NCOUNT, DIM) and
C   MIN(BDROP, DIM+1):MIN(EDROP, DIM) of logical array VALUE.
C
C   Inputs:
C      VALUE   L(DIM)    Value of double-precision column
C      DIM     I         Dimension of column value; greater than or
C                        equal to zero
C      NCOUNT  I         Number of array indices in first block;
C                        greater than or equal to zero
C      BDROP   I         First array index in second block; greater
C                        than NCOUNT
C      EDROP   I         Last array index in second block
C
C   Output:
C      IRET     I        Status
C                        0 - values written
C                        1 - failed to write to output file and
C                            generated an error message.
C-----------------------------------------------------------------------
      INTEGER   DIM
      LOGICAL   VALUE(DIM)
      INTEGER   NCOUNT, BDROP, EDROP, IRET
C
C     Local variables:
C
C     I         Array index
C     LAST      Last index in block
C
      INTEGER   I, LAST
C
      INCLUDE 'FORMAT.INC'
C-----------------------------------------------------------------------
C
C     Write first block:
C
      LAST = MIN (DIM, NCOUNT)
C
C     Invariant: array elements 1:I have been written out
C
      DO 10 I = 1, LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         IF (VALUE(I)) THEN
            OFBUFF(OFPOS+1:OFPOS+4) = 'true'
            OFPOS = OFPOS + 4
         ELSE
            OFBUFF(OFPOS+1:OFPOS+5) = 'false'
            OFPOS = OFPOS + 5
         END IF
   10 CONTINUE
C
C     Write second block:
C
      LAST = MIN (DIM, EDROP)
C
C     Invariant: array elements MIN(BDROP, DIM+1):I have been written
C                out
C
      DO 20 I = MIN(BDROP, DIM+1), LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         IF (VALUE(I)) THEN
            OFBUFF(OFPOS+1:OFPOS+4) = 'true'
            OFPOS = OFPOS + 4
         ELSE
            OFBUFF(OFPOS+1:OFPOS+5) = 'false'
            OFPOS = OFPOS + 5
         END IF
   20 CONTINUE
C
      IRET = 0
C
      END
      SUBROUTINE OFWRTM (VALUE, DIM, NCOUNT, BDROP, EDROP, IRET)
C-----------------------------------------------------------------------
C   Write out elements 1:MIN(NCOUNT, DIM) and
C   MIN(BDROP, DIM+1):MIN(EDROP, DIM) of double-precision array VALUE as
C   times.
C
C   Inputs:
C      VALUE    D(DIM)   Value of double-precision column
C      DIM      I        Dimension of column value; greater than or
C                        equal to zero
C      NCOUNT   I        Number of array indices in first block;
C                        greater than or equal to zero
C      BDROP    I        First array index in second block; greater
C                        than NCOUNT
C      EDROP    I        Last array index in second block
C
C   Output:
C      IRET     I        Status
C                        0 - values written
C                        1 - failed to write to output file and
C                            generated an error message.
C-----------------------------------------------------------------------
      INTEGER   DIM
      DOUBLE PRECISION VALUE(DIM)
      INTEGER   NCOUNT, BDROP, EDROP, IRET
C
C     Local variables:
C
C     I         Array index
C     LAST      Last index in block
C     D         Day number
C     H         Hour number
C     M         Minute number
C     S         Seconds
C
      INTEGER   I, LAST, D, H, M
      DOUBLE PRECISION S
C
      INCLUDE 'FORMAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C
C     Write first block:
C
      LAST = MIN (DIM, NCOUNT)
C
C     Invariant: array elements 1:I have been written out
C
      DO 10 I = 1, LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         IF (VALUE(I) .EQ. DBLANK) THEN
            OFBUFF(OFPOS+1:OFPOS+10) = 'indefinite'
            OFPOS = OFPOS + 10
         ELSE
            S = VALUE(I)
            D = INT (S)
            S = 24.0D0 * (S - D)
            H = INT (S)
            S = 60.0D0 * (S - H)
            M = INT (S)
            S = 60.0D0 * (S - M)
            WRITE (OFBUFF(OFPOS+1:OFPOS+OFTMWD), 1000) D, H, M, S
            OFPOS = OFPOS + OFTMWD
         END IF
   10 CONTINUE
C
C     Write second block:
C
      LAST = MIN (DIM, EDROP)
C
C     Invariant: array elements MIN(BDROP, DIM+1):I have been written
C                out
C
      DO 20 I = MIN(BDROP, DIM+1), LAST
         IF (OFPOS .GT. 0) THEN
C
C           A separator is needed.
C
            OFBUFF(OFPOS+1:OFPOS+1) = OFSEP
            OFPOS = OFPOS + 1
         END IF
         IF (VALUE(I) .EQ. DBLANK) THEN
            OFBUFF(OFPOS+1:OFPOS+10) = 'indefinite'
            OFPOS = OFPOS + 10
         ELSE
            S = VALUE(I)
            D = INT (S)
            S = 24.0D0 * (S - D)
            H = INT (S)
            S = 60.0D0 * (S - H)
            M = INT (S)
            S = 60.0D0 * (S - M)
            WRITE (OFBUFF(OFPOS+1:OFPOS+OFTMWD), 1000) D, H, M, S
            OFPOS = OFPOS + OFTMWD
         END IF
   20 CONTINUE
C
      IRET = 0
C-----------------------------------------------------------------------
 1000 FORMAT (I3.3, '/', I2.2, ':', I2.2, ':', F4.1)
      END
