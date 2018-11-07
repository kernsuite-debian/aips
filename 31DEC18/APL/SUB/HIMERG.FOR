      SUBROUTINE HIMERG (LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD,
     *   CNONEW, NOLD, NNEW, CATBLK, BUFER1, BUFER2, IERR)
C-----------------------------------------------------------------------
C! creates several new history files by merging several old ones.
C# History
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   HIMERG merges NOLD history files an copies them to NNEW new
C   history files. The merged history file consists of the whole of the
C   "master" history file, followed by the other history files
C   starting from the first line at which each differs from the master.
C   The history files are separated by comment lines noting the
C   number of lines omitted.
C   If any of the input files is unreadable, it is omitted from the
C   input list and a comment is inserted in the merged history. If
C   there is trouble writing to any of the output files, copying to
C   that file is stopped.
C   If one of the output files is also one of the (readable) input
C   files, that file is designated the master. If more that one of
C   the output files is in the input list, the last such duplicate
C   is the master; no history copy is attempted for the earlier
C   duplicate files and the program returns an error code of 2.
C   If none of the output files is in the input list, the master
C   file is the first (readable) input file.
C   If a read error is encountered while copying, the output HI
C   files are reset to their pristine state, i.e. empty for new
C   files and with the original contents for old files.
C   The task name, date, and time are entered on the new files.
C   This is a generalised version of HISCOP/HICOPY.
C   Inputs:
C      LUNOLD   I(NOLD)  LUNs for old history file.
C      LUNNEW   I(NNEW)  LUN for new history file.
C      VOLOLD   I(NOLD)  Vol. number for old history file.
C      VOLNEW   I(NNEW)  Vol. number for new history file.
C      CNOOLD   I(NOLD)  Catalog slot number of old history file.
C      CNONEW   I(NNEW)  Catalog slot number of new history file.
C      NOLD     I        Number of old history files.
C      NNEW     I        Number of new history files.
C   In/Out:
C      CATBLK   I(256,NNEW)   Catalog header of map for new file.
C      BUFER1   I(256,NOLD)   Work buffer, used for old files.
C      BUFER2   I(256,NNEW)   Work buffer, new file; must be used
C                             in further HIADD calls until file
C                             is closed.
C   Output:
C      IERR     I        Return error code: 0 => OK.
C                           1 => could not open old history file.
C                           2 => could not copy old history file.
C                           3 => could not write time on new file
C                           4 => could not create/open new HI file.
C                           5 => Two or more output files the same.
C                           6 => Wrong number of input files.
C   NOTE: IERR < 3 is a warning only, = 3 serious, = 4 a real problem.
C   Calling programs should ignore IERR < 3, branch to HICLOS of the
C   new HI file on IERR = 3, and skip over all HI stuff on IERR = 4.
C   Errors 5 and 6 should not occur in working programs.
C-----------------------------------------------------------------------
      INTEGER   NOLD, NNEW, LUNOLD(*), LUNNEW(*), VOLOLD(*), VOLNEW(*),
     *   CNOOLD(*), CNONEW(*), CATBLK(256,*), BUFER1(256,*),
     *   BUFER2(256,*), IERR
C
      CHARACTER HILINE*72, CTIME*8, CDATE*12
      INTEGER   IER, TIME(3), DATE(3), JERR, WERR, RERR, FIRST, FIRSTN
      INTEGER   IPTRO(10), IPTRN(10), ITEMP(10)
      INTEGER   MREC(10), IND(10), NREC, NC, I, J, K, OLDNEW, NGOOD
      INTEGER   SAMEAS(10), ISOLD(10), NSAME(10), LIMIT
      LOGICAL   F, BADOLD(10), DOADD, DOCOP, HILOST(10)
      PARAMETER (F = .FALSE.)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OLDNEW = 0
      FIRST = 1
      FIRSTN = 0
      NGOOD = 0
C                                       Check inputs
      IF (NNEW.LE.0 .OR. NOLD.LE.0 .OR. (NNEW+NOLD).GT.10) THEN
         IERR = 6
         GO TO 999
         END IF
C                                       Check to see if any of the
C                                       output files are the same
C                                       (Major error).
      DO 10 I = 1, NNEW
         ISOLD(I) = 0
         HILOST(I) = .FALSE.
         DO 5 J = 1, I-1
            IF (VOLNEW(I).EQ.VOLNEW(J) .AND.
     *          CNONEW(I).EQ.CNONEW(J)) THEN
               IERR = 5
               WRITE (MSGTXT,1040) I,J
               CALL MSGWRT (7)
               GO TO 999
               END IF
 5          CONTINUE
 10      CONTINUE
C                                      Loop through input files
C                                      doing checks & startup
      DO 40 I = 1, NOLD
         SAMEAS(I) = 0
         NSAME(I) = 0
         BADOLD(I) = .FALSE.
C                                       Check to see if any of the
C                                       input files are the same
         DO 15 J = 1,I-1
            IF (SAMEAS(J) .NE. 0) GO TO 15
            IF (VOLOLD(I).EQ.VOLOLD(J) .AND.
     *          CNOOLD(I).EQ.CNOOLD(J)) THEN
               SAMEAS(I) = J
               IF (I .EQ. FIRST) FIRST = FIRST + 1
               GO TO 40
               END IF
 15         CONTINUE
C                                       Not the same as any previous
C                                       input file:
C                                       Open file
         CALL HIOPEN (LUNOLD(I), VOLOLD(I), CNOOLD(I),
     *      BUFER1(1,I), IER)
         CALL HILOCT ('SRCH', LUNOLD(I), IPTRO(I), JERR)
         IF (IER.EQ.0 .AND. JERR.EQ.0) THEN
C                                       loop limits for copy:
            MREC(I) = HITAB(IPTRO(I)+2)
            IND(I)  = HITAB(IPTRO(I)+1)
            IPTRO(I) = IPTRO(I) + 6
            NGOOD = NGOOD + 1
         ELSE
C                                       File should, but doesn't exist
            WRITE (MSGTXT,1020) I
            CALL MSGWRT (7)
            BADOLD(I) = .TRUE.
            SAMEAS(I) = -1
            IF (I .EQ. FIRST) FIRST = FIRST + 1
            IERR = 1
            END IF
C                                       Are any output files same as
C                                       current input file ?
         DO 20 J = 1,NNEW
            IF (VOLOLD(I).EQ.VOLNEW(J) .AND.
     *          CNOOLD(I).EQ.CNONEW(J)) THEN
C                                       Was there trouble opening file?
               IF (BADOLD(I)) THEN
                  IF (IER.NE.5) THEN
C                                       Can't get to output file, quit:
                     IERR = 4
                     GO TO 999
                     END IF
C                                       Maybe we can make a new HI file
C                                       Leave in output list & flag lost.
                  HILOST(J) = .TRUE.
               ELSE
C                                       No trouble, make this master
C                                       file.
                  ISOLD(J) = I
                  FIRST = I
                  FIRSTN = J
                  OLDNEW = OLDNEW + 1
                  IF (OLDNEW .GE. 2) IERR = 2
                  END IF
               GO TO 40
               END IF
 20         CONTINUE
 40      CONTINUE
C                                       create new HI files
      DO 50 I = 1, NNEW
         IF (ISOLD(I) .GT. 0) GO TO 50
         CALL HICREA (LUNNEW(I), VOLNEW(I), CNONEW(I),
     *      CATBLK(1,I), BUFER2(1,I), IER)
         CALL HILOCT ('SRCH', LUNNEW(I), IPTRN(I), JERR)
         IF (IER.NE.0 .OR. JERR.NE.0) THEN
            IERR = 4
            GO TO 999
            END IF
         ITEMP(I) = BUFER2(2,I)
 50      CONTINUE
C
         IF (NGOOD .LE. 0) THEN
C                                       Nothing to copy from
            WRITE(MSGTXT,1050)
            CALL MSGWRT (7)
            GO TO 125
            END IF
         IF (NNEW.EQ.OLDNEW .AND. NGOOD.LE.1) THEN
C                                       Nothing to copy to
            WRITE(MSGTXT,1060)
            CALL MSGWRT (7)
            GO TO 125
            END IF
C                                       copy old to new:
C                                       Initialize WRITE errors
      WERR = 0
C                                       loop adding MREC lines
      LIMIT = MREC(FIRST)
      DO 100 I = 1, LIMIT
C                                       Check we havn't run out
C                                       of data on other files:
         DO 70  J = 1, NOLD
            IF (SAMEAS(J).NE.0) GO TO 70
            IF (MREC(J) .LT. I) THEN
               SAMEAS(J) = -1
               NSAME(J) = MREC(J)
               END IF
 70         CONTINUE
C                                       Find record number for
C                                       current line.
         NREC = (I-1) / NHILPR + 1
C                                       Read input if required
         IF (NREC .NE. HITAB(IPTRO(FIRST))) THEN
            DO 75 J = 1,NOLD
               IF (SAMEAS(J) .NE. 0) GO TO 75
               HITAB(IPTRO(J)) = NREC
               CALL ZFIO ('READ', LUNOLD(J), IND(J), NREC,
     *            BUFER1(1,J), RERR)
               IF (RERR .NE. 0) THEN
                  WRITE(MSGTXT,1900) RERR, J
                  CALL MSGWRT(7)
                  BADOLD(J) = .TRUE.
                  SAMEAS(J) = -1
                  IF (J .EQ. FIRST) GO TO 125
                  END IF
 75            CONTINUE
            END IF
C                                       Work out offset in BUFER1
         NC = 4 + NHIWPL * MOD (I-1, NHILPR)
C                                       Compare lines
         DO 90 J = 1, NOLD
            IF (J.EQ.FIRST .OR. SAMEAS(J).NE.0) GO TO 90
            DO 80 K = 1,NHIWPL
               IF (BUFER1(NC+K,FIRST) .NE. BUFER1(NC+K,J)) THEN
                  NSAME(J) = I - 1
                  SAMEAS(J) = -1
                  END IF
 80            CONTINUE
 90         CONTINUE
C                                       Add output
         CALL H2CHR (72, 1, BUFER1(NC+1,FIRST), HILINE)
         CALL HIADDN (LUNNEW, NNEW, HILINE, BUFER2,
     *                ISOLD, WERR)
 100     CONTINUE
C
      IF (FIRSTN .GE. 1) THEN
C                                       Can now copy rest of
C                                       history to old output map:
C                                       Close first input & reopen
C                                       as output
         CALL HICLOS (LUNOLD(FIRST), F, BUFER1(1,FIRST), JERR)
         IF (JERR .NE. 0) THEN
            WRITE(MSGTXT,1080)
            CALL MSGWRT(7)
            END IF
         SAMEAS(FIRST) = FIRST
         ISOLD(FIRSTN) = 0
         CALL HIOPEN (LUNNEW(FIRSTN), VOLNEW(FIRSTN), CNONEW(FIRSTN),
     *               BUFER2(1,FIRSTN), IER)
         CALL HILOCT ('SRCH', LUNNEW(FIRSTN), IPTRN(FIRSTN), JERR)
         IF (IER.NE.0 .OR. JERR.NE.0) THEN
            IERR = 4
            GO TO 999
            END IF
         ITEMP(FIRSTN) = BUFER2(2,FIRSTN)
         END IF
C                                       Mark end of first history
C                                       file
      WRITE(HILINE,1210) TSKNAM, FIRST
      CALL HIADDN (LUNNEW, NNEW, HILINE, BUFER2,
     *             ISOLD, WERR)
      IF (FIRST .NE. 1) THEN
         IF (FIRSTN .NE. 0) THEN
C                                       Note funny order.
            WRITE(HILINE,1260) TSKNAM
            CALL HIADDN (LUNNEW, NNEW, HILINE, BUFER2, ISOLD, WERR)
         ELSE
C                                       Note missing files
            WRITE(HILINE,1200) TSKNAM, FIRST - 1
            CALL HIADDN (LUNNEW, NNEW, HILINE, BUFER2, ISOLD, WERR)
            END IF
         END IF
C
C                                       Now copy the rest:
C
      DO 120 I = 1, NOLD
         IF (I .EQ. FIRST) GO TO 120
C                                       Write beginning delimiter
C                                       into history file.
         WRITE(HILINE,1220) TSKNAM, I
            CALL HIADDN (LUNNEW, NNEW, HILINE, BUFER2, ISOLD, WERR)
C                                       Check for any left-over
C                                       duplicates.
         IF (SAMEAS(I).EQ.0 .AND. MREC(I).GT.MREC(FIRST)) THEN
            SAMEAS(I) = -1
            NSAME(I) = MREC(FIRST)
            END IF
C                                       Check for special comments
         DOADD = .TRUE.
         DOCOP = .FALSE.
         IF (SAMEAS(I) .GT. 0) THEN
C                                       Note duplicate files in
C                                       history.
            WRITE(HILINE,1230) TSKNAM, I, SAMEAS(I)
         ELSE IF (SAMEAS(I) .EQ. 0) THEN
C                                       Different files with same
C                                       history? Very strange.
            WRITE(HILINE,1250) TSKNAM, I
         ELSE IF (BADOLD(I)) THEN
            WRITE(HILINE,1270) TSKNAM, I
         ELSE IF (NSAME(I) .GT. 0) THEN
C                                       Note duplicate lines in
C                                       history (if any)
            DOCOP = .TRUE.
            WRITE(HILINE,1240) TSKNAM, NSAME(I), I
         ELSE
C                                       Just copy history.
            DOADD = .FALSE.
            DOCOP= .TRUE.
            END IF
C                                       Actually write the comment.
         IF (DOADD) CALL HIADDN (LUNNEW, NNEW, HILINE, BUFER2,
     *                           ISOLD, WERR)
C
         IF (.NOT. DOCOP) GO TO 115
C                                       Copy rest of history file.
         DO 110 J = NSAME(I)+1, MREC(I)
            NREC = (J-1) / NHILPR + 1
C                                       Read input if required
            IF (NREC .NE. HITAB(IPTRO(I))) THEN
               HITAB(IPTRO(I)) = NREC
               CALL ZFIO ('READ', LUNOLD(I), IND(I), NREC,
     *            BUFER1(1,I), RERR)
               IF (RERR .NE. 0) THEN
                  WRITE(MSGTXT,1900) RERR, J
                  CALL MSGWRT(7)
                  BADOLD(I) = .TRUE.
                  GO TO 125
                  END IF
               END IF
C                                       Work out offset in BUFER1
            NC = 5 + NHIWPL * MOD (J-1, NHILPR)
C                                       Add output
            CALL H2CHR (72, 1, BUFER1(NC,I), HILINE)
            CALL HIADDN (LUNNEW, NNEW, HILINE, BUFER2,
     *                   ISOLD, WERR)
 110        CONTINUE
C                                       Write ending delimiter
C                                       into history file.
 115     WRITE(HILINE,1210) TSKNAM, I
         CALL HIADDN (LUNNEW, NNEW, HILINE, BUFER2,
     *                ISOLD, WERR)

 120     CONTINUE
C                                       End of History copy.
C                                       Check for write errors:
 125  IF (WERR .NE. 0) IERR = 2
C                                       Discard input if bad read.
      IF (RERR .NE. 0) THEN
         IERR = 2
         DO 130 I = 1,NNEW
            HITAB(IPTRN(I)+2) = 0
            HITAB(IPTRN(I)+3) = ITEMP(I)
            HITAB(IPTRN(I)+6) = 1
            BUFER2(1,I) = 0
            BUFER2(2,I) = ITEMP(I)
 130        CONTINUE
         WRITE (MSGTXT,1070)
         CALL MSGWRT(7)
         END IF
C                                       close old history files
      DO 140 I = 1,NOLD
         IF (SAMEAS(I) .NE. 0) GO TO 140
         CALL HICLOS (LUNOLD(I), F, BUFER1(1,I), JERR)
         IF (JERR .NE. 0) THEN
            WRITE(MSGTXT,1080)
            CALL MSGWRT(7)
            END IF
 140     CONTINUE
C                                       open existing "new" files
 150  DO 160 J = 1,NNEW
         IF (ISOLD(J) .GT. 0) THEN
            CALL HIOPEN (LUNNEW(J), VOLNEW(J), CNONEW(J),
     *         BUFER2(1,J), IER)
            IF (IER .NE. 0) THEN
               IERR = 4
               GO TO 999
               END IF
            WRITE (HILINE,1300) TSKNAM
            CALL HIADD(LUNNEW(J), HILINE, BUFER2(1,J), WERR)
         ELSE IF (HILOST(J)) THEN
C                                       This was an existing file
C                                       whose old HI file was lost.
C                                       Put warning in HI File
            WRITE (HILINE,1280) TSKNAM
            CALL HIADD(LUNNEW(J), HILINE, BUFER2(1,J), WERR)
            WRITE (HILINE,1290) TSKNAM
            CALL HIADD(LUNNEW(J), HILINE, BUFER2(1,J), WERR)
            IF (WERR .NE. 0) THEN
               WRITE (MSGTXT,1910) WERR, J
               CALL MSGWRT(7)
               END IF
            END IF
 160     CONTINUE
C                                       Write time / date on new files
 200  CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1100) TSKNAM, RLSNAM, CDATE, CTIME
      DO 210 I = 1,NNEW
         CALL HIADD (LUNNEW, HILINE, BUFER2, IER)
         IF (IER .NE. 0) IERR = 3
 210     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('UNABLE TO OPEN OLD HISTORY FILE #',I3)
 1040 FORMAT ('OUTPUT FILES',I3,' AND',I3,' ARE IDENTICAL.')
 1050 FORMAT ('UNABLE TO OPEN ANY OLD HISTORY FILES')
 1060 FORMAT ('ALL OUTPUT FILES ARE OLD: NO HISTORY COPIED')
 1070 FORMAT ('UNABLE TO COPY OLD HISTORY TO SOME NEW FILE(S)')
 1080 FORMAT ('TROUBLE CLOSING OLD HISTORY FILE(S)')
 1100 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1200 FORMAT (A6,'/****** Could not open history files for first',
     *   ' input files.')
 1210 FORMAT (A6,'/****** End of history file for input file #',I3)
 1220 FORMAT (A6,'/****** Begin history file for input file #',I3,':')
 1230 FORMAT (A6,'/ Input file #',I3,' is the same as input file #',I3)
 1240 FORMAT (A6,'/ First',I7,' history lines of file',I3,
     *   ' are same as first file.')
 1250 FORMAT (A6,'/ History of file',I3,' is identical to first file')
 1260 FORMAT (A6,'/****** Out of order because this file is being',
     *   ' overwritten.')
 1270 FORMAT (A6,'/****** Could not open history file for file',I3)
 1280 FORMAT (A6,'/****** WRITING TO EXISTING FILE BUT CAN''T FIND',
     *        ' OLD HISTORY FILE:')
 1290 FORMAT (A6,'/****** NEW HISTORY FILE WAS MADE FROM INPUT FILES')
 1300 FORMAT (A6,'/****** NO HISTORY COPIED: OUTPUT FILE IS OLD')
 1900 FORMAT ('HIMERG READ ERROR',I7,' ON INPUT FILE',I3)
 1910 FORMAT ('HIMERG ADD ERROR',I7,' ON OUTPUT FILE',I3)
      END
