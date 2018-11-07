      SUBROUTINE UVSORT (DISKI, CNOSCI, DISKO, CNOSCO, SORT, ROTATE,
     *   CATRIO, NBUF, LUNST, JBUFSZ, BUFF1, BUFF2, IERR)
C-----------------------------------------------------------------------
C! Sorts (and rotates) one uv data file to another
C# Sort UV-util AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2001, 2006, 2008
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
C   Sorts and optionally rotates the u,v plane of one uv data file to
C   another.  Will create output file is necessary.
C      Uses the "AP" to presort blocks of data and uses a disk based
C   merge sort if necessary.
C      The contents of DCAT.INC and DUVH.INC commons are modified.
C   Input:
C      DISKI    I        Input file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO    I        Output file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      SORT    C*2       desired sort order keys:
C                        T=time, B=baseline, X=abs (U), Y=abs (V)
C      ROTATE   R        U, V rotation in deg.
C      NBUFF    I        Number of input streams to be used in the merge
C      LUN     I(NBUFF,2) LUNs for work files, each is opened NBUFF
C                        times.
C   Input/Output:
C      CNOSCO   I        Output file catalog slot number or /CFILES/
C                        scratch file number. 0 => create.
C      CATRIO   R(*)     On input the input uv file catalog header
C                        On output the output uv file cat. header; also
C                        it is updated on disk.
C      JBUFSZ   I        I/O buffer size in bytes
C      BUFF1    R(*)     I/O buffer
C      BUFF2    R(*)     I/O buffer
C   Output:
C      IERR     I        Return code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOSCI, DISKO, CNOSCO, NBUF, LUNST(NBUF,1),
     *   JBUFSZ, IERR
      CHARACTER SORT*2
      REAL      ROTATE, CATRIO(*), BUFF1(*), BUFF2(*)
C
      INTEGER   APSIZE, NSORT, JERR, LRECS, ISIZE, DISKX, CNOX, VOL(2),
     *   FILCNO(2), KEY1, KEY2, LENBU, LREC2, NF, FR(2), ICATI(256),
     *   OCATI(256), I, INDX, I1, I2, INSCR, MSGSAV, NEED, KAP
      REAL      ICATR(256), OCATR(256)
      HOLLERITH ICATH(256), OCATH(256)
      DOUBLE PRECISION ICATD(256), OCATD(256)
      LOGICAL   ONEPAS
      CHARACTER SORDER*4
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (ICATI, ICATR, ICATH, ICATD)
      EQUIVALENCE (OCATI, OCATR, OCATH, OCATD)
      DATA FR /2*2/
C                                       Recognized sort order codes
      DATA SORDER /'TBXY'/
C-----------------------------------------------------------------------
C                                       Check sort codes
      I1 = INDEX (SORDER, SORT(1:1))
      I2 = INDEX (SORDER, SORT(2:2))
      IF ((I1.LE.0) .OR. (I2.LE.0)) THEN
         MSGTXT = 'UVSORT: UNKNOWN SORT CODE: ' // SORT
         IERR = 1
         GO TO 990
         END IF
C                                       How many vis can be sorted at a
C                                       time?
      CALL RCOPY (256, CATRIO, ICATR)
      CALL RCOPY (256, CATRIO, CATBLK)
      CALL UVPGET (JERR)
C                                       Number of "AP" words per vis.
      LRECS = LREC + 2 + 3
C                                       No. words per vis. in sort.
      LREC2 = LREC + 2
C                                       No. words per buffer
      ISIZE = ((JBUFSZ / NBUF) - 2 * NBPS) / 2
      LENBU = ISIZE / LREC2
C                                       "AP" size
      NEED = NVIS * LRECS + LENBU + 20
      NEED = NEED / 1024
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = MSGSAV
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = NVIS * LRECS / 10 + LENBU + 20
         NEED = NEED / 1024
         NEED = MIN (64 * 1024, NEED) + 2
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            NEED = NVIS * LRECS / 100 + LENBU + 20
            NEED = NEED / 1024
            NEED = MIN (16 * 1024, NEED) + 2
            CALL QINIT (NEED, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               IERR = 8
               MSGTXT = 'UVSORT: UNABLE TO GET MEMORY FOR SORTING'
               GO TO 990
               END IF
            END IF
         END IF
      APSIZE = PSAPNW * 1024 - 20
      NSORT = APSIZE / LRECS
      CALL QRLSE
C                                       Can the job be done in a single
C                                       pass?
      ONEPAS = NSORT.GE.NVIS
      NSORT = MIN (NSORT, NVIS)
C                                       Create output scratch file if
C                                       necessary.
      IF ((DISKO.LE.0) .AND. (CNOSCO.EQ.0)) THEN
         CALL UVSIZE (LREC, NVIS+100, ISIZE)
         CALL SCREAT (ISIZE, BUFF2, IERR)
         CNOSCO = NSCR
         IF (IERR.GT.0) THEN
            MSGTXT = 'CREATING SCRATCH FILE'
            IF (IERR.EQ.1) MSGTXT = 'NO SPACE FOR SCRATCH FILE'
            GO TO 990
            END IF
         END IF
C                                       Set output file for first pass
      IF (ONEPAS) THEN
         DISKX = DISKO
         CNOX = CNOSCO
C                                       Horseshit for AMERGE
      ELSE
         INSCR = NSCR
C                                       No. words per vis. in sort.
         LREC2 = LREC + 2
C                                       No. words per buffer
         ISIZE = ((JBUFSZ / NBUF) - 2 * NBPS) / 2
         LENBU = ISIZE / LREC2
C                                       NSORT must be a multiple of
C                                       LENBU
         ISIZE = NSORT / LENBU
         NSORT = ISIZE * LENBU
         NSORT = MAX (NSORT, 1)
C                                       Make a couple more scratch
C                                       files.
         CALL UVSIZE (LREC2, NVIS+100, ISIZE)
         CALL SCREAT (ISIZE, BUFF2, IERR)
         VOL(1) = SCRVOL(NSCR)
         FILCNO(1) = SCRCNO(NSCR)
         DISKX = 0
         CNOX = NSCR
         IF (IERR.GT.0) THEN
            MSGTXT = 'CREATING SCRATCH FILE'
            IF (IERR.EQ.1) MSGTXT = 'NO SPACE FOR SCRATCH FILE'
            GO TO 990
            END IF
         CALL SCREAT (ISIZE, BUFF2, IERR)
         VOL(2) = SCRVOL(NSCR)
         FILCNO(2) = SCRCNO(NSCR)
         IF (IERR.GT.0) THEN
            MSGTXT = 'CREATING SCRATCH FILE'
            IF (IERR.EQ.1) MSGTXT = 'NO SPACE FOR SCRATCH FILE'
            GO TO 990
            END IF
C                                       End of preparation for merge
         END IF
C                                       First pass, read to "AP" sort
C                                       and write output.
      CALL SRTIN (DISKI, CNOSCI, DISKX, CNOX, SORT, ROTATE, ONEPAS,
     *   NSORT, LUNST, JBUFSZ, BUFF1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Merge sort if multiple passes
      IF (.NOT.ONEPAS) THEN
         KEY1 = 1
         KEY2 = 2
C                                       Merge sort
         CALL AMERGE (KEY1, KEY2, NVIS, LREC2, NSORT, NBUF, LUNST, VOL,
     *      FILCNO, LENBU, BUFF1, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Copy scratch to output
         CALL SRTOUT (VOL(1), FILCNO(1), DISKO, CNOSCO, LUNST, JBUFSZ,
     *      BUFF1, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Delete scratch files
         NF = 2
         CALL MAPCLR (NF, VOL, FILCNO, FR, BUFF1)
         NSCR = INSCR
         END IF
C                                       Fix up output CATBLK
      IF (DISKO.LE.0) THEN
         DISKX = SCRVOL(CNOSCO)
         CNOX = SCRCNO(CNOSCO)
      ELSE
         DISKX = DISKO
         CNOX = CNOSCO
         END IF
C                                       Read output CATBLK
      CALL CATIO ('READ', DISKX, CNOX, OCATI, 'REST', BUFF1, IERR)
      IF ((IERR.GT.1) .AND. (IERR.LE.3)) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
      ELSE
         IERR = 0
         END IF
C                                       Copy random parms
      OCATI(KIPCN) = ICATI(KIPCN)
      INDX = KHPTP
      DO 400 I = 1,OCATI(KIPCN)
         OCATH(INDX) = ICATH(INDX)
         INDX = INDX + 1
         OCATH(INDX) = ICATH(INDX)
         INDX = INDX + 1
 400     CONTINUE
C                                       Copy regular axes
      OCATI(KIDIM) = ICATI(KIDIM)
      INDX = KHCTP
      DO 410 I = 1,OCATI(KIPCN)
         OCATH(INDX) = ICATH(INDX)
         INDX = INDX + 1
         OCATH(INDX) = ICATH(INDX)
         INDX = INDX + 1
         OCATI(KINAX+I-1) = ICATI(KINAX+I-1)
         OCATR(KRCIC+I-1) = ICATR(KRCIC+I-1)
         OCATR(KRCRP+I-1) = ICATR(KRCRP+I-1)
         OCATR(KRCRT+I-1) = ICATR(KRCRT+I-1)
         OCATD(KDCRV+I-1) = ICATD(KDCRV+I-1)
 410     CONTINUE
C                                       No. vis.
      OCATI(KIGCN) = NVIS
C                                       Sort order
      CALL CHR2H (2, SORT, 1, OCATH(KITYP))
C                                       Rotation
      OCATR(KRCRT+JLOCD) = OCATR(KRCRT+JLOCD) + ROTATE
C                                       Units
      OCATH(KHBUN) = ICATH(KHBUN)
      OCATH(KHBUN+1) = ICATH(KHBUN+1)
C                                       Save output CATBLK
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL CATIO ('UPDT', DISKX, CNOX, OCATI, 'REST', BUFF1, IERR)
C                                       If failed try 'WRIT'
      IF (IERR.NE.0) CALL CATIO ('WRIT', DISKX, CNOX, OCATI, 'REST',
     *   BUFF1, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.GT.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1410) IERR
         GO TO 990
      ELSE
         IERR = 0
         END IF
C                                       Return output CATBLK
      CALL RCOPY (256, OCATR, CATRIO)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'UVSORT: ERROR SORTING UV DATA'
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVSORT: ERROR ',I3,' READING OUTPUT CATBLK')
 1410 FORMAT ('UVSORT: ERROR ',I3,' WRITING OUTPUT CATBLK')
      END
