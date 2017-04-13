LOCAL INCLUDE 'PRTTP.INC'
      REAL      DOCRT
      CHARACTER PRTEXT*132, TITL1*132, TITL2*132, CBUF*2880, TAPNAM*12,
     *   INFILE*48
      INTEGER   IBUF(4096), NFILES, LTYPE, FDVEC(50), TBIND, PLUN, PIND,
     *   NLINES, INTAPE, IPAGE, IPTLEV, NCR, TAPBUF(29184), BITPIX,
     *   NRECS, IDOEOT, UNKNWN, IEXTRA, NPIECE, XERR, FNRECS, NPV(4)
      LOGICAL   RQUICK, SHORT, VLBA, UVTABL, FILE1, DODISK, VLDISK
      DOUBLE PRECISION BSCALE(2), BZERO(2), BYTES, FBYTES
      COMMON /PRTTAP/ BSCALE, BZERO, BYTES, FBYTES, DOCRT, TAPBUF, IBUF,
     *   BITPIX, RQUICK, SHORT, VLBA, NFILES, LTYPE, PLUN, PIND, NLINES,
     *   INTAPE, IPAGE, IPTLEV, NCR, FDVEC, TBIND, NRECS, IDOEOT, FILE1,
     *   UNKNWN, DODISK, UVTABL, IEXTRA, NPIECE, XERR, FNRECS, VLDISK,
     *   NPV
      COMMON /PRTPCH/ PRTEXT, TITL1, TITL2, CBUF, TAPNAM, INFILE
LOCAL END
LOCAL INCLUDE 'VLARCH.INC'
C                                       Commons for VLA archive tapes
C                                       Index info
      CHARACTER PGIDA(2000)*8, DATEA(2000)*8, CORMA(2000)*4
      INTEGER   D10IDA(2000), SUBAA(2000), CHCODA(2000)
      LOGICAL   FULL
      INTEGER   NOPGM, NOREC(2000), NOVIS(2000)
      DOUBLE PRECISION    FREQAA(2000), TIMERA(2,2000)
      COMMON /TAPE/ FREQAA, TIMERA, NOPGM, NOREC, NOVIS, FULL, D10IDA,
     *   SUBAA, CHCODA
      COMMON /VLACH/ PGIDA, CORMA, DATEA
LOCAL END
      PROGRAM PRTTP
C-----------------------------------------------------------------------
C! PRTTP prints a summary of contents of user's tape.
C# Tape Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2001-2004, 2007, 2009-2010, 2012, 2015-2016
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
C   PRTTP is an AIPS task to print on the line printer a summary of
C   the contents of a user's tape.  The program recognizes the
C   following formats:
C   (1) VLA visibility data "export" format
C   (2) IBM (Charlottesville) package map format
C   (3) FITS standard array format, including any following standard
C       TABLES format blocks
C   (4) FITS group format (i.e. UV data)
C   (5) VLBA binary FITS format
C   Inputs adverb:
C      INTAPE     R       Tape drive number: < 1 => 1
C      NFILES     R       Number of files to advance from begin of tape
C      PRTLEV     R       Print level control: (FITS only now)
C                           -1 => very brief summary
C                            0 => only summary
C                            1 => summary, non-history
C                            2 => everything
C      DOCRT      R       > 0 => use CRT, else line printer
C-----------------------------------------------------------------------
      INTEGER   IRET, NWD, IERR
      INCLUDE 'PRTTP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Init things
      CALL PRTPIN (IRET)
      IF (IRET.NE.0) GO TO 995
      UNKNWN = 0
      NWD   = 29184
      NWD = (NWD * NBITWD) / 8
C                                       Loop point: what format?
 10   IF (FNRECS.GT.0) THEN
         IF ((FNRECS.GT.1) .OR. (FBYTES.GT.512.0D0)) THEN
            FBYTES = FBYTES / (1024.0D0 * 1024.0D0)
            WRITE (MSGTXT,1991) FBYTES, FNRECS
            CALL MSGWRT (4)
            CALL PRTPLI (4, IERR)
            END IF
         FNRECS = 0
         FBYTES = 0.0D0
         END IF
C                                       What is next?
      FDVEC(3) = NWD
      CALL PRTPHD (IERR)
      IRET = IERR
      IF ((IERR.EQ.4) .AND. (NFILES.GT.0)) IRET = 0
      IF ((IERR.EQ.6) .AND. (NFILES.GT.0)) IRET = 0
      IF (IERR.NE.0) GO TO 990
C                                       Do the file
         NFILES = NFILES + 1
         IF (DOCRT.LE.0.0) THEN
            WRITE (MSGTXT,1010) NFILES
            CALL MSGWRT (2)
            END IF
         IF (LTYPE.EQ.1) CALL PRTFIT (IRET)
         IF (LTYPE.EQ.2) CALL PRTIBM (IRET)
         IF (LTYPE.EQ.3) CALL PRTEXP (IRET)
         IF (LTYPE.EQ.4) CALL PRTVLA (IRET)
         IF (LTYPE.EQ.5) CALL PRTXXX (IRET)
         IF ((IRET.EQ.0) .AND. (.NOT.DODISK)) GO TO 10
C                                       Close the files
 990  IF (VLDISK) THEN
         CALL FLDKIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
      ELSE
         CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
         END IF
      IF (UNKNWN.GT.0) THEN
         WRITE (MSGTXT,1990) UNKNWN
         CALL MSGWRT (6)
         CALL PRTPLI (4, IERR)
         END IF
      IF (FNRECS.GT.0) THEN
         IF ((FNRECS.GT.1) .OR. (FBYTES.GT.512.0D0)) THEN
            FBYTES = FBYTES / (1024.0D0 * 1024.0D0)
            WRITE (MSGTXT,1991) FBYTES, FNRECS
            CALL MSGWRT (4)
            CALL PRTPLI (4, IERR)
            END IF
         FNRECS = 0
         FBYTES = 0.0D0
         END IF
      BYTES = BYTES / (1024.0D0 * 1024.0D0)
      WRITE (MSGTXT,1992) BYTES, NRECS
      CALL MSGWRT (4)
      CALL PRTPLI (4, IERR)
      IF (IPTLEV.GT.-3) THEN
         CALL LPCLOS (PLUN, PIND, NLINES, IERR)
      ELSE
         CALL ZTXCLS (PLUN, PIND, IERR)
         END IF
C                                       Close the task
 995  IF (IRET.EQ.-1) IRET = 0
      CALL DIETSK (IRET, RQUICK, IBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('Begin reading file',I6)
 1990 FORMAT ('WARNING:',I7,' unknown records skipped while reading',
     *   ' tape')
 1991 FORMAT ('This file: read',F10.3,' Megabytes in',I8,
     *   ' logical records')
 1992 FORMAT ('Read at least',F11.3,' Megabytes in',I8,
     *   ' logical records')
      END
      SUBROUTINE PRTPIN (IRET)
C-----------------------------------------------------------------------
C   PRTPIN inits the task PRTTP including calling ZDCHIN and GTPARM,
C   resuming AIPS if requested, opening the printer and tape, and
C   rewinding the tape.  Note that the tape is opened as a "map"
C   file.
C   Output: IRET  I    Return code: 0 => ok, else quit directly
C-----------------------------------------------------------------------
      INTEGER IRET
C
      CHARACTER PRGNAM*6, LPNAME*48
      HOLLERITH XLPNAM(12), XTPNAM(3)
      INTEGER   IERR, IROUND, NP, I, ITRIM, ITEMP(25)
      HOLLERITH  XINFIL(12)
      REAL      TAPEIN, XFILES, PRTLEV, DDCRT, XBFILE, XDOEOT, XPIECE,
     *   XERROR
      INCLUDE 'PRTTP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /INPARM/ TAPEIN, XFILES, XINFIL, PRTLEV, DDCRT, XLPNAM,
     *   XTPNAM, XBFILE, XDOEOT, XPIECE, XERROR
      DATA PRGNAM /'PRTTP '/
C-----------------------------------------------------------------------
C                                       Init for AIPS
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NP = 35
      CALL GTPARM (PRGNAM, NP, RQUICK, TAPEIN, IBUF, IRET)
      IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      IF (IRET.NE.0) THEN
         RQUICK = .TRUE.
         IF (IRET.EQ.1) GO TO 999
         GO TO 20
         END IF
      RQUICK = RQUICK .AND. (DDCRT.LE.0.0)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      DODISK = INFILE.NE.' '
      IF (((NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000)) .OR. DODISK)
     *   GO TO 20
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         IRET = 8
         RQUICK = .TRUE.
 20   IF (RQUICK) CALL RELPOP (IRET, IBUF, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Init parms
      IRET = 8
      IPAGE = 0
      CALL H2CHR (12, 1, XTPNAM, TAPNAM)
      DOCRT = DDCRT
      INTAPE = IROUND (TAPEIN)
      IF (INTAPE.LE.0) INTAPE = 1
      NFILES = IROUND (XFILES)
      IF (NFILES.LE.0) NFILES = 0
      IPTLEV = IROUND (PRTLEV)
      IF (IPTLEV.LE.-3) DOCRT = -1
      NLINES = 900
      NRECS = 0
      BYTES = 0.0D0
      FNRECS = 0
      FBYTES = 0.0D0
      IF (XDOEOT.LT.0.0) XDOEOT = 0.0
      IDOEOT = XDOEOT + 1.9
      NPIECE = IROUND (XPIECE)
      IF (.NOT.DODISK) NPIECE = 1
      XERR = IROUND (XERROR)
C                                       Open tape
C                                       Initialize FDVEC
      CALL FILL (50, 0, FDVEC)
      FDVEC(2) = 2880
      FDVEC(3) = (29184 * NBITWD) / 8
      FDVEC(5) = INTAPE
      CALL CHR2H (48, INFILE, 1, FDVEC(7))
C                                       Disk
      VLDISK = .FALSE.
      IF (DODISK) THEN
         INTAPE = 1
C                                       Try VLA archive first
         FDVEC(1) = 29
         FDVEC(2) = 2048 * 13
         FDVEC(33) = MAX (0, NFILES) + 1
C                                       Open tape
         MSGSUP = 32000
         CALL FLDKIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
         MSGSUP = 0
C                                       Check VLA MC Archive
         IF (IERR.EQ.0) THEN
            CALL FLDKIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.EQ.0) THEN
               CALL ZI16IL (25, 1, TAPBUF(TBIND), ITEMP)
C                                       ITEMP(1) = block number
C                                       ITEMP(2) = number of blocks
               IF ((ITEMP(1).EQ.1) .AND. (ITEMP(2).GE.1) .AND.
C                                       ITEMP(5) = format (1 only)
     *            (ITEMP(5).EQ.1) .AND.
C                                       ITEMP(6) = format revision no.
     *            (ITEMP(6).GE.3) .AND. (ITEMP(6).LE.50) .AND.
C                                       ITEMP(20) = no. antennas
     *            (ITEMP(20).GE.0) .AND. (ITEMP(20).LE.29)) THEN
                  VLDISK = .TRUE.
                  DODISK = .FALSE.
               ELSE
                  CALL FLDKIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
                  END IF
               END IF
            END IF
         IF ((IERR.GT.0) .AND. (IERR.NE.8)) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Okay - try FITS
         IF (.NOT.VLDISK) THEN
            FDVEC(1) = 25
            FDVEC(2) = 2880
            FDVEC(5) = INTAPE
            FDVEC(33) = 0
            MSGSUP = 32000
            CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
            MSGSUP = 0
            IF (IERR.EQ.0) THEN
               CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
            ELSE
               I = ITRIM (INFILE)
               INFILE(I+1:) = '1'
               CALL CHR2H (48, INFILE, 1, FDVEC(7))
               END IF
            END IF
      ELSE
         FDVEC(1) = 129 - INTAPE
         FDVEC(6) = 10
         END IF
      IF (.NOT.VLDISK) THEN
         CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
C                                       Rewind the tape
      IF (.NOT.((DODISK) .OR. (VLDISK))) THEN
         IF (NFILES.LE.10000) THEN
            FILE1 = .TRUE.
            I = 1
            CALL ZTAPE ('REWI', FDVEC(1), FDVEC(40), I, IERR)
            IF ((IERR.EQ.0) .AND. (NFILES.GT.0)) THEN
               CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), NFILES, IERR)
               FILE1 = .FALSE.
               END IF
C                                       Back to start current file
         ELSE
            FILE1 = .FALSE.
            CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
            END IF
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
         END IF
C                                       Open printer
      IF (NFILES.GT.10000) THEN
         NFILES = XBFILE - 0.99
         IF (NFILES.LT.1) NFILES = 10000
         IF (VLDISK) NFILES = 0
         END IF
      IF (DODISK) NFILES = 0
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (IPTLEV.GT.-3) THEN
         IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
         CALL LPOPEN (LPNAME, DOCRT, PLUN, PIND, NCR, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR, 'LPOPEN'
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                       open text file instead
      ELSE
         IF (LPNAME.EQ.' ') THEN
            LPNAME = 'PRTTPout.txt'
            MSGTXT = 'WARNING: OUTPRINT=''PRTTPout.txt'' USED' //
     *         ' - BLANK NOT ALLOWED'
            CALL MSGWRT (7)
            END IF
         NCR = 132
         PLUN = 3
         CALL ZTXOPN ('WRIT', PLUN, PIND, LPNAME, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR, 'ZTXOPN'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         END IF
      SHORT = NCR.LE.79
      IRET = 0
      GO TO 999
C                                       Close tape on error
 990  CALL TAPIO ('CLOS' ,FDVEC, TAPBUF, TBIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I7,' TRYING TO OPEN PSEUDO-TAPE DISK FILE')
 1010 FORMAT ('TAPES NOT ALLOWED IN BATCH JOBS')
 1020 FORMAT ('ERROR',I7,' TRYING TO OPEN TAPE')
 1030 FORMAT ('ERROR',I7,' TRYING TO POSITION TAPE FOR START OF JOB')
 1040 FORMAT ('ERROR',I7,' FROM ',A,' TRYING TO OPEN OUTPUT/PRINTER')
      END
      SUBROUTINE PRTPHD (IERR)
C-----------------------------------------------------------------------
C   PRTPHD reads a tape header.  It tests it for being a FITS header by
C   looking for 'SIMPLE  = '.  If that fails it checks for tape label
C   marks 'VOL1', 'HDR1', etc.  If they are found it advances file and
C   repeats.  If not, it tests for a VLA UV-data export tape by checking
C   the block length word and for the character string 'RUN '.  Failing
C   that it checks for an IBM map header by checking axis dimensions.
C   A set of somewhat arbitrary criteria are used for VLA archive
C   tapes.
C   Outputs:
C   Common  LTYPE I        1 => FITS,  2 => IBM map,  3 => UV export,
C                          4 => VLA archive
C   Variab  IERR  I        error return: 0 => ok
C                          OTHER = return from TAPIO
C                          10 = neither FITS nor DEC nor Export
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER LINE*80, HDRTYP(7)*4
      INTEGER   IEREOF, I(2), IT, JERR, II, ITRY, ITEMP(25), J, IEREOM,
     *   NEOF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PRTTP.INC'
      DATA IEREOF, IEREOM /4, 6/
      DATA HDRTYP /'VOL1','HDR1','HDR2','EOF1','EOF2','EOV1','EOV2'/
C-----------------------------------------------------------------------
      ITRY = 1
      IF (VLDISK) GO TO 50
 10   FDVEC(2) = 2880
      FDVEC(6) = 10
C                                       suppress messages
C                                       read one record
      FDVEC(31) = 0
      FDVEC(32) = 0
      IF (FILE1) IDOEOT = IDOEOT + 1
      IF (DODISK) IDOEOT = 1
      DO 12 NEOF = 1,IDOEOT
         MSGSUP = 32000
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         MSGSUP = 0
         IF (IERR.NE.4) GO TO 13
         NFILES = NFILES + 1
         WRITE (MSGTXT,1010) NFILES
         CALL MSGWRT (6)
         CALL PRTPLI (4, IERR)
         IF (IERR.NE.0) GO TO 999
         IERR = 4
 12      CONTINUE
 13   IF (FILE1) IDOEOT = IDOEOT - 1
      FILE1 = .FALSE.
      IF ((IERR.NE.0) .AND. (IERR.NE.10)) GO TO 980
      NRECS = NRECS + 1
      BYTES = BYTES + FDVEC(42)
      FNRECS = FNRECS + 1
      FBYTES = FBYTES + FDVEC(42)
C                                       FITS ?
      CALL ZC8CL (80, 1, TAPBUF(TBIND), LINE)
      IF (LINE(1:10).EQ.'SIMPLE  = ') THEN
         LTYPE = 1
         IF (IERR.EQ.10) THEN
            IERR = 0
            LTYPE = -1
         ELSE
            IT = INDEX (LINE, '/')
            IF (IT.LE.0) IT = 80
            DO 15 J = 11,IT
               IF (LINE(J:J).NE.' ') THEN
                  IF (LINE(J:J).NE.'T') GO TO 16
                  GO TO 970
                  END IF
 15            CONTINUE
 16         MSGTXT = 'NON-SIMPLE FITS HEADER: TRY TO PROCEDE ANYWAY'
            CALL MSGWRT (6)
            CALL PRTPLI (4, IERR)
            GO TO 999
            END IF
         GO TO 970
         END IF
      IF (DODISK) THEN
         MSGTXT = 'DISK FILES MUST BE FITS FORMAT'
         CALL MSGWRT (8)
         IERR = 99
         GO TO 999
         END IF
      IERR = 0
C                                       Check for labeled tape
      DO 20 IT = 1,7
         IF (LINE(1:4).EQ.HDRTYP(IT)) GO TO 30
 20      CONTINUE
      GO TO 50
C                                       Tape labels
 30   CONTINUE
         IF (FNRECS.GT.0) THEN
            IF ((FNRECS.GT.1) .OR. (FBYTES.GT.512.0D0)) THEN
               FBYTES = FBYTES / (1024.0D0 * 1024.0D0)
               WRITE (MSGTXT,1991) FBYTES, FNRECS
               CALL MSGWRT (4)
               CALL PRTPLI (4, IERR)
               END IF
            FNRECS = 0
            FBYTES = 0.0D0
            END IF
         NFILES = NFILES + 1
         CALL PRTPLI (1, IERR)
         IF (IERR.NE.0) GO TO 999
         DO 35 II = 1,9
            CALL ZC8CL (80, 1, TAPBUF(TBIND), CBUF)
            WRITE (PRTEXT,1030) ' '
            PRTEXT(25:104) = CBUF(1:80)
            CALL PRTPLI (2, IERR)
            IF (IERR.NE.0) GO TO 999
            MSGSUP = 32000
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            MSGSUP = 0
            NRECS = NRECS + 1
            BYTES = BYTES + FDVEC(42)
            FNRECS = FNRECS + 1
            FBYTES = FBYTES + FDVEC(42)
C                                       Watch for labels!
            IF ((IERR.NE.0) .AND. (IERR.NE.10)) GO TO 40
 35         CONTINUE
         II = 1
         CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), II, IERR)
 40      IF (IT.LT.6) THEN
            IF ((IERR.EQ.0) .OR. (IERR.EQ.IEREOF) .OR.
     *         (IERR.EQ.10)) GO TO 10
            GO TO 980
            END IF
C                                       EOVolume label
         MSGTXT = 'End of multi-tape volume found.  Mount next tape'
         IERR = 6
         GO TO 990
C                                       VLA Archive disk already known
 50   IF (VLDISK) THEN
C                                       Close previous
         CALL FLDKIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
C                                       Open tape
         FDVEC(33) = MAX (0, NFILES) + 1
         MSGSUP = 32000
         CALL FLDKIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
         MSGSUP = 0
         IF (IERR.EQ.8) THEN
            IERR = 6
            GO TO 999
            END IF
         IF (IERR.EQ.0) CALL FLDKIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR, NFILES+1
            GO TO 990
            END IF
         END IF
C                                       Check VLA MC Archive
      CALL ZI16IL (25, 1, TAPBUF(TBIND), ITEMP)
C                                       ITEMP(1) = block number
C                                       ITEMP(2) = number of blocks
      IF ((ITEMP(1).EQ.1) .AND. (ITEMP(2).GE.1) .AND.
C                                       ITEMP(5) = format (1 only)
     *   (ITEMP(5).EQ.1) .AND.
C                                       ITEMP(6) = format revision no.
     *   (ITEMP(6).GE.3) .AND. (ITEMP(6).LE.50) .AND.
C                                       ITEMP(20) = no. antennas
     *   (ITEMP(20).GE.0) .AND. (ITEMP(20).LE.29)) THEN
         LTYPE = 4
         GO TO 995
         END IF
      IF (VLDISK) THEN
         MSGTXT = 'ALL FILES MUST BE VLA EXPORT IF ANY ARE'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 999
         END IF
C                                       Must be an IBM header
C                                       Try for UV export
      LTYPE = 3
      CALL ZI16IL (1, 1, TAPBUF(TBIND), IT)
      IT = IT / 2
      IF ((IT.LE.0) .OR. (IT.GT.2004)) GO TO 60
         CALL ZC8CL (4, 9, TAPBUF(TBIND), LINE)
         IF (LINE(:4).EQ.'RUN ') GO TO 995
         IF (LINE(:4).EQ.'DEF ') GO TO 995
C                                       check bound on DEC dims
C                                       on IBM map
 60   LTYPE = 2
      CALL ZI16IL (2, 17, TAPBUF(TBIND), I)
      IF ((I(1).GE.16) .AND. (I(2).GE.16) .AND. (I(1).LE.4096) .AND.
     *   (I(2).LE.4096) .AND. (FDVEC(42).EQ.256)) GO TO 995
         IF (ITRY.GT.1) GO TO 65
            ITRY = ITRY + 1
            CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 985
            MSGTXT = 'Format not recognized: retry the tape read'
            CALL MSGWRT (6)
            CALL PRTPLI (4, IERR)
            IF (IERR.NE.0) GO TO 999
            GO TO 10
 65   CONTINUE
         IERR = 0
         LTYPE = 5
         MSGTXT = 'HEADER TYPE NOT RECOGNIZED'
         CALL MSGWRT (6)
         GO TO 995
C                                        errors
 970  IF (LTYPE.EQ.1) GO TO 999
         IF (LTYPE.EQ.-1) THEN
            WRITE (MSGTXT,1970) FDVEC(42)
            CALL MSGWRT (8)
            CALL PRTPLI (4, IERR)
            IF (IERR.EQ.0) IERR = 11
            GO TO 999
         ELSE
            MSGTXT = 'NON-SIMPLE FITS HEADER: TRY TO PROCEDE ANYWAY'
            CALL MSGWRT (6)
            CALL PRTPLI (4, IERR)
            GO TO 999
            END IF
C                                       End of tape
 980  IF (IERR.NE.0) THEN
         II = 1
         IF (IERR.EQ.IEREOF) II = 2
         IF (.NOT.(DODISK .OR. VLDISK)) THEN
            CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), II, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1980) JERR
               GO TO 990
               END IF
            END IF
         IF ((IERR.EQ.IEREOF) .OR. (IERR.EQ.IEREOM)) GO TO 999
         END IF
 985  WRITE (MSGTXT,1985) IERR
 990  CALL MSGWRT (6)
      CALL PRTPLI (4, II)
      GO TO 999
C                                       Back to start of file
 995  IF (.NOT.(DODISK .OR. VLDISK)) CALL TAPIO ('BAKF', FDVEC, TAPBUF,
     *   TBIND, IERR)
C                                       Reset read count
      FDVEC(32) = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('NULL FILE AT FILE NUMBER',I6)
 1030 FORMAT ('Read label file record: ',83X,A1)
 1050 FORMAT ('OPEN/READ VLA DISK ERROR',I3,' FILE',I3)
 1970 FORMAT ('NON-STANDARD FITS RECORD LENGTH',I7,' BYTES READ')
 1980 FORMAT ('ERROR',I6,' POSITIONING TAPE AT THE END-OF-TAPE')
 1985 FORMAT ('HEADER BAKF I/O ERROR = ',I6)
 1991 FORMAT ('This file: read',F10.3,' Megabytes in',I8,
     *   ' logical records')
      END
      SUBROUTINE PRTPLI (ICODE, IERR)
C-----------------------------------------------------------------------
C   PRTPLI handles the actual printing of lines including paging and
C   page headers via common text PRTEXT and
C   Input: ICODE  I    1 => new file header
C                      2 => double space text in PRTEXT
C                      3 => single space text in PRTEXT
C                      4 => error message display
C   Output: IERR  I    0 => continue, > 0 I/O error, -1 => please quit
C-----------------------------------------------------------------------
      INTEGER   ICODE, IERR
C
      CHARACTER BLANKS*132, SCRTCH*132, SPRTXT*132, TITL(5)*8
      INTEGER   I, LCODE, J, ITRIM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'PRTTP.INC'
      DATA TITL /'FITS FMT', 'IBM MAPS', 'EXPORT  ', 'VLA ARCH',
     *   'UNKNOWN '/
      DATA BLANKS /' '/
C-----------------------------------------------------------------------
      IERR = 0
      IF (IPTLEV.LT.-2) GO TO 999
      IF ((ICODE.LT.1) .OR. (ICODE.GT.4)) GO TO 999
      IF (ICODE.EQ.4) THEN
         IF (DOCRT.GT.0.0) GO TO 999
         IF (MSGTXT.EQ.' ') GO TO 999
         END IF
      J = (NCR - 1) / 4 + 1
C                                       New file
      IF (ICODE.LE.1) THEN
         IF ((DOCRT.GT.0.0) .AND. (NLINES.GT.ABS(CRTMAX)-6))
     *      NLINES = 900
         IF ((DOCRT.LE.0.0) .AND. (NLINES.GT.PRTMAX-8)) NLINES = 900
         WRITE (TITL1,1000) NFILES, TITL(LTYPE), TAPNAM
         WRITE (TITL2,1001) ('- - ', I = 1,J)
C                                       display file break
         IF (NLINES.LT.900) THEN
            IF (DOCRT.GT.-2.5) THEN
               CALL PRTLIN (PLUN, PIND, DOCRT, NCR, TITL1, TITL2,
     *            BLANKS, NLINES, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 999
               WRITE (SPRTXT, 1001) ('****', I = 1,33)
               IF ((DOCRT.LE.0.0) .AND. (IPTLEV.GE.0)) CALL PRTLIN
     *            (PLUN, PIND, DOCRT, NCR, TITL1, TITL2, SPRTXT, NLINES,
     *            IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 999
               IF ((DOCRT.LE.0.0) .AND. (IPTLEV.GE.0)) CALL PRTLIN
     *            (PLUN, PIND, DOCRT, NCR, TITL1, TITL2, BLANKS, NLINES,
     *            IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            CALL PRTLIN (PLUN, PIND, DOCRT, NCR, TITL1, TITL2,
     *         TITL1, NLINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (DOCRT.GT.-2.5) THEN
               CALL PRTLIN (PLUN, PIND, DOCRT, NCR, TITL1, TITL2,
     *            BLANKS, NLINES, IPAGE, SCRTCH, IERR)
               END IF
         ELSE IF (DOCRT.LE.-2.5)  THEN
            CALL PRTLIN (PLUN, PIND, DOCRT, NCR, TITL1, TITL2,
     *         TITL1, NLINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         GO TO 999
         END IF
C                                       Regular text line
      LCODE = ICODE
      IF ((IPTLEV.LT.0) .AND. (LCODE.NE.4)) LCODE = 3
      IF ((LCODE.NE.3) .AND. (DOCRT.GT.-2.5)) THEN
         CALL PRTLIN (PLUN, PIND, DOCRT, NCR, TITL1, TITL2,
     *      BLANKS, NLINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IF (LCODE.EQ.4) PRTEXT = '>>>>> ' // MSGTXT(:ITRIM(MSGTXT)) //
     *   ' <<<<<'
      CALL PRTLIN (PLUN, PIND, DOCRT, NCR, TITL1, TITL2, PRTEXT,
     *   NLINES, IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((LCODE.EQ.4) .AND. (DOCRT.GT.-2.5)) THEN
         CALL PRTLIN (PLUN, PIND, DOCRT, NCR, TITL1, TITL2,
     *      BLANKS, NLINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('****   File number',I6,4X,'of type ',A8,4X,'tape',1X,A,
     *   ' ****')
 1001 FORMAT (33A4)
      END
      SUBROUTINE PRTEXP (IERR)
C-----------------------------------------------------------------------
C   Read EXPORT tape, interpret each record type and either skip it,
C   print data, or accumulate number of VIS records.
C   Output: IERR   I     Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER IERR
C
      CHARACTER TYPES(6)*4, CH1*1, CH2*1, SOURCE*8, CTEMP*24, BTEMP*8,
     *   CCTEMP*8, IMTYPE*2, IMNAME*12, IMCLAS*6, OBS*4, DATE*12
      INTEGER   IW(4), ITYPES(2,6), IN, I, I4, IQUAL, IS, ITYP, IWT1,
     *   IWT2, IWT3, IWT4, NWD, RM(2), DM(2), ISEQ, ITEMP(8), JER, NVIS,
     *   TVIS, IRTEMP(2)
      REAL      R4, DSEC, RSEC, RTEMP(2)
      LOGICAL   FSOUR, FIRST, ENDCOM
      DOUBLE PRECISION    R8, FREQ, RA0, DEC0, F8, XDAT
      INCLUDE 'PRTTP.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (R8, I4, R4, IW(1))
      EQUIVALENCE (IRTEMP, RTEMP)
      DATA TYPES /'RUN ','ANTE','SOUR','VIS ','DEF ','END '/
C-----------------------------------------------------------------------
C                                       set logical flags
      FIRST = .TRUE.
      ENDCOM = .FALSE.
      FSOUR = .TRUE.
      NWD = 0
      IS = 20
      ISEQ = 0
      NVIS = 0
      TVIS = 0
      CALL PRTPLI (1, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       convert chars to mess on tape
      IN = -3
      DO 10 I = 1,6
         IN = IN + 4
         CALL ZCLC8 (4, TYPES(I), IN, IBUF)
 10      CONTINUE
      CALL ZI16IL (12, 1, IBUF, ITYPES)
C                                       Initialize tape reading
      FDVEC(2) = 4008
      FDVEC(6) = 1
      FDVEC(31) = 1
      FDVEC(32) = 0
C
C                                       read logical record loop
C                                       locate logical record
 50   IF (FIRST) GO TO 60
         IW(1) = IBUF(IS-4)
         IW(2) = IBUF(IS-3)
         ITYP = 7
         DO 55 I = 1,6
            IF ((IW(1).EQ.ITYPES(1,I)).AND.(IW(2).EQ.ITYPES(2,I)))
     *         ITYP = I
 55         CONTINUE
         IF (IBUF(IS-6).GT.1) GO TO 56
            WRITE (MSGTXT,1055) IBUF(IS-6)
            IERR = 4
            GO TO 980
 56      IS = IS + IBUF(IS-6)/2
C                                       read tape record ?
 60   IF ((.NOT.FIRST) .AND. (IS.LE.NWD)) GO TO 90
         MSGTXT = ' '
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         CALL PRTPLI (4, JER)
         IF (IERR.EQ.0) GO TO 70
            IF (IERR.NE.4) GO TO 65
               IERR = 0
               IF (ENDCOM) GO TO 995
               WRITE (MSGTXT,1060)
               GO TO 980
 65         CONTINUE
               WRITE (MSGTXT,1065) IERR
               GO TO 980
 70      CALL ZI16IL (1, 1, TAPBUF(TBIND), IN)
         IF (JER.NE.0) THEN
            IERR = JER
            GO TO 999
            END IF
         NRECS = NRECS + 1
         BYTES = BYTES + FDVEC(42)
         FNRECS = FNRECS + 1
         FBYTES = FBYTES + FDVEC(42)
         NWD = IN / 2
         IF ((NWD.GT.0) .AND. (NWD.LE.2004)) GO TO 75
            WRITE (MSGTXT,1070) NWD
            GO TO 980
 75      CALL ZI16IL (NWD, 1, TAPBUF(TBIND), IBUF)
         IS = 9
         ENDCOM = .FALSE.
         IF (.NOT.FIRST) GO TO 90
            FIRST = .FALSE.
            GO TO 50
C
C                                       branch to handle record types
 90   GO TO (100, 200, 300, 400, 500, 600, 700), ITYP
C
C                                       RUN records
 100  CONTINUE
         CALL ZILI16 (2, IBUF(IS), 1, IRTEMP)
         CALL ZC8CL (4, 1, IRTEMP, OBS)
         CALL ZR8P4 ('4IB8', IBUF(IS+4), XDAT)
         XDAT = XDAT + 2400000.5D0
         CALL GREG (XDAT, SOURCE)
         CALL DATDAT (SOURCE, DATE)
         WRITE (PRTEXT,1100) OBS, DATE
         CALL PRTPLI (2, IERR)
         GO TO 50
C
C                                       ANTEnna records
 200  CONTINUE
         GO TO 50
C
C                                       SOURce records
 300  CONTINUE
         IF (FSOUR) GO TO 310
            WRITE (PRTEXT,1300) NVIS
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
 310     NVIS = 0
         ISEQ = ISEQ + 1
         FSOUR = .FALSE.
         CALL ZILI16 (4, IBUF(IS), 1, ITEMP)
         CALL ZC8CL (8, 1, ITEMP, CTEMP)
         SOURCE(1:8) = CTEMP(1:8)
         IQUAL = IBUF(IS+4)
         CALL ZR8P4 ('4IB8', IBUF(IS+18), F8)
         RA0 = 360.0D0 * 2D0**(-31) * F8
         CALL  ZR8P4 ('4IB8', IBUF(IS+20), F8)
         DEC0 = 360.0D0 * 2D0**(-31) * F8
         CALL COORDD (1, RA0, CH1, RM, RSEC)
         CALL COORDD (2, DEC0, CH2, DM, DSEC)
         CALL ZR8P4 ('4IB8', IBUF(IS + 22), F8)
         FREQ = F8 * 1D-6
         WRITE (PRTEXT,1310) ISEQ, SOURCE, IQUAL, CH1, RM, RSEC,
     *      CH2, DM, DSEC, FREQ
         CALL FILZCH (2, 31, PRTEXT)
         CALL FILZCH (2, 34, PRTEXT)
         CALL FILZCH (6, 37, PRTEXT)
         CALL FILZCH (2, 45, PRTEXT)
         CALL FILZCH (2, 48, PRTEXT)
         CALL FILZCH (5, 51, PRTEXT)
         CALL PRTPLI (2, IERR)
         IF (IERR.NE.0) GO TO 999
         GO TO 50
C                                       VISibility records
400   CONTINUE
         IWT1 = MAX (0, IBUF(IS+8))
         IWT2 = MAX (0, IBUF(IS+11))
         IWT3 = MAX (0, IBUF(IS+14))
         IWT4 = MAX (0, IBUF(IS+17))
         IF ((IWT1.GT.0) .OR. (IWT2.GT.0) .OR. (IWT3.GT.0) .OR.
     *      (IWT4.GT.0)) THEN
            NVIS = NVIS + 1
            TVIS = TVIS + 1
            END IF
         GO TO 50
C
C                                       DEFinition records
500   CONTINUE
         GO TO 50
C
C                                       END records
600   CONTINUE
         WRITE (PRTEXT,1300) NVIS
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         ENDCOM = .TRUE.
         FSOUR = .TRUE.
         GO TO 50
C
C                                       records of unknown origin
700   CONTINUE
         GO TO 50
C
C                                       error exit
 980  CALL MSGWRT (8)
      CALL PRTPLI (4, I)
C                                       file output only
 995  IF (IPTLEV.LE.-3) THEN
         IMNAME = 'MODCOMP VIS '
         IMCLAS = 'EXPORT'
         IMTYPE = 'EX'
         BTEMP = 'TotalVis'
         WRITE (CCTEMP, 1994) TVIS
         WRITE (PRTEXT,1995) TAPNAM, NFILES, IMTYPE, IMNAME, IMCLAS,
     *      0, BTEMP, CCTEMP
         CALL ZTXIO ('WRIT', PLUN, PIND, PRTEXT(:64), JER)
         IF (JER.NE.0) THEN
            WRITE (MSGTXT,1996) JER
            CALL MSGWRT (8)
            END IF
         IF (IERR.EQ.0) IERR = JER
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1055 FORMAT ('PRTEXP: LOGICAL RECORD LENGTH=',I8,' ERROR')
 1060 FORMAT ('PRTEXP: UNEXPECTED END OF FILE')
 1065 FORMAT ('PRTEXP: TAPIO TROUBLE WITH VIS. TAPE, IERR=',I3)
 1070 FORMAT ('PRTEXP: PHYSICAL RECORD LENGTH=',I9,' ERROR')
 1100 FORMAT (4X,'RUN  observer = ''',A4,'''   date = ',A12)
 1300 FORMAT (I8,' valid vis records')
 1310 FORMAT (I5,4X,'Found ',A8,I4,' :',A1,I2,I3,F7.3,1X,A1,I2,I3,F6.2,
     *   2X,'F=',F10.6,' GHz')
 1994 FORMAT (I8)
 1995 FORMAT (A12,I6,1X,A2,1X,A12,1X,A6,I5,1X,A8,1X,A8)
 1996 FORMAT ('ZTXIO RETURNS ERROR',I5)
      END
      SUBROUTINE PRTIBM (IRET)
C-----------------------------------------------------------------------
C   PRTIBM prints information about each IBM map format image in the
C   current tape file.
C   Output:  IRET  I    Error code: 0 => ok (incl unexpected EOF)
C                       Else serious tape error => quit
C-----------------------------------------------------------------------
      INTEGER IRET
C
      CHARACTER DIRTY(2)*5, DCLASS(8)*4, PRODUC(5)*8, CH1*1, CH2*1,
     *   NAME*8, BTEMP*8, CTEMP*8, IMTYPE*2, IMNAME*12, IMCLAS*6
      INTEGER   IERR, NX, I, ND, DD, IEREOF, MSKCLS, NCLASS, NY, MSQUAL,
     *   MNO, D2NX, D2NY, DFR1, IRA(2), IDEC(2), ZAND, JER, NIMAGE
      REAL      X, Y, ROT, RAS, DECS
      DOUBLE PRECISION    FREQ, C1, RA, DEC, DERA, DEDEC
      HOLLERITH HBUF(10)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PRTTP.INC'
      EQUIVALENCE (HBUF, IBUF)
      DATA IEREOF, MSKCLS /4,255/
      DATA DFR1, NCLASS, MSQUAL, MNO, D2NX, D2NY /9,7,5,6,17,18/
      DATA DIRTY /'DIRTY','CLEAN'/
      DATA DCLASS /'????','BEAM','IPOL','QPOL','UPOL','VPOL','P   ',
     *   'PA  '/
      DATA PRODUC /'NORMAL  ', 'COMPNTS ', 'RESIDUAL',
     *   'POINTS  ', '????????'/
C-----------------------------------------------------------------------
C                                       Init print, tape to start
      CALL PRTPLI (1, IRET)
      IF (IRET.NE.0) GO TO 999
      IRET = 0
      NIMAGE = 0
C                                       Read loop
C                                       Initialize, read 1 record
 10      FDVEC(2) = 256
         FDVEC(6) = 1
         FDVEC(31) = 0
         FDVEC(32) = 1
         MSGSUP = 32000
         MSGTXT = ' '
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         MSGSUP = 0
         CALL PRTPLI (4, JER)
         IF (IERR.EQ.IEREOF) GO TO 995
         IF (IERR.NE.0) GO TO 980
         IF (JER.NE.0) THEN
            IRET = JER
            GO TO 999
            END IF
         NRECS = NRECS + 1
         BYTES = BYTES + FDVEC(42)
         FNRECS = FNRECS + 1
         FBYTES = FBYTES + FDVEC(42)
         NIMAGE = NIMAGE + 1
C                                       translate header
CCC         CALL ZI16IL (124, 5, TAPBUF(TBIND), IBUF(5))
CCC         CALL ZC8CL (8, 1, TAPBUF(TBIND), CBUF)
         CALL ZI16IL (128, 1, TAPBUF(TBIND), IBUF)
C                                       Source, freq, status
         ND = IBUF(NCLASS)
         DD = 1
         IF (ND.LT.0) DD = 2
         ND = ZAND (ND, MSKCLS)
         ND = ND + 1
         IF ((ND.GT.8) .OR. (ND.LT.1)) ND = 1
         CALL ZR8P4 ('4IB8', IBUF(DFR1), FREQ)
         FREQ = FREQ * 1.0D-3
         CALL H2CHR (8, 1, HBUF, NAME)
         IF (NIMAGE.EQ.1) IMNAME = NAME
         IF (.NOT.SHORT) THEN
            WRITE (PRTEXT,1010) NAME, IBUF(MSQUAL),
     *         IBUF(MNO), DCLASS(ND), DIRTY(DD), FREQ
         ELSE
            WRITE (PRTEXT,1020) NAME, IBUF(MSQUAL),
     *         IBUF(MNO), DCLASS(ND), DIRTY(DD), FREQ
            END IF
         CALL PRTPLI (2, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Img size, max/min
         X = 2.0 ** IBUF(14)
         Y = IBUF(16) * X
         X = IBUF(15) * X
         IF (.NOT.SHORT) THEN
            WRITE (PRTEXT,1011) IBUF(D2NX), IBUF(D2NY), X, Y
            IF (IBUF(67).GT.0) WRITE (PRTEXT,1011) IBUF(D2NX),
     *      IBUF(D2NY), X, Y, IBUF(67)
         ELSE
            WRITE (PRTEXT,1021) IBUF(D2NX), IBUF(D2NY), X, Y
            IF (IBUF(67).GT.0) WRITE (PRTEXT,1021) IBUF(D2NX),
     *         IBUF(D2NY), X, Y, IBUF(67)
            END IF
         CALL PRTPLI (3, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       RA, DEC, increments, rot.
         IF (IPTLEV.LT.0) GO TO 50
         ROT = IBUF(23) / 100.0
         C1 = 360.0D0 / 65536.0D0 / 32768.0D0
         CALL ZR8P4 ('4IB8', IBUF(27), RA)
         CALL ZR8P4 ('4IB8', IBUF(29), DEC)
         CALL ZR8P4 ('4IB8', IBUF(19), DERA)
         CALL ZR8P4 ('4IB8', IBUF(21), DEDEC)
         RA = RA * C1
         DEC = DEC * C1
         DERA = -DERA / 1.0D5
         DEDEC = DEDEC / 1.0D5
         CALL COORDD (1, RA, CH1, IRA, RAS)
         CALL COORDD (2, DEC, CH2, IDEC, DECS)
         IF (.NOT.SHORT) THEN
            WRITE (PRTEXT,1012) CH1, IRA, RAS, CH2, IDEC, DECS,
     *         DERA, DEDEC, ROT
         ELSE
            WRITE (PRTEXT,1022) CH1, IRA, RAS, CH2, IDEC, DECS,
     *         DERA, DEDEC, ROT
            END IF
         CALL PRTPLI (3, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Clean parms
         IF ((DD.NE.2) .OR. (IBUF(40).LE.0)) GO TO 50
            I = IBUF(8)/256
            I = MAX (1, I)
            IF (I.GT.4) I = 5
            ROT = IBUF(39) / 100.0
            CALL ZR8P4 ('4IB8', IBUF(35), DERA)
            DERA = DERA / 1.0D5
            CALL ZR8P4 ('4IB8', IBUF(37), DEDEC)
            DEDEC = DEDEC / 1.0D5
            IF (.NOT.SHORT) THEN
               WRITE (PRTEXT,1013) IBUF(40), DERA,
     *            DEDEC, ROT, PRODUC(I)
            ELSE
               WRITE (PRTEXT,1023) IBUF(40), DERA, DEDEC,
     *            ROT, PRODUC(I)
               END IF
            CALL PRTPLI (3, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Skip over the rows
 50      NX = IBUF(D2NX)
         NY = IBUF(D2NY)
         FDVEC(2) = NX * 2
         FDVEC(31) = 0
         FDVEC(32) = NY
         IF (IERR.NE.0) THEN
            IF (IERR.NE.IEREOF) THEN
               WRITE (MSGTXT,1050) IERR
               GO TO 985
            ELSE
               MSGTXT = 'UNEXPECTED END OF FILE STARTING TO READ '
     *            // 'IBM-MAP DATA'
               GO TO 990
               END IF
            END IF
         DO 70 I = 1,NY
            MSGTXT = ' '
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            CALL PRTPLI (4, JER)
            IF (IERR.NE.0) THEN
               IF (IERR.NE.IEREOF) THEN
                  WRITE (MSGTXT,1060) IERR, I
                  GO TO 985
               ELSE
                  WRITE (MSGTXT,1061) I
                  GO TO 990
                  END IF
               GO TO 985
            ELSE
               NRECS = NRECS + 1
               BYTES = BYTES + FDVEC(42)
               FNRECS = FNRECS + 1
               FBYTES = FBYTES + FDVEC(42)
               END IF
            IF (JER.NE.0) THEN
               IRET = JER
               GO TO 999
               END IF
 70         CONTINUE
         GO TO 10
C
 980  WRITE (MSGTXT,1980) IERR
C
 985  IRET = 8
 990  CALL MSGWRT (8)
      CALL PRTPLI (4, IERR)
C                                       file output only
 995  IF (IPTLEV.LE.-3) THEN
         IMCLAS = 'IBMMAP'
         IMTYPE = 'IM'
         BTEMP = '# IMAGES'
         WRITE (CTEMP, 1994) NIMAGE
         WRITE (PRTEXT,1995) TAPNAM, NFILES, IMTYPE, IMNAME, IMCLAS,
     *      0, BTEMP, CTEMP
         CALL ZTXIO ('WRIT', PLUN, PIND, PRTEXT(:64), JER)
         IF (JER.NE.0) THEN
            WRITE (MSGTXT,1996) JER
            CALL MSGWRT (8)
            END IF
         IF (IRET.EQ.0) IRET = JER
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Source= ''',A8,'''',I6,3X,'Mapno=',I6,3X,'Class',
     *   1X,A4,1X,A5,3X,'Freq=',F10.3,' MHz')
 1011 FORMAT ('Mapsize=',2I5,6X,'Min,max=',2(1PE13.4),5X,I2,
     *   ' blanking bits')
 1012 FORMAT ('RA=',A1,I2,I3,F7.3,2X,'Dec=',A1,I2,I3,F6.2,4X,
     *   'Cellsize=',2F8.3,4X,'Rotat=',F6.1)
 1013 FORMAT ('CLEAN iter',I6,4X,'Bmaj=',F8.4,' Bmin=',F8.4,' Bpa=',
     *   F6.1,4X,'Product= ',A8)
 1020 FORMAT ('Source= ''',A8,'''',I6,3X,'No=',I6,3X,'Class',
     *   1X,A4,1X,A5,3X,'Freq=',F10.3)
 1021 FORMAT ('Size=',2I5,5X,'Min,max=',2(1PE12.4),5X,I2,' SN bits')
 1022 FORMAT ('RA=',A1,I2,I3,F7.3,' Dec=',A1,I2,I3,F6.2,3X,
     *   'Cells=',2F8.3,3X,'Rot=',F6.1)
 1023 FORMAT ('Niter=',I6,4X,'Bmaj=',F7.3,'  Bmin=',F7.3,'  Bpa=',
     *   F6.1,4X,'Prod= ',A8)
 1050 FORMAT ('ERROR',I7,' STARTING TO READ IBM-MAP DATA')
 1060 FORMAT ('ERROR',I7,' READING IBM-MAP ROW',I5)
 1061 FORMAT ('UNEXPECTED END OF FILE READING IBM-MAP ROW',I5)
 1980 FORMAT ('ERROR',I7,' READING IBM-MAP HEADER')
 1994 FORMAT (I8)
 1995 FORMAT (A12,I6,1X,A2,1X,A12,1X,A6,I5,1X,A8,1X,A8)
 1996 FORMAT ('ZTXIO RETURNS ERROR',I5)
      END
      SUBROUTINE PRTFIT (IRET)
C-----------------------------------------------------------------------
C   PRTFIT prints each card image in a FITS headers.  It parses them
C   to determine the expected number of standard data records.  It
C   then skips over that number of records and counts any further
C   records in the file.
C   Output: IRET   I     Error code: 0 => ok incl unexpected EOF
C                        Other => serious tape error. quit
C-----------------------------------------------------------------------
      INTEGER IRET
C
      CHARACTER PTYPES(20)*8, TABNAM*8, SYM*8, TWORD(5)*8
      INTEGER   PCOUNT, NDIM, INC, NC, JJ, NPNT, ITYP, IEREOF, TABLES,
     *   TABWID, TABCRD, I, IC, IERR, ITAB, NCHAR, IER, GROUP,
     *   GCOUNT, TABCNT, JJ4, JJJ4, NAXIS, JER, II, IPIECE, ITRIM,
     *   USED(300)
      DOUBLE PRECISION    X, PSCAL(20), POFF(20)
      LOGICAL   EQUAL, DOIT, LEND, EOF, ISHIST
      REAL      PCMATX(7,7), CDMATX(7,7), PVMATX(7,7)
      INCLUDE 'PRTTP.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA IEREOF /4/
      DATA TWORD /'END     ', 'TABCOUNT', 'TABWIDTH',
     *   'TABCARDS', 'TABNAME '/
C-----------------------------------------------------------------------
      IPIECE = 1
C                                       open new version
 10   IF ((IPIECE.GT.1) .AND. (DODISK)) THEN
C                                       close old
         CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
         I = ITRIM (INFILE)
         IF (IPIECE-1.GT.9) I = I - 1
         IF (IPIECE-1.GT.99) I = I - 1
         IF (IPIECE.LE.9) THEN
            WRITE (INFILE(I:),1010) IPIECE
         ELSE IF (IPIECE.LE.99) THEN
            WRITE (INFILE(I:),1011) IPIECE
         ELSE
            WRITE (INFILE(I:),1012) IPIECE
            END IF
         CALL CHR2H (48, INFILE, 1, FDVEC(7))
         CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1015) IRET
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
C                                       Init printer
      CALL PRTPLI (1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Init parms
      PCOUNT = 0
      NDIM = 0
      GROUP = 0
      GCOUNT = 1
      TABLES = 0
      CALL FILL (4, 0, NPV)
C                                       Init header
      DO 15 I = 1,20
         PSCAL(I) = 0.0D0
         POFF(I) = 0.0D0
         PTYPES(I) = ' '
 15      CONTINUE
      CALL RFILL (49, 0.0, PCMATX)
      CALL RFILL (49, 0.0, PVMATX)
      CALL RFILL (49, 0.0, CDMATX)
      CALL CATINI (CATBLK)
      CALL FILL (300, 0, USED)
      IERR = 0
      NRECS = NRECS - 1
      BYTES = BYTES - FDVEC(42)
      FNRECS = FNRECS - 1
      FBYTES = FBYTES - FDVEC(42)
C                                       Read header loop
      DO 50 JJ = 1,30000000
         IF ((JJ.GT.1) .OR. (IPIECE.GT.1)) THEN
            MSGTXT = ' '
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            CALL PRTPLI (4, JER)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR
               IF (IERR.EQ.IEREOF) GO TO 995
               GO TO 990
               END IF
            IF (JER.NE.0) THEN
               IRET = JER
               GO TO 999
               END IF
            END IF
         NRECS = NRECS + 1
         BYTES = BYTES + 2880
         FNRECS = FNRECS + 1
         FBYTES = FBYTES + 2880
         CALL ZC8CL (2880, 1, TAPBUF(TBIND), CBUF)
C                                       Loop over cards in block
         PRTEXT (1:NCR) = ' '
         DO 40 NC = 1,36
C                                       Print the card first
            INC = (NC-1) * 80 + 1
            PRTEXT = CBUF(INC:INC+79)
            ISHIST = PRTEXT(1:8) .EQ. 'HISTORY '
            DOIT = (IPTLEV.GE.2) .OR. ((IPTLEV.EQ.1) .AND.
     *         (.NOT.ISHIST) .AND. (PRTEXT(1:8).NE.' '))
            EQUAL = PRTEXT(1:8) .EQ. 'ORIGIN  '
            LEND = PRTEXT(1:8) .EQ. 'DATE    '
            DOIT = ((DOIT) .OR. (LEND) .OR. (EQUAL)) .AND. (IPTLEV.GE.0)
            IF (DOIT) CALL PRTPLI (3, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Parse keywords
            IF ((.NOT.ISHIST) .OR. (XERR.LT.2)) THEN
               CALL FPARSE (NC, CBUF, PSCAL, POFF, PTYPES, TABLES, LEND,
     *            GROUP, BITPIX, BSCALE, BZERO, NAXIS, NPV, UVTABL,
     *            USED, IERR)
               IF (LEND) GO TO 60
               IF ((IERR.EQ.1) .AND. ((PRTEXT(:2).EQ.'CD') .OR.
     *            (PRTEXT(:2).EQ.'PC') .OR. (PRTEXT(:2).EQ.'PV')))
     *            CALL PCCARD (GROUP, PRTEXT, PCMATX, CDMATX, PVMATX)
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       END found
C                                       PC -> CROTA
C                                       CD -> CDELT, CROTA
 60   CALL PCHDR (PCMATX, CDMATX, PVMATX)
      PCOUNT = CATBLK(KIPCN)
      GCOUNT = CATBLK(KIGCN)
      NDIM = CATBLK(KIDIM)
      VLBA = (NDIM.EQ.0) .AND. (GROUP.EQ.1) .AND. (GCOUNT.EQ.0) .AND.
     *   (PCOUNT.EQ.0) .AND. (BITPIX.EQ.8)
      IF ((PCOUNT.GT.0) .OR. VLBA .OR. UVTABL) THEN
         CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      ELSE
         CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
         END IF
      IF (VLBA) THEN
         NPIECE = 1
         CALL PRTVLB (IRET)
      ELSE IF (UVTABL) THEN
         MSGTXT = 'Reading binary tables for UV data'
         CALL MSGWRT (2)
         IPIECE = NPV(1)
         IF (NPIECE.LE.0) NPIECE = NPV(2)
         NPIECE = MIN (NPIECE, NPV(2))
      ELSE
         NPIECE = 1
         CALL PRTHDR (PSCAL, POFF, PTYPES, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       Size of data file
      X = 0.0D0
      IF (NDIM.LT.1) GO TO 120
         X = 1.0D0
         DO 110 I = 1,NDIM
            X = X * CATBLK(KINAX+I-1)
 110        CONTINUE
 120  X = X + PCOUNT
      IF (GCOUNT.GT.1) X = X * GCOUNT
      X = X * ABS(BITPIX) / 8.0D0
      JJ4 = (X - 1.D0) / 2880.0D0 + 1.0D0 + 1.0D-7
      JJJ4 = JJ4
      IF (UVTABL) JJJ4 = 0
C                                       Advance over data
C                                       fast advance for PRTLEV=-4
C                                       and file bigger than ~1 MByte
      IF ((IPTLEV.LE.-4) .AND. (JJ4.GT.400) .AND. (.NOT.DODISK)) THEN
         IF (UVTABL) CALL PRTHDR (PSCAL, POFF, PTYPES, IRET)
         CALL TAPIO ('ADVF', FDVEC, TAPBUF, TBIND, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.4)) THEN
            WRITE (MSGTXT,1120) IERR
            GO TO 990
            END IF
C                                       Can't count tables
         BYTES = BYTES + JJ4 * 2880.D0
         NRECS = NRECS + JJ4
         FBYTES = FBYTES + JJ4 * 2880.D0
         FNRECS = FNRECS + JJ4
         IEXTRA = 0
         GO TO 250
C                                       Advance by reading data
      ELSE IF ((JJ4.GT.0) .AND. (.NOT.UVTABL)) THEN
 124     I = 0
         JJ = MIN (300000, JJ4)
         IF (IERR.NE.0) GO TO 130
         DO 125 I = 1,JJ
            MSGTXT = ' '
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            CALL PRTPLI (4, JER)
            IF (IERR.NE.0) GO TO 130
            IF (JER.NE.0) THEN
               IRET = JER
               GO TO 999
               END IF
            NRECS = NRECS + 1
            BYTES = BYTES + 2880
            FNRECS = FNRECS + 1
            FBYTES = FBYTES + 2880
 125        CONTINUE
         JJ4 = JJ4 - JJ
         IF (JJ4.GT.0) GO TO 124
         END IF
      GO TO 135
C                                       Error
 130  CONTINUE
         WRITE (MSGTXT,1130) IERR, I
         IF (IERR.NE.IEREOF) GO TO 990
         GO TO 995
C                                       report reading
 135  WRITE (PRTEXT,1135) JJJ4
      IF ((IPTLEV.GE.0) .AND. (.NOT.VLBA)) CALL PRTPLI (2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       See if we have new fits tables.
      IEXTRA = 0
      PRTEXT(1:NCR) = ' '
      CALL EXTPRT (IPTLEV, EOF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((EOF) .AND. (TABLES.LE.0)) GO TO 250
      IEXTRA = 1
C                                       TABLES extensions
      IF (TABLES.LE.0) GO TO 230
      IEXTRA = 0
C                                       Loop over tables
         DO 220 ITAB = 1,TABLES
            TABCNT = 0
            TABWID = 0
            TABCRD = 0
            TABNAM = ' '
            DO 190 JJ = 1,2000
C                                       Loop over header cards
               PRTEXT(1:NCR) = ' '
               DO 180 NC = 1,36
                  INC = (NC-1) * 80 + 1
                  PRTEXT(1:80) = CBUF(INC:INC+79)
                  IF ((NC.EQ.1) .AND. (IPTLEV.GE.1)) CALL PRTPLI (2,
     *               IRET)
                  IF ((NC.NE.1) .AND. (IPTLEV.GE.1)) CALL PRTPLI (3,
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       Care about keyword?
                  DO 165 IC = 1,5
                     EQUAL = PRTEXT(1:8) .EQ. TWORD(IC)
                     IF (EQUAL) GO TO 170
 165                 CONTINUE
                  GO TO 180
C                                       END card or must parse
 170              IF (IC.EQ.1) GO TO 200
                  NPNT = 1
                  CALL GETSYM (PRTEXT, NPNT, SYM, ITYP)
                  IF (ITYP.EQ.0) GO TO 175
                     WRITE (MSGTXT,1050) JJ, NC
                     CALL MSGWRT (7)
                     CALL PRTPLI (4, IRET)
                     IF (IRET.NE.0) GO TO 999
                     GO TO 180
 175              IF (IC.NE.5) THEN
                     CALL GETNUM (PRTEXT, 80, NPNT, X)
                     IF (X.EQ.DBLANK) GO TO 975
                     IF (IC.EQ.2) TABCNT = X + 0.01
                     IF (IC.EQ.3) TABWID = X + 0.01
                     IF (IC.EQ.4) TABCRD = X + 0.01
                  ELSE
                     CALL GETSTR (PRTEXT, 80, 8, NPNT, TABNAM, NCHAR)
                     END IF
 180              CONTINUE
C                                       Read next table header card.
               MSGTXT = ' '
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
               CALL PRTPLI (4, JER)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1150) IERR
                  IF (IERR.EQ.IEREOF) GO TO 995
                  GO TO 990
               ELSE
                  CALL ZC8CL (2880, 1, TAPBUF(TBIND), CBUF)
                  NRECS = NRECS + 1
                  BYTES = BYTES + 2880
                  FNRECS = FNRECS + 1
                  FBYTES = FBYTES + 2880
                  END IF
               IF (JER.NE.0) THEN
                  IRET = JER
                  GO TO 999
                  END IF
 190           CONTINUE
            WRITE (MSGTXT,1190) ITAB
            CALL MSGWRT (8)
            CALL PRTPLI (4, IRET)
            IF (IRET.NE.0) GO TO 999
            GO TO 230
C                                       Read over table data
 200        IF (TABCRD.LE.0) THEN
               WRITE (MSGTXT,1200) ITAB
               CALL MSGWRT (6)
               CALL PRTPLI (4, IRET)
               IF (IRET.NE.0) GO TO 999
               GO TO 230
               END IF
            WRITE (PRTEXT,1205) TABCNT
            PRTEXT(13:20) = TABNAM(1:8)
            IF (IPTLEV.LT.1) CALL PRTPLI (2, IRET)
            IF (IRET.NE.0) GO TO 999
            IF ((TABCNT.LE.0) .OR. (TABWID.LE.0)) GO TO 220
            X = TABCNT
            X = X * TABWID
            X = DINT ((X-1.0D0)/TABCRD) + 1.0D0
            X = X - (36-NC)
            IF (X.LT.1.0D0) GO TO 215
               JJ = (X-1.0D0) / 36.0D0 + 1.0D0
               DO 210 I = 1,JJ
C                                       Read next table
                  MSGTXT = ' '
                  CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
                  CALL PRTPLI (4, JER)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1206) IERR, I, ITAB
                     IF (IERR.EQ.IEREOF) GO TO 995
                     GO TO 990
                  ELSE
                     NRECS = NRECS + 1
                     BYTES = BYTES + 2880
                     FNRECS = FNRECS + 1
                     FBYTES = FBYTES + 2880
                     END IF
                  IF (JER.NE.0) THEN
                     IRET = JER
                     GO TO 999
                     END IF
 210              CONTINUE
 215        WRITE (PRTEXT,1215)
            IF (IPTLEV.GE.0) CALL PRTPLI (2, IRET)
            IF (IRET.NE.0) GO TO 999
 220        CONTINUE
C                                       Look for any extra records
 230  MSGSUP = 32000
      DO 240 I = 1,10000000
         IEXTRA = IEXTRA + 1
         UNKNWN = UNKNWN + 1
         MSGTXT = ' '
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         CALL PRTPLI (4, JER)
         IF (IERR.NE.0) GO TO 245
         IF (JER.NE.0) THEN
            IRET = JER
            GO TO 999
            END IF
         NRECS = NRECS + 1
         BYTES = BYTES + 2880
         FNRECS = FNRECS + 1
         FBYTES = FBYTES + 2880
 240     CONTINUE
      MSGSUP = 0
      WRITE (PRTEXT,1240) IEXTRA
      CALL PRTPLI (2, IRET)
      II = 1
      IF (.NOT.DODISK) CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), II,
     *   IERR)
      GO TO 250
C                                       error ?
 245  CONTINUE
         IEXTRA = IEXTRA - 1
         UNKNWN = UNKNWN - 1
         IF (IERR.NE.IEREOF) THEN
            MSGSUP = 0
            WRITE (MSGTXT,1245) IERR, IEXTRA
            GO TO 990
            END IF
 250  MSGSUP = 0
      IF (IEXTRA.LE.0) WRITE (PRTEXT,1250)
      IF (IEXTRA.GT.0) WRITE (PRTEXT,1251) IEXTRA
      IF ((IEXTRA.GT.0) .OR. (IPTLEV.GE.0)) CALL PRTPLI (2, IRET)
C                                       loop for more
      IF (IPIECE.LT.NPIECE) THEN
         IF (FNRECS.GT.0) THEN
            IF ((FNRECS.GT.1) .OR. (FBYTES.GT.512.0D0)) THEN
               FBYTES = FBYTES / (1024.0D0 * 1024.0D0)
               WRITE (MSGTXT,1991) FBYTES, FNRECS
               CALL MSGWRT (4)
               CALL PRTPLI (4, IERR)
               END IF
            FNRECS = 0
            FBYTES = 0.0D0
            END IF
         NFILES = NFILES + 1
         IPIECE = IPIECE + 1
         GO TO 10
         END IF
      GO TO 999
C
 975  MSGTXT = 'PRTFIT: NUMBER ERROR PARSING CARD' // PRTEXT
 990  IRET = 8
 995  CALL MSGWRT (8)
      CALL PRTPLI (4, IER)
      IF ((IERR.EQ.IEREOF) .AND. (IRET.NE.8)) THEN
         MSGTXT = 'UNEXPECTED END OF FILE'
         CALL MSGWRT (6)
         CALL PRTPLI (4, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (I1)
 1011 FORMAT (I2)
 1012 FORMAT (I3)
 1015 FORMAT ('ERROR OPENING NEW DISK FILE:',I7)
 1020 FORMAT ('ERROR',I7,' READING FITS TAPE')
 1050 FORMAT ('Recognized keyword in record',I5,' card',I3,
     *   'IMPROPER TYPE')
 1120 FORMAT ('ERROR ADVANCING TO EOF',I5)
 1130 FORMAT ('ERROR',I7,' TRYING TO READ RECORD',I5,'  OF FITS DATA')
 1135 FORMAT (I7,' records of FITS data read correctly')
 1150 FORMAT ('ERROR',I7,' READING FITS TABLE HEADER')
 1190 FORMAT ('END NOT FOUND FOR FITS TABLE HEADER',I6)
 1200 FORMAT ('FITS TABLE',I7,' ILLEGAL VALUE FOR TABCARDS')
 1205 FORMAT ('Tablename= ''',8X,'''',6X,'Tablesize=',I10)
 1206 FORMAT ('ERROR',I7,' READING RECORD',I4,' IN TABLE',I4)
 1215 FORMAT ('All FITS table records read')
 1240 FORMAT ('MORE THAN',I7,' UNKNOWN RECORDS FOLLOW FITS DATA')
 1245 FORMAT ('ERROR',I7,' AFTER',I7,' RECORDS FOLLOWING THE DATA')
 1250 FORMAT ('End of file read as expected')
 1251 FORMAT (I7,' unknown records follow FITS data')
 1991 FORMAT ('This file: read',F10.3,' Megabytes in',I8,
     *   ' logical records')
      END
      SUBROUTINE EXTPRT (PRTLEV, EOF, IERR)
C-----------------------------------------------------------------------
C   This routine will parse a block from a FITS tape and look for the
C   required cards of a FIT extension header block
C   XTENSION, BITPIX, NAXIS, NAXISn, PCOUNT, GCOUNT.
C   Inputs:
C      PRTLEV       I   Amount of printing.
C   In/Out from common:
C      IBUF(*)      I   a block of fit header data.
C      TBIND        I   Pointer in IBUF
C   Outputs:
C      EOF          L   T means End of file on read.
C      IERR         I   0=ok, 1=messed up. An error message will
C                       be printed.
C-----------------------------------------------------------------------
      CHARACTER STRING*80, SYM*8, EWORD(7)*8, SYMBOL*8, SNAXIS*8,
     *   CHSKIP(3)*4, EXTTYP*16, CVER*1
      INTEGER   PRTLEV, IERR, NAXIS, PCOUNT, GCOUNT, NAXISI(50),
     *   ILINES, INBLK, JJ4, IKEYWD, NPNT, ITABNO, II, JJ, NBY, NBYT,
     *   NCHAR, I, NLEN, TERR, IFOUND, ICARD, K, INC, ITYP, IER, JER,
     *   TFIELD, NFOUND
      LOGICAL   END, TABL, EQUAL, EOF
      DOUBLE PRECISION    X, NBITS, AXCNT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'PRTTP.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSKIP /'HIST','COMM','    '/
      DATA EWORD /'XTENSION', 'BITPIX  ',  'NAXIS   ',
     *   'PCOUNT  ', 'GCOUNT  ', 'TFIELDS', 'EXTNAME ' /
C-----------------------------------------------------------------------
      NBY = 2880 * 2
      DO 700 K = 1,32000
C                                       Read 1st block.
 10      MSGTXT = ' '
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
         CALL PRTPLI (4, JER)
         IF (TERR.NE.0) THEN
            IF (TERR.EQ.4) GO TO 940
            WRITE (MSGTXT,1000)
            GO TO 980
            END IF
         IF (JER.NE.0) THEN
            IERR = JER
            GO TO 999
            END IF
         NRECS = NRECS + 1
         BYTES = BYTES + 2880
         FNRECS = FNRECS + 1
         FBYTES = FBYTES + 2880
         CALL ZC8CL (2880, 1, TAPBUF(TBIND), CBUF)
         IF (CBUF(:8).NE.EWORD(1)) THEN
            IF (CBUF(:7).EQ.'TABNAME') GO TO 950
            IF (CBUF(:8).EQ.'SIMPLE  ') THEN
               MSGTXT = 'EXTPRT: NEW FITS FILE FOUND WHILE LOOKING FOR'
     *            // ' TABLE EXTENSION'
               CALL MSGWRT (8)
               GO TO 950
               END IF
            UNKNWN = UNKNWN + 1
            GO TO 10
            END IF
         EOF = .FALSE.
C                                       Look for XTENSION= 'type' card
         ICARD = 1
         IKEYWD = 1
         NPNT = 1
         CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), CBUF, NPNT,
     *      PRTEXT, SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
         IF (IERR.NE.0) GO TO 950
         IF (PRTLEV.GE.0) CALL PRTPLI (2, IERR)
         IF (IERR.NE.0) GO TO 999
         NLEN = 16
         CALL GETSTR (PRTEXT, 80, NLEN, NPNT, STRING, NCHAR)
         NBYT = 16
         EXTTYP = ' '
         EXTTYP(1:NCHAR) = STRING(1:NCHAR)
C
         ICARD = ICARD + 1
         IKEYWD = IKEYWD + 1
         NPNT = 1
         CALL SKPBLK (CBUF, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), CBUF, NPNT,
     *      PRTEXT, SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (PRTLEV.GT.0) CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Get value of BITPIX
         CALL GETNUM (PRTEXT, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         IF (X.GT.0.) BITPIX = X + 0.1
         IF (X.LE.0.) BITPIX = X - 0.1
C                                       Warning-Non standard bits/pixel.
         IF ((BITPIX.NE.8) .AND. (BITPIX.NE.16) .AND. (BITPIX.NE.32))
     *      THEN
            IF ((EXTTYP.NE.'IMAGE') .OR. (BITPIX.NE.-32)) THEN
               WRITE (MSGTXT,1010) BITPIX
               CALL MSGWRT (5)
               CALL PRTPLI (4, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            END IF
C                                       Get NAXIS
         BITPIX = ABS (BITPIX)
         ICARD = ICARD + 1
         IKEYWD = IKEYWD + 1
         NPNT = 1
         CALL SKPBLK (CBUF, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), CBUF, NPNT,
     *      PRTEXT, SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (PRTLEV.GT.0) CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GETNUM (PRTEXT, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         NAXIS = X + .01
C                                       Check for invalid no. of axis
C                                       for our buffer.
         IF (NAXIS.GT.50) GO TO 960
C                                       Find NAXISi
         DO 230 I = 1,NAXIS
            ICARD = ICARD + 1
            NPNT = 1
            CALL SKPBLK (CBUF, ICARD, FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (I.LE.10) THEN
               NLEN = 6
               WRITE (STRING,1020) I
            ELSE
               NLEN = 7
               WRITE (STRING,1200) I
               END IF
            SNAXIS = ' '
            SNAXIS(1:NLEN) = STRING(1:NLEN)
            CALL GTWCRD (ICARD, 1, 0, SNAXIS, CBUF, NPNT, PRTEXT,
     *         SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
            IF (IERR.NE.0) GO TO 970
            IF (PRTLEV.GT.0) CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GETNUM (PRTEXT, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 975
            NAXISI(I) = X + .01
 230        CONTINUE
C                                       PCOUNT
         ICARD = ICARD + 1
         IKEYWD = IKEYWD + 1
         NPNT = 1
         CALL SKPBLK (CBUF, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), CBUF, NPNT,
     *      PRTEXT, SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (PRTLEV.GT.0) CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GETNUM (PRTEXT, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         PCOUNT = X
C                                       Get GCOUNT
         ICARD = ICARD + 1
         IKEYWD = IKEYWD + 1
         NPNT = 1
         CALL SKPBLK (CBUF, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), CBUF, NPNT,
     *      PRTEXT, SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (PRTLEV.GT.0) CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GETNUM (PRTEXT, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         GCOUNT = X
C                                       Get TFIELDS
         IF (EXTTYP.NE.'IMAGE') THEN
            ICARD = ICARD + 1
            IKEYWD = IKEYWD + 1
            NPNT = 1
            CALL SKPBLK (CBUF, ICARD, FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), CBUF, NPNT,
     *         PRTEXT, SYMBOL, ITABNO, NFOUND, IFOUND, CVER, END, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (PRTLEV.GT.0) CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GETNUM (PRTEXT, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 975
            TFIELD = X
            END IF
C                                       Forget about table name if we
C                                       are printing everything.
         IF (PRTLEV.LE.0) THEN
            TABL = ((EXTTYP(1:8) .EQ. 'TABLE   ') .OR.
     *      (EXTTYP(1:8) .EQ. 'A3DTABLE') .OR.
     *      (EXTTYP(1:8) .EQ. '3DTABLE ') .OR.
     *      (EXTTYP(1:8) .EQ. 'BINTABLE'))
         ELSE
            TABL = .FALSE.
            END IF
C                                       Read until we get an end card
         DO 400 I = 1, 32000
            ICARD = ICARD + 1
C                                       Read another block of data.
            IF (ICARD.GE.37) THEN
               MSGTXT = ' '
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
               CALL PRTPLI (4, JER)
               IF (TERR.NE.0) THEN
                  IF (TERR.EQ.4) GO TO 940
                  WRITE (MSGTXT,1000)
                  GO TO 980
                  END IF
               IF (JER.NE.0) THEN
                  IERR = JER
                  GO TO 999
                  END IF
               NRECS = NRECS + 1
               BYTES = BYTES + 2880
               FNRECS = FNRECS + 1
               FBYTES = FBYTES + 2880
               ICARD = 1
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), CBUF)
               END IF
C
            INC = (ICARD - 1) * 80 + 1
            PRTEXT(1:80) = CBUF(INC:INC+79)
            EQUAL = PRTEXT(1:4) .EQ. CHSKIP(1)
            IF (.NOT.EQUAL) EQUAL = PRTEXT(1:4) .EQ. CHSKIP(2)
            IF (.NOT.EQUAL) EQUAL = PRTEXT(1:4) .EQ. CHSKIP(3)
            IF (PRTLEV.LE.0) EQUAL = .TRUE.
            IF (PRTLEV.GT.1) EQUAL = .FALSE.
            IF (.NOT.EQUAL) CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Get next symbol.
            NPNT = 1
            CALL GETSYM (PRTEXT, NPNT, SYM, ITYP)
            SYMBOL(1:8) = SYM(1:8)
C                                       Test for END card.
            IF (SYMBOL.EQ.'END     ') GO TO 500
C                                       EXTNAME
            IF (SYMBOL.EQ.'EXTNAME') THEN
C                                       Print name and stop looking.
               IF (TABL) THEN
                  IF (PRTLEV.GE.-1) CALL PRTPLI (3, IERR)
                  IF (IERR.NE.0) GO TO 999
                  TABL = .FALSE.
                  END IF
               IF ((UVTABL) .AND. (PRTEXT(12:18).EQ.'AIPS UV')) THEN
                  CALL PRTUVT (ICARD, TFIELD, NAXIS, NAXISI, IERR)
                  IF (IERR.NE.0) GO TO 999
                  X = NAXISI(1)
                  X = NAXISI(2) * X
                  JJ4 = (X - 1.D0) / 2880.0D0 + 1.0D0 + 1.0D-7
C                                       Advance over data
C                                       fast advance for PRTLEV=-4
C                                       and file bigger than ~1 MByte
                  IF ((IPTLEV.LE.-4) .AND. (JJ4.GT.400) .AND.
     *               (.NOT.DODISK)) THEN
                     CALL TAPIO ('ADVF', FDVEC, TAPBUF, TBIND, IERR)
                     IF ((IERR.NE.0) .AND. (IERR.NE.4)) THEN
                        WRITE (MSGTXT,1120) IERR
                        CALL MSGWRT (8)
                        END IF
C                                       Can't count tables
                     BYTES = BYTES + JJ4 * 2880.D0
                     NRECS = NRECS + JJ4
                     FBYTES = FBYTES + JJ4 * 2880.D0
                     FNRECS = FNRECS + JJ4
                     IEXTRA = 0
                     GO TO 999
                     END IF
                  GO TO 500
                  END IF
               END IF
 400        CONTINUE
C                                       Shouldn't get here.
         WRITE (MSGTXT,1400)
         CALL MSGWRT (6)
         CALL PRTPLI (4, IER)
         GO TO 999
C                                       End card found
C                                       Advance the proper number of
C                                       records.
C                                       Calculate no. of data blocks.
 500     AXCNT = 1
         DO 550 II = 1,NAXIS
            AXCNT = AXCNT * NAXISI(II)
 550        CONTINUE
         AXCNT = AXCNT + PCOUNT
         NBITS = (ABS(BITPIX) / 8) * GCOUNT * AXCNT
         INBLK = (NBITS + 2879.0D0) / 2880.0D0
         JJ4 = 0
 560     JJ = 300000
            IF (INBLK.LT.JJ) JJ = INBLK
            DO 565 I = 1,JJ
               JJ4 = JJ4 + 1
               MSGTXT = ' '
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
               CALL PRTPLI (4, JER)
               IF (IERR.NE.0) GO TO 570
               IF (JER.NE.0) THEN
                  IERR = JER
                  GO TO 999
                  END IF
               NRECS = NRECS + 1
               BYTES = BYTES + 2880
               FNRECS = FNRECS + 1
               FBYTES = FBYTES + 2880
 565           CONTINUE
            INBLK = INBLK - JJ
            IF (INBLK.LE.0) GO TO 580
            GO TO 560
C                                       Error
 570     IF (IERR.NE.4) THEN
            WRITE (MSGTXT,1570) IERR, JJ4
         ELSE
            WRITE (MSGTXT,1571) JJ4
            END IF
         CALL MSGWRT (8)
         CALL PRTPLI (4, IER)
         GO TO 999
C                                       Print tape summary.
C                                       Suppress for terse output.
 580     IF (PRTLEV.GE.-1) THEN
            IF (NAXISI(1).GT.0) AXCNT = AXCNT / NAXISI(1)
            ILINES = AXCNT
            WRITE (PRTEXT,1580) ILINES
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 700     CONTINUE
C                                       End of file.
 940  IERR = 0
      EOF = .TRUE.
      GO TO 999
C                                       Not extension record.
 950  IERR = 0
      GO TO 999
C                                       Invalid number of axis.
 960  WRITE (MSGTXT,1960) NAXIS
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) EWORD(IKEYWD), SYMBOL
      GO TO 980
 975  MSGTXT = 'EXTPRT: NUMBER ERROR ON ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      CALL PRTPLI (2, IERR)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR READING TAPE')
 1010 FORMAT ('WARNING! Nonstandard bits per pixel =',I6)
 1020 FORMAT ('NAXIS',I1)
 1120 FORMAT ('ERROR ADVANCING TO EOF',I5)
 1200 FORMAT ('NAXIS',I2)
 1400 FORMAT ('MORE THAN 32000 HEADER CARDS FOR THIS EXTENSION.')
 1570 FORMAT ('TAPE READ ERROR',I4,' AT BLOCK',I8,' IN EXTENSION DATA')
 1571 FORMAT ('UNEXPECTED END OF FILE AT BLOCK',I8,' IN EXTENSION DATA')
 1580 FORMAT ('Number of extension data lines =',I8)
 1960 FORMAT ('NUMBER OF AXIS TOO LARGE FOR BUFFER =',I6)
 1970 FORMAT ('EXPECTED KEYWORD ',A8,'. FOUND ',A8,'.')
      END
      SUBROUTINE PRTHDR (PSCAL, POFF, PTYPES, IERR)
C-----------------------------------------------------------------------
C   PRTHDR lists the contents of a standard header on the printer.
C   Inputs:
C      PSCAL    D(20)     Random parameter scaling factors
C      POFF     D(20)     Random parameter offsets
C      PTYPES   C(20)*8   Random parameter types
C   Output:
C      IERR     I         not 0 => please quit
C-----------------------------------------------------------------------
      CHARACTER PTYPES(20)*(*)
      DOUBLE PRECISION PSCAL(20), POFF(20)
      INTEGER   IERR
C
      CHARACTER LDATE*12, MDATE*12, VTYA(2)*8, VTYB(3)*4, PRODUC(5)*8,
     *   BTEMP*8, CTEMP*8, BCODE(7)*10, CH1*1, CH2*1, IMTYPE*2,
     *   IMNAME*12, IMCLAS*6, CTYPE*2
      INTEGER   I, J, NAX, HM(2), INC, KPT, DM(2), CLAXIS, CMAXIS, IBS
      REAL      SEC, DEC, BEAMS(3), DSEC, XTEST, YTEST
      DOUBLE PRECISION    BS, BZ, FREQ
      LOGICAL   ISUV, ISFREQ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PRTTP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA VTYA, VTYB /'OPTICAL ','RADIO   ', 'LSR ','SUN ',
     *   'YOU '/
      DATA PRODUC /'NORMAL  ','COMPNTS ','RESIDUAL','POINTS  ',
     *   'DIRTY   '/
      DATA BCODE /'UNKNOWN ','PIXEL 8BIT','SHORT INT ',
     *   'LONG INT','DOUBLE INT','FLOATING','DBL FLOAT'/
C-----------------------------------------------------------------------
C                                       Image name and file name
      CALL H2CHR (8, 1, CATH(KHOBJ), BTEMP)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), IMNAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), IMCLAS)
      CALL H2CHR (2, KHPTYO, CATH(KHPTY), IMTYPE)
      ISUV = IMTYPE.EQ.'UV'
C                                       file output only
      IF (IPTLEV.LE.-3) THEN
         CALL H2CHR (8, 1, CATH(KHDOB), BTEMP)
         CALL H2CHR (8, 1, CATH(KHDMP), CTEMP)
         WRITE (PRTEXT,1300) TAPNAM, NFILES, IMTYPE, IMNAME, IMCLAS,
     *      CATBLK(KIIMS), BTEMP, CTEMP
         CALL ZTXIO ('WRIT', PLUN, PIND, PRTEXT(:64), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1301) IERR
            CALL MSGWRT (8)
            END IF
         GO TO 999
         END IF
C                                       Terse message.
      IF (IPTLEV.LE.-1) THEN
         WRITE (PRTEXT,1200) BTEMP, IMTYPE, IMNAME, IMCLAS,
     *      CATBLK(KIIMS)
         CALL PRTPLI (3, IERR)
C                                       Decode Observation and map date
         CALL H2CHR (8, 1, CATH(KHDOB), CTEMP)
         CALL DATDAT (CTEMP, LDATE)
         CALL H2CHR (8, 1, CATH(KHDMP), CTEMP)
         CALL DATDAT (CTEMP, MDATE)
C                                       get number of axies
         NAX = CATBLK(KIDIM)
C                                       assume no frequency
         ISFREQ = .FALSE.
C                                       For all axis
         DO 50 I = 1,NAX
            KPT = KHCTP+(I-1)*2
C                                       Iranslate label
            CALL H2CHR (8, 1, CATH(KPT), CTEMP)
C                                       If Frequency
            IF (CTEMP(1:4).EQ.'FREQ') THEN
C                                       Record frequency
               FREQ  = CATD(KDCRV-1+I)
               ISFREQ = .TRUE.
               END IF
C                                       End for all axies
 50         CONTINUE
C                                       If a frequency in header
         IF (ISFREQ) THEN
C                                       Check if visibilities
            IBS = CATBLK(KIGCN)
            J = CATBLK(KIPCN)
            IF (IBS.GT.0 .AND. J.GE.0) THEN
C                                       Write number of vis.
               WRITE (PRTEXT,1041) LDATE, FREQ, IBS
            ELSE
C                                       Else write image size
               WRITE (PRTEXT,1042) LDATE, FREQ,
     *            (CATBLK(I+KINAX-1), I=1,NAX)
               END IF
         ELSE
C                                       Else write image size
            WRITE (PRTEXT,1043) LDATE, (CATBLK(I+KINAX-1), I=1,NAX)
            END IF
         CALL PRTPLI (3, IERR)
         GO TO 999
C                                       End if terse message
         END IF
C                                       short message only
      IF (IPTLEV.LT.0) THEN
         NAX = MIN (CATBLK(KIDIM), KICTPN)
         NAX = NAX + KINAX - 1
C                                       if wide print out
         IF (NCR.GE.120) THEN
            WRITE (PRTEXT,1000) BTEMP, IMTYPE, IMNAME, IMCLAS,
     *         CATBLK(KIIMS), CATR (KRDMN), CATR(KRDMX),
     *         (CATBLK(I), I = KINAX, NAX)
            IF (ISUV) WRITE (PRTEXT,1001) BTEMP, IMTYPE, IMNAME, IMCLAS,
     *         CATBLK(KIIMS), CATBLK(KIPCN), (CATBLK(I), I = KINAX,NAX)
            CALL PRTPLI (3, IERR)
            GO TO 999
         ELSE
C                                       Else Narrow paper
            WRITE (PRTEXT,1010) BTEMP, IMTYPE, IMNAME, IMCLAS,
     *         CATBLK(KIIMS)
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
            WRITE (PRTEXT,1005) CATR(KRDMN), CATR(KRDMX), (CATBLK(I),
     *         I = KINAX,NAX)
            IF (ISUV) WRITE (PRTEXT,1006) CATBLK(KIPCN), (CATBLK(I),
     *         I = KINAX,NAX)
            CALL PRTPLI (3, IERR)
            END IF
C                                       End if short message
         END IF
      WRITE (PRTEXT,1010) BTEMP, IMTYPE, IMNAME, IMCLAS, CATBLK(KIIMS)
      CALL PRTPLI (2, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Telescope and Receiver
      CALL H2CHR (8, 1, CATH(KHTEL), BTEMP)
      CALL H2CHR (8, 1, CATH(KHINS), CTEMP)
      WRITE (PRTEXT,1020) BTEMP, CTEMP
      CALL PRTPLI (3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Observer and user #
      CALL H2CHR (8, 1, CATH(KHOBS), BTEMP)
      WRITE (PRTEXT,1030) BTEMP, CATBLK(KIIMU)
      CALL PRTPLI (3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Observation and map date
      CALL H2CHR (8, 1, CATH(KHDOB), CTEMP)
      CALL DATDAT (CTEMP, LDATE)
      CALL H2CHR (8, 1, CATH(KHDMP), CTEMP)
      CALL DATDAT (CTEMP, MDATE)
      WRITE (PRTEXT,1040) LDATE, MDATE
      CALL PRTPLI (3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Bit information
      I = 1
      IF (BITPIX.EQ.8) I = 2
      IF (BITPIX.EQ.16) I = 3
      IF (BITPIX.EQ.32) I = 4
      IF (BITPIX.EQ.64) I = 5
      IF (BITPIX.EQ.-32) I = 6
      IF (BITPIX.EQ.-64) I = 7
      WRITE (PRTEXT,1050) BCODE(I)
      CALL PRTPLI (3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Scaling and offset factors
      BS = BSCALE(1)
      BZ = BZERO(1)
      IF (BS.EQ.0.0D0) BS = 1.0D0
      IF ((BS.NE.1.0D0) .OR. (BZ.NE.0.0D0)) THEN
         WRITE (PRTEXT,1060) BS, BZ
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Real minimum and maximum
      CALL H2CHR (8, 1, CATH(KHBUN), BTEMP)
      WRITE (PRTEXT,1070) CATR(KRDMX), CATR(KRDMN), BTEMP
      IF ((CATR(KRDMX).NE.0.0) .OR. (CATR(KRDMN).NE.0.0))
     *   CALL PRTPLI (3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Random axes
      IBS = CATBLK(KIGCN)
      J = CATBLK(KIPCN)
      IF ((IBS.LT.1) .OR. (J.LE.0)) GO TO 90
         CALL H2CHR (2, 1, CATBLK(KITYP), CTYPE)
         WRITE (PRTEXT,1080) IBS, CTYPE
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (PRTEXT,1090)
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (PRTEXT,1082)
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         J = MIN (J, 20)
         DO 85 I = 1,J
            WRITE (PRTEXT,1084) PSCAL(I), POFF(I)
            PRTEXT(12:19) = PTYPES(I)
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
 85         CONTINUE
C                                       Set up loop for axes
 90   NAX = CATBLK(KIDIM)
      WRITE (PRTEXT,1090)
      CALL PRTPLI (3, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (PRTEXT,1091)
      CALL PRTPLI (3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop over axes
      INC = 2
      CLAXIS = -1
      CMAXIS = -1
      DO 150 I = 1,NAX
         KPT = KHCTP+(I-1)*INC
         CALL H2CHR (8, 1, CATH(KPT), BTEMP)
C                                          Check axis type
         IF ((BTEMP(1:4).NE.'LL  ') .AND. (BTEMP(1:4).NE.'RA  ') .AND.
     *      (BTEMP(1:4).NE.'RA--')) GO TO 110
            CALL COORDD (1, CATD(KDCRV-1+I), CH1, HM, SEC)
            DEC = CATR(KRCIC-1+I)*3600.
            CLAXIS = I - 1
            GO TO 130
 110     IF ((BTEMP(1:4).NE.'MM  ') .AND. (BTEMP(1:4).NE.'DEC ') .AND.
     *      (BTEMP(1:4).NE.'DEC-')) GO TO 120
            CALL COORDD (2, CATD(KDCRV-1+I), CH1, HM, SEC)
            DEC = CATR(KRCIC-1+I)*3600.
            CMAXIS = I - 1
            GO TO 130
C                                          No RA or DEC axis
 120     WRITE (PRTEXT,1120) BTEMP, CATBLK(KINAX-1+I), CATD(KDCRV-1+I),
     *      CATR(KRCRP-1+I), CATR(KRCIC-1+I), CATR(KRCRT-1+I)
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         GO TO 150
C                                          RA or DEC axis
 130     WRITE (PRTEXT,1130) BTEMP, CATBLK(KINAX-1+I), CH1, HM, SEC,
     *      CATR(KRCRP-1+I), DEC, CATR(KRCRT-1+I)
         CALL FILZCH (2, 19, PRTEXT)
         CALL FILZCH (2, 22, PRTEXT)
         CALL FILZCH (6, 25, PRTEXT)
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
 150     CONTINUE
      WRITE (PRTEXT,1090)
      CALL PRTPLI (3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                          Maptype and iterations
      IF ((CATBLK(KINIT).LE.0) .AND. (CATR(KRBMJ).LE.0.) .AND.
     *   (CATR(KRBMN).LE.0.)) GO TO 170
         I = MAX (1, CATBLK(KITYP))
         IF (I.GT.4) I = 1
         IF (CATBLK(KINIT).LE.0) I = 5
         WRITE (PRTEXT,1160) PRODUC(I), CATBLK(KINIT)
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
C                                          Beam Parameters
         BEAMS(1) = CATR(KRBMJ) * 3600.0
         BEAMS(2) = CATR(KRBMN) * 3600.0
         BEAMS(3) = CATR(KRBPA)
         WRITE (PRTEXT,1161) BEAMS
         IF ((BEAMS(1).LT.0.5) .AND. (BEAMS(2).LT.0.5))
     *      WRITE (PRTEXT,1162) BEAMS
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Observed RA, Dec
 170  XTEST = 1.0E-8
      IF (CLAXIS.GE.0) XTEST = MAX(ABS(CATR(KRCIC+CLAXIS))/100.,XTEST)
      YTEST = 1.0E-8
      IF (CMAXIS.GE.0) YTEST = MAX(ABS(CATR(KRCIC+CMAXIS))/100.,YTEST)
      IF ((CATD(KDORA).EQ.0.0D0) .AND. (CATD(KDODE).EQ.0.0D0)) GO TO 180
         IF ((CLAXIS.GE.0) .AND. (ABS(CATD(KDORA)-
     *      CATD(KDCRV+CLAXIS)).LT.
     *      XTEST) .AND. (CMAXIS.GE.0) .AND. (ABS(CATD(KDODE)-
     *      CATD(KDCRV+CMAXIS)).LT.YTEST)) GO TO 180
            CALL COORDD (1, CATD(KDORA), CH1, HM, SEC)
            CALL COORDD (2, CATD(KDODE), CH2, DM, DSEC)
            WRITE (PRTEXT,1170) CH1, HM, SEC, CH2, DM, DSEC
            I = 14
            CALL FILZCH (2, I, PRTEXT)
            I = 17
            CALL FILZCH (2, I, PRTEXT)
            I = 20
            CALL FILZCH (6, I, PRTEXT)
            I = 35
            CALL FILZCH (2, I, PRTEXT)
            I = 38
            CALL FILZCH (2, I, PRTEXT)
            I = 41
            CALL FILZCH (6, I, PRTEXT)
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Shifts
 180  IF ((CATR(KRXSH).EQ.0.0) .AND. (CATR(KRYSH).EQ.0.0)) GO TO 190
         IF ((ABS(CATR(KRXSH)).LT.XTEST) .AND. (ABS(CATR(KRYSH))
     *      .LT.YTEST)) GO TO 190
         SEC = 3600. * CATR(KRXSH)
         DSEC = 3600. * CATR(KRYSH)
         WRITE (PRTEXT,1180) SEC, DSEC
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Alternative axis type
 190  IF (CATBLK(KIALT).EQ.0) GO TO 200
         I = CATBLK(KIALT)/256 + 1
         J = CATBLK(KIALT) - (I-1) * 256
         IF ((I.LT.1) .OR. (I.GT.2) .OR. (J.LT.1) .OR. (J.GT.3))
     *      GO TO 200
            BS = CATD(KDRST) / 1.D6
            WRITE (PRTEXT,1190) BS, VTYA(I), VTYB(J)
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
            WRITE (PRTEXT,1191) CATD(KDARV), CATR(KRARP)
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
C
 200  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Image=',A8,' (',A2,')','  Name=',A12,'.',A6,'.',I4,
     *   '  Min,max=',2(1PE11.3),'  Size=',2I5,5I4)
 1001 FORMAT ('Image=',A8,'  (',A2,')','   Name=',A12,'.',A6,'.',I4,
     *   '   # parms=',I3,'   Size=',7I3)
 1005 FORMAT ('Min,max=',2(1PE11.3),'  Size=',2I5,5I4)
 1006 FORMAT ('# parms=',I3,'   Size=',7I3)
 1010 FORMAT ('Image=',A8,'  (',A2,')',9X,'Filename=',A12,'.',A6,
     *   '.',I4)
 1020 FORMAT ('Telescope=',A8,11X,'Receiver=',A8)
 1030 FORMAT ('Observer=',A8,12X,'User #=',I5)
 1040 FORMAT ('Observ. date=',A12,4X,'Map date=',A12)
 1041 FORMAT ('Observ. date=',A12,' Freq=',1PE14.7,', # vis=',I10)
 1042 FORMAT ('Observ. date=',A12,' Freq=',1PE14.7,': ',7I6)
 1043 FORMAT ('Observ. date=',A12,': ',7I6)
 1050 FORMAT ('Pixel type: ',A10)
 1060 FORMAT ('Map scale=',1PE15.8,4X,'Map offset=',E15.8)
 1070 FORMAT ('Maximum=',1PE15.8,6X,'Minimum=',E15.8,1X,A8)
 1080 FORMAT ('# visibilities',I10,5X,'Sort order  ',A2)
 1082 FORMAT ('Rand axes: Type',10X,'Pscale',10X,'Pzero')
 1084 FORMAT (20X,2(2X,1PE14.7))
 1090 FORMAT (2('--------------------------------'))
 1091 FORMAT ('Type    Pixels   Coord value     at Pixel ',
     *   '    Coord incr   Rotat')
 1120 FORMAT (A8,I6,2X,1PE14.7,0PF11.2,1PE15.7,0PF8.2)
 1130 FORMAT (A8,I6,3X,A1,I2,I3,F7.3,F11.2,F15.4,F8.2)
 1160 FORMAT ('Map type=',A8,12X,'Number of iterations=',I8)
 1161 FORMAT ('Conv size=',F7.2,' X',F7.2,3X,'Position angle=',F7.2)
 1162 FORMAT ('Conv size=',F8.5,' X',F8.5,3X,'Position angle=',F7.2)
 1170 FORMAT ('Observed RA ',A1,I2,I3,F7.3,4X,'Dec ',A1,I2,I3,F6.2)
 1180 FORMAT ('Phase shifted in X',F9.3,2X,'in Y',F9.3)
 1190 FORMAT ('Rest freq',F11.3,9X,'Vel type: ',A7,' wrt ',A4)
 1191 FORMAT ('Alt ref. value',1PE13.5,2X,'wrt pixel',0PF8.2)
 1200 FORMAT ('Image=',A8,' (',A2,')',' Name=',A12,'.',A6,'.',I4)
 1300 FORMAT (A12,I6,1X,A2,1X,A12,1X,A6,I5,1X,A8,1X,A8)
 1301 FORMAT ('ZTXIO RETURNS ERROR',I5)
      END
      SUBROUTINE PRTUVT (ICARD, NCOLS, ND, NAXI, IRET)
C-----------------------------------------------------------------------
C   PRTUVT lists the contents of a UV binary table file header on the
C   printer.  It assumes that it needs to read all tape records
C   containing the table header and leaves the CATBLK set for and the
C   tape positioned at the binary data for skipping by the calling
C   routine.
C   Input:
C      ICARD   I      Buffer card number of last processed card image
C      NCOLS   I      Number of columns in table
C      ND      I      number of axes
C      NAXI    I(7)   Number points each axis
C   Output:
C      IRET    I      not 0 => please quit
C-----------------------------------------------------------------------
      INTEGER   ICARD, NCOLS, ND, NAXI(7), IRET
C
      INTEGER   MXTBKW
      PARAMETER (MXTBKW = 200)
      CHARACTER PTYPES(20)*8, DIMCRD*80, CHNCOL*3, KEYWRD(MXTBKW)*8,
     *   KEYCHR(MXTBKW)*8, IMNAME*18, SORT*2, DATMAP*8
      INTEGER   PCOUNT, NDIM, INC, NC, JJ, IEREOF, TABLES, I, GROUP,
     *   GCOUNT, JER, DIMSAV(2), J, OP, IERR, NC1, NUMKEY, NP,
     *   KEYTYP(MXTBKW), KEYV(2*MXTBKW), IMSEQ
      DOUBLE PRECISION    X, PSCAL(20), POFF(20), KEYVAL(MXTBKW)
      LOGICAL   EQUAL, DOIT, LEND
      INCLUDE 'PRTTP.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DBHD.INC'
      EQUIVALENCE (KEYVAL, KEYV)
      DATA IEREOF /4/
C-----------------------------------------------------------------------
      CALL H2CHR (18, KHIMNO, CATH(KHIMN), IMNAME)
      CALL H2CHR (2, 1, CATBLK(KITYP), SORT)
      CALL H2CHR (8, 1, CATH(KHDMP), DATMAP)
      IMSEQ = CATBLK(KIIMS)
C                                       Init parms
      WRITE (DIMCRD,1000) NCOLS
      IF (NCOLS.LT.10) THEN
         CHNCOL = DIMCRD(3:3)
      ELSE IF (NCOLS.LT.100) THEN
         CHNCOL = DIMCRD(2:3)
      ELSE
         CHNCOL = DIMCRD(1:3)
         END IF
      PCOUNT = 0
      NDIM = 0
      GROUP = 0
      GCOUNT = 1
      TABLES = 0
      NC1 = ICARD + 1
C                                       Init header
      DO 15 I = 1,20
         PSCAL(I) = 0.0D0
         POFF(I) = 0.0D0
         PTYPES(I) = ' '
 15      CONTINUE
      CALL SETDEF
      CALL CATINI (CATBLK)
      CATBLK(KIDIM) = ND
      CALL COPY (ND, NAXI, CATBLK(KINAX))
      IRET = 0
C                                       Read header loop
      DO 50 JJ = 1,30000000
         MSGTXT = ' '
         IF (JJ.NE.1) THEN
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            CALL PRTPLI (4, IRET)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR
               IF (IERR.EQ.IEREOF) GO TO 995
               GO TO 990
               END IF
            IF (IRET.NE.0) GO TO 999
            NC1 = 1
            NRECS = NRECS + 1
            BYTES = BYTES + 2880
            FNRECS = FNRECS + 1
            FBYTES = FBYTES + 2880
            CALL ZC8CL (2880, 1, TAPBUF(TBIND), CBUF)
            END IF
C                                       Loop over cards in block
         PRTEXT (1:NCR) = ' '
         DO 40 NC = NC1,36
C                                       Print the card first
            INC = (NC-1) * 80 + 1
            PRTEXT (1:80) = CBUF(INC:INC+79)
            IF (PRTEXT(:4).EQ.'TDIM') DIMCRD = PRTEXT(:80)
            IF (PRTEXT(:7).EQ.'TFIELDS') THEN
               OP = 11
               CALL GETNUM (PRTEXT, 80, OP, X)
               IF (X.NE.DBLANK) NCOLS = X + 0.5D0
               END IF
            EQUAL = PRTEXT(1:8).EQ.'HISTORY '
            DOIT = (IPTLEV.GE.2) .OR. ((IPTLEV.EQ.1) .AND.
     *         (.NOT.EQUAL) .AND. (PRTEXT(1:4).NE.'    '))
            EQUAL = PRTEXT(1:8).EQ.'ORIGIN  '
            LEND = PRTEXT(1:8).EQ.'DATE    '
            DOIT = ((DOIT) .OR. (LEND) .OR. (EQUAL)) .AND. (IPTLEV.GE.0)
            IF ((PRTEXT(:8).EQ.'EXTNAME ') .AND. (IPTLEV.GE.-1)) DOIT =
     *         .TRUE.
            IF (DOIT) CALL PRTPLI (3, IRET)
            IF (IRET.NE.0) GO TO 999
            LEND = PRTEXT(:8).EQ.'END'
            IF (LEND) GO TO 55
            CALL TPARSE (NC, NUMKEY, KEYWRD, KEYVAL, KEYCHR, KEYTYP,
     *         CBUF, IERR)
 40         CONTINUE
 50      CONTINUE
C                                       END found
 55   DIMSAV(1) = CATBLK(KINAX)
      DIMSAV(2) = CATBLK(KINAX+1)
C                                       Piecewise UV file
      IF ((NPV(1).GT.0) .AND. (NPV(2).GT.0)) THEN
         WRITE (PRTEXT,1055) NPV(1), NPV(2)
         CALL PRTPLI (2, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (PRTEXT,1056) NPV(3), NPV(4)
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       build header
      CATBLK(KIDIM) = MAXIS
      NP = 1
      DO 60 I = 1,CATBLK(KIDIM)
         NP = NP * MAXISI(I)
         CATBLK(KINAX+I-1) = MAXISI(I)
         CATR(KRCIC+I-1) = CDELT(I)
         CATR(KRCRP+I-1) = CRPIX(I)
         CATR(KRCRT+I-1) = CROTA(I)
         CATD(KDCRV+I-1) = CRVAL(I)
         CALL CHR2H (8, CTYPE(I), 1, CATH(KHCTP+2*(I-1)))
 60      CONTINUE
      CATD(KDORA) = OBSRA
      CATD(KDODE) = OBSDEC
      CALL CHR2H (8, DATOBS, 1, CATH(KHDOB))
      CALL CHR2H (8, DATMAP, 1, CATH(KHDMP))
      CALL CHR2H (8, TELESC, 1, CATH(KHTEL))
      CALL CHR2H (8, OBSRVR, 1, CATH(KHOBS))
      CALL CHR2H (8, BUNIT, 1, CATH(KHBUN))
      CALL CHR2H (8, OBJECT, 1, CATH(KHOBJ))
      CALL CHR2H (18, IMNAME, KHIMNO, CATH(KHIMN))
      CATBLK(KIIMS) = IMSEQ
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CALL CHR2H (2, SORT, 1, CATBLK(KITYP))
      CATR(KREPO) = CEPOCH
      CATBLK(KIGCN) = MAX (NPV(4), DIMSAV(2))
      CATBLK(KIPCN) = NCOLS - 1
      J = 0
      DO 65 I = 1,NCOLS
         IF (TTYPE(I)(:8).NE.'VISIBILI') THEN
            CALL CHR2H (8, TTYPE(I), 1, CATH(KHPTP+J))
            J = J + 2
            END IF
 65      CONTINUE
      CALL PRTHDR (TSCAL, TZERO, TTYPE, IRET)
      CATBLK(KIDIM) = 2
      CATBLK(KINAX) = DIMSAV(1)
      CATBLK(KINAX+1) = DIMSAV(2)
      GO TO 999
C
 990  IRET = 8
 995  CALL MSGWRT (8)
      CALL PRTPLI (4, JER)
      IF ((IERR.EQ.IEREOF) .AND. (IRET.NE.8)) THEN
         MSGTXT = 'UNEXPECTED END OF FILE'
         CALL MSGWRT (6)
         CALL PRTPLI (4, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I3)
 1020 FORMAT ('ERROR',I7,' READING FITS TAPE')
 1055 FORMAT ('Piece',I3,' of',I3,' in the full data set')
 1056 FORMAT ('Visibilities',I9,' through',I9,' from original')
      END
      SUBROUTINE PRTVLB (IERR)
C-----------------------------------------------------------------------
C   PRTVLB lists the contents of a VLBA binary table file header
C   on the printer.
C   Output:
C      IERR     I         not 0 => please quit
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER LDATE*12, MDATE*12, BTEMP*8, CTEMP*8, BCODE(7)*10,
     *   IMTYPE*2, IMNAME*12, IMCLAS*6
      INTEGER   I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PRTTP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA BCODE /'UNKNOWN ','PIXEL 8BIT','SHORT INT ',
     *   'LONG INT','DOUBLE INT','FLOATING','DBL FLOAT'/
C-----------------------------------------------------------------------
      MSGTXT = 'Data in VLBA binary tables format'
      CALL MSGWRT (3)
      WRITE (PRTEXT,1000)
      CALL PRTPLI (3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       file output only
      IF (IPTLEV.LE.-3) THEN
         IMNAME = 'VLBA FORMAT'
         IMCLAS = 'TABLES'
         IMTYPE = 'UV'
         CALL H2CHR (8, 1, CATH(KHDOB), BTEMP)
         CALL H2CHR (8, 1, CATH(KHDMP), CTEMP)
         WRITE (PRTEXT,1300) TAPNAM, NFILES, IMTYPE, IMNAME, IMCLAS,
     *      CATBLK(KIIMS), BTEMP, CTEMP
         CALL ZTXIO ('WRIT', PLUN, PIND, PRTEXT(:64), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1301) IERR
            CALL MSGWRT (8)
            END IF
C                                       Terse message.
      ELSE IF (IPTLEV.LE.-1) THEN
C                                       Decode Observation and map date
         CALL H2CHR (8, 1, CATH(KHDOB), CTEMP)
         CALL DATDAT (CTEMP, LDATE)
         CALL H2CHR (8, 1, CATH(KHDMP), CTEMP)
         CALL DATDAT (CTEMP, MDATE)
         WRITE (PRTEXT,1010) LDATE, MDATE
         CALL PRTPLI (3, IERR)
C                                       Telescope and Observer
      ELSE
         CALL H2CHR (8, 1, CATH(KHTEL), BTEMP)
         CALL H2CHR (8, 1, CATH(KHOBS), CTEMP)
         WRITE (PRTEXT,1020) BTEMP, CTEMP
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Observation and map date
         CALL H2CHR (8, 1, CATH(KHDOB), CTEMP)
         CALL DATDAT (CTEMP, LDATE)
         CALL H2CHR (8, 1, CATH(KHDMP), CTEMP)
         CALL DATDAT (CTEMP, MDATE)
         WRITE (PRTEXT,1010) LDATE, MDATE
         CALL PRTPLI (3, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Bit information
         I = 1
         IF (BITPIX.EQ.8) I = 2
         IF (BITPIX.EQ.16) I = 3
         IF (BITPIX.EQ.32) I = 4
         IF (BITPIX.EQ.64) I = 5
         IF (BITPIX.EQ.-32) I = 6
         IF (BITPIX.EQ.-64) I = 7
         WRITE (PRTEXT,1030) BCODE(I)
         CALL PRTPLI (3, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLBA Binary FITS file base header')
 1010 FORMAT ('Observ. date=',A12,4X,'Map date=',A12)
 1020 FORMAT ('Telescope=',A8,11X,'Observer=',A8)
 1030 FORMAT ('Pixel type: ',A10)
 1300 FORMAT (A12,I6,1X,A2,1X,A12,1X,A6,I5,1X,A8,1X,A8)
 1301 FORMAT ('ZTXIO RETURNS ERROR',I5)
      END
      SUBROUTINE PRTVLA (IRET)
C-----------------------------------------------------------------------
C   Index VLA archive format (post Jan88)
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER IRET
C
      CHARACTER PGID*8, DATE*8, CORM*4, BTEMP*8, CTEMP*8, IMTYPE*2,
     *   IMNAME*12, IMCLAS*6
      INTEGER   LIMIT, D10ID, MCBLK, MCNBLK, NANT, SUBA, CHCODE, LOOP,
     *   PECNT, JER, LRWORD
      DOUBLE PRECISION    FREQA, TIME
      LOGICAL   F, FIRST
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'PRTTP.INC'
      INCLUDE 'VLARCH.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Set up FDVEC
      FDVEC(2) = 26624
      FDVEC(6) = 1
      FDVEC(31) = 0
      FDVEC(32) = 0
      PECNT = 0
      FULL = F
      FIRST = .TRUE.
C                                       Initialize
      CALL INDEXR (1, PGID, FREQA, TIME, D10ID, NANT, SUBA, CHCODE,
     *   CORM, DATE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       read first tape record
 100     MSGSUP = 1000
         MSGTXT = ' '
         IF (VLDISK) THEN
            IF (.NOT.FIRST) CALL FLDKIO ('READ', FDVEC, TAPBUF, TBIND,
     *         IRET)
            FIRST = .FALSE.
         ELSE
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IRET)
            END IF
         MSGSUP = 0
         IF (IRET.EQ.10) IRET = 0
C                                       Trap parity errors
         IF (IRET.EQ.3) THEN
            PECNT = PECNT + 1
            IF (PECNT.GT.100) GO TO 300
            GO TO 100
            END IF
         CALL PRTPLI (4, JER)
         IF (IRET.NE.0) GO TO 300
         IF (JER.NE.0) THEN
            IRET = JER
            GO TO 999
            END IF
         NRECS = NRECS + 1
         BYTES = BYTES + FDVEC(42)
         FNRECS = FNRECS + 1
         FBYTES = FBYTES + FDVEC(42)
C                                       Crack header
         CALL HEDCRK (TAPBUF(TBIND), PGID, FREQA, TIME, D10ID, NANT,
     *      SUBA, CHCODE, CORM, DATE, MCBLK, MCNBLK)
C                                       Accumulate info
         IF (MCBLK.EQ.1) THEN
            IF (VLDISK) THEN
               CALL ZI32IL (1, 2, TAPBUF(TBIND), LRWORD)
               LRWORD = 2 * LRWORD + 4 * MCNBLK
               LRWORD = (LRWORD-1) / 2048 + 1
               IF (LRWORD.GE.13) THEN
                  FDVEC(43) = LRWORD - 13
               ELSE
                  FTAB(FDVEC(40)+5) = FTAB(FDVEC(40)+5) + LRWORD - 13
                  IF (MCNBLK.NE.1) THEN
                     MSGTXT = 'PROBLEM IN CORRECTING RECORD ' //
     *                  'POSITION ON DISK'
                     CALL MSGWRT (9)
                     END IF
                  END IF
               END IF
            CALL INDEXR (2, PGID, FREQA, TIME, D10ID, NANT, SUBA,
     *         CHCODE, CORM, DATE, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Skip over rest of log. record
         LIMIT = MCNBLK - MCBLK
         DO 200 LOOP = 1,LIMIT
            MSGSUP = 1000
            MSGTXT = ' '
            IF (VLDISK) THEN
               CALL FLDKIO ('READ', FDVEC, TAPBUF, TBIND, IRET)
            ELSE
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IRET)
               END IF
            MSGSUP = 0
            IF (IRET.EQ.10) IRET = 0
C                                       Trap parity error
            IF (IRET.EQ.3) THEN
               PECNT = PECNT + 1
               IF (PECNT.GT.100) GO TO 300
               IRET = 0
               END IF
            CALL PRTPLI (4, JER)
            IF (IRET.NE.0) GO TO 300
            IF (JER.NE.0) THEN
               IRET = JER
               GO TO 999
               END IF
            NRECS = NRECS + 1
            BYTES = BYTES + FDVEC(42)
            FNRECS = FNRECS + 1
            FBYTES = FBYTES + FDVEC(42)
 200        CONTINUE
         GO TO 100
C                                       Give results
 300  IF (NOPGM.GE.1) THEN
         CALL INDEXR (3, PGID, FREQA, TIME, D10ID, NANT, SUBA, CHCODE,
     *      CORM, DATE, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Parity errors
      IF (PECNT.GT.0) THEN
         WRITE (PRTEXT,1992) PECNT
         CALL PRTPLI (2, IRET)
         END IF
C                                       file output only
      IF (IPTLEV.LE.-3) THEN
         IMNAME = 'VLA ARCHIVE '
         IMCLAS = 'DATA  '
         IMTYPE = 'AR'
         BTEMP = PGID
         CTEMP = DATE
         WRITE (PRTEXT,1995) TAPNAM, NFILES, IMTYPE, IMNAME, IMCLAS,
     *      NOPGM, BTEMP, CTEMP
         CALL ZTXIO ('WRIT', PLUN, PIND, PRTEXT(:64), JER)
         IF (JER.NE.0) THEN
            WRITE (MSGTXT,1996) JER
            CALL MSGWRT (8)
            END IF
         IF (IRET.EQ.0) IRET = JER
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1992 FORMAT ('Encountered ',I8, ' tape parity errors')
 1995 FORMAT (A12,I6,1X,A2,1X,A12,1X,A6,I5,1X,A8,1X,A8)
 1996 FORMAT ('ZTXIO RETURNS ERROR',I5)
      END
      SUBROUTINE HEDCRK (BUFFER, PGID, FREQA, TIME, D10ID, NANT, SUBA,
     *   CHCODE, CORM, DATE, MCBLK, MCNBLK)
C-----------------------------------------------------------------------
C   Routine to interprete header for VLA archive data.
C   Input:
C      BUFFER   I(*)   Buffer containing header.
C   Output:
C      PGID     C*8    Observing program name
C      FREQA    D      IF A Center frequency (GHz)
C      TIME     D      Time (radians) IAT of geometry calculations.
C      D10ID    I      Dec 10 Id number
C      NANT     I      Number of antennas
C      SUBA     I      Subarray number
C      CHCODE   I      Channel code.
C      CORM     C*4    Correlator mode.
C      DATE     C*8    Date as 'dd/mm/yy' packed.
C      MCBLK    I      Block No. (better be 1)
C      MCNBLK   I      Number of tape blocks in logical record.
C-----------------------------------------------------------------
      CHARACTER DATE*8, PGID*8, CORM*4
      INTEGER   BUFFER(*), D10ID, NANT, SUBA, CHCODE, MCBLK, MCNBLK
      DOUBLE PRECISION    FREQA, TIME
C
      INTEGER   NDATA, INDEX, MCSUBA, MJDATE
      DOUBLE PRECISION    R8(2), XDATE
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Block numbers
      NDATA = 1
      INDEX = 1
      CALL ZI16IL (NDATA, INDEX, BUFFER, MCBLK)
      INDEX = 2
      CALL ZI16IL (NDATA, INDEX, BUFFER, MCNBLK)
C                                       Validity check
      IF (MCBLK.NE.1) GO TO 999
C                                       Number of antennas
      INDEX = 20
      CALL ZI16IL (NDATA, INDEX, BUFFER, NANT)
C                                       Subarray area pointer
      INDEX = 8
      CALL ZI32IL (NDATA, INDEX, BUFFER, MCSUBA)
C                                       Program ID
      NDATA = 6
      INDEX = (MCSUBA + 11) * 2 + 5
      CALL ZC8CL (NDATA, INDEX, BUFFER, PGID)
C                                       Dec 10 user number
      NDATA = 1
      INDEX = MCSUBA + 17
      CALL ZI16IL (NDATA, INDEX, BUFFER, D10ID)
C                                       Subarray
      NDATA = 1
      INDEX = MCSUBA + 3
      CALL ZI16IL (NDATA, INDEX, BUFFER, SUBA)
C                                       Channel code
      NDATA = 1
      INDEX = MCSUBA + 21
      CALL ZI16IL (NDATA, INDEX, BUFFER, CHCODE)
C                                       Correlator mode
      NDATA = 4
      INDEX = (MCSUBA + 157) * 2 + 5
      CALL ZC8CL (NDATA, INDEX, BUFFER, CORM)
C                                       Date
      NDATA = 1
      INDEX = 4
      CALL ZI32IL (NDATA, INDEX, BUFFER, MJDATE)
      XDATE = MJDATE + 2400000.5D0
      CALL GREG (XDATE, DATE)
C                                       Frequency
      NDATA = 8
      INDEX = (MCSUBA + 56) * 2 + 5
      CALL ZBYMOV (NDATA, INDEX, BUFFER, 1, R8)
      NDATA = 4
      CALL ZBYTFL (NDATA, R8, R8)
      NDATA = 1
      CALL ZDM2DL (NDATA, R8, FREQA)
C                                       Time
      NDATA = 8
      INDEX = (MCSUBA + 72) * 2 + 5
      CALL ZBYMOV (NDATA, INDEX, BUFFER, 1, R8)
      NDATA = 4
      CALL ZBYTFL (NDATA, R8, R8)
      NDATA = 1
      CALL ZDM2DL (NDATA, R8, TIME)
C
 999  RETURN
      END
      SUBROUTINE INDEXR (IOP, PGID, FREQA, TIME, D10ID, NANT, SUBA,
     *   CHCODE, CORM, DATE, IERR)
C-----------------------------------------------------------------------
C   Routine to do accounting for VLA archive data
C   Input:
C      IOP      I        Op code, 1=init, 2=accumulate, 3=summarize
C      PGID     C*8      Observing program name
C      FREQA    D        IF A Center frequency (GHz)
C      TIME     D        Time (radians) IAT of geometry calculations.
C      D10ID    I        Dec 10 Id number
C      NANT     I        Number of antennas
C      SUBA     I        Subarray number
C      CHCODE   I        Channel code.
C      CORM     C*4      Correlator mode.
C      DATE     C*8      Date as 'dd/mm/yy' packed.
C   Input/Output values in common /TAPE/:
C      NOPGM    I        Number of entries
C      NOREC    I(*)     Number of records per entry
C      NOVIS    I(*)     Approx. number of vis per entry
C      D10IDA   I(*)     Dec 10 IDs
C      SUBAA    I(*)     Subarray numbers
C      CHCODA   I(*)     Channel codes
C      CORMA    C*4(*)   Correlator modes
C      DATEA    C*8(*)   Date as 'dd/mm/yy'
C      PGIDA    C*8(*)   Program names
C      FREQAA   D(*)     IF A Frequency (GHz)
C      TIMERA   D(2,*)   Timerange (Radians)
C   Output:
C      IERR     I        Return error code 0=>OK
C-----------------------------------------------------------------------
      INTEGER   IOP, D10ID, NANT, SUBA, CHCODE, IERR
      CHARACTER PGID*8, CORM*4, DATE*8
      DOUBLE PRECISION    FREQA, TIME
C
      CHARACTER CH1*1, CH2*1, PGEXP*8
      INTEGER   LOOP, THEPGM, HM1(2), HM2(2)
      LOGICAL   EQUAL
      REAL      SEC1, SEC2
      DOUBLE PRECISION    TT(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PRTTP.INC'
      INCLUDE 'VLARCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Initialize
      IF (IOP.EQ.1) THEN
         NOPGM = 0
         DO 100 LOOP = 1,2000
            FREQAA(LOOP) = 0.0D0
            TIMERA(1,LOOP) = 0.0D0
            TIMERA(2,LOOP) = 0.0D0
            PGIDA(LOOP) = ' '
            DATEA(LOOP) = ' '
            CORMA(LOOP) = ' '
            NOREC(LOOP) = 0
            NOVIS(LOOP) = 0
            SUBAA(LOOP) = 0
            CHCODA(LOOP) = 0
 100        CONTINUE
         GO TO 999
         END IF
C                                       Make entry
      IF (IOP.EQ.2) THEN
C                                       See if already have
         DO 200 LOOP = 1,NOPGM
C                                       Program name
            EQUAL = PGID(1:6) .EQ. PGIDA(LOOP)
            IF (.NOT.EQUAL) GO TO 200
C                                       Frequency (10%)
            IF (ABS (FREQA-FREQAA(LOOP)) .GT. (0.1*FREQA)) GO TO 200
C                                       Dec 10 ID
            IF (D10ID.NE.D10IDA(LOOP)) GO TO 200
C                                       Subarray
            IF (SUBA.NE.SUBAA(LOOP)) GO TO 200
C                                       Channel code
            IF (CHCODE.NE.CHCODA(LOOP)) GO TO 200
C                                       Correlator mode
            IF (CORM.NE.CORMA(LOOP)) GO TO 200
C                                       Match
            THEPGM = LOOP
            GO TO 300
 200        CONTINUE
C                                       New entry
C                                       Make sure there is room
         IF (NOPGM.GE.2000) THEN
            IF (.NOT.FULL) THEN
               WRITE (MSGTXT,1200)
               CALL MSGWRT (6)
               CALL PRTPLI (4, IERR)
               FULL = .TRUE.
               GO TO 999
            ELSE
               GO TO 999
               END IF
            END IF
         NOPGM = NOPGM + 1
         PGIDA(NOPGM)(1:6) = PGID(1:6)
         FREQAA(NOPGM) = FREQA
         D10IDA(NOPGM) = D10ID
         SUBAA(NOPGM) = SUBA
         CHCODA(NOPGM) = CHCODE
         CORMA(NOPGM) = CORM
         DATEA(NOPGM) = DATE
         TIMERA(1,NOPGM) = TIME
         TIMERA(2,NOPGM) = TIME
         NOREC(NOPGM) = 1
         NOVIS(NOPGM) = (NANT * NANT) / 2
         GO TO 999
C                                       Old entry
 300     TIMERA(2,THEPGM) = TIME
         NOREC(THEPGM) = NOREC(THEPGM) + 1
         NOVIS(THEPGM) = NOVIS(THEPGM) + (NANT * NANT) / 2
         GO TO 999
         END IF
C                                       Give results
      IF (IOP.EQ.3) THEN
C                                       Print heading
         CALL PRTPLI (1, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Tell user how to read
         WRITE (PRTEXT,1301)
         CALL PRTPLI (2, IERR)
         IF (IERR.NE.0) GO TO 999
         DO 400 LOOP = 1,NOPGM
            PGEXP(1:6) = PGIDA(LOOP)(1:6)
            WRITE (PRTEXT,1302)  PGEXP, D10IDA(LOOP), FREQAA(LOOP)
            CALL PRTPLI (2, IERR)
            IF (IERR.NE.0) GO TO 999
            WRITE (PRTEXT,1303) SUBAA(LOOP), CHCODA(LOOP),
     *         CORMA(LOOP)
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
            TT(1) = TIMERA(1,LOOP) * 57.29577951
            TT(2) = TIMERA(2,LOOP) * 57.29577951
            CALL COORDD (1, TT(1), CH1, HM1, SEC1)
            CALL COORDD (1, TT(2), CH2, HM2, SEC2)
            WRITE (PRTEXT,1304) DATEA(LOOP), CH1, HM1, SEC1,
     *         CH2, HM2, SEC2
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
            WRITE (PRTEXT,1305) NOREC(LOOP), NOVIS(LOOP)
            CALL PRTPLI (3, IERR)
            IF (IERR.NE.0) GO TO 999
 400        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('WARNING: EXCEEDED LIMIT OF 2000 ENTRIES')
 1301 FORMAT ('Read this data with AIPS task FILLM')
 1302 FORMAT (' Obs. program = ',A6,' Obs. ID =',I4,
     *   ', Freq(Ghz) =', F10.5)
 1303 FORMAT (' Subarray =',I2,' Channel code =',I7,
     *   ' Correlator mode = ',A4)
 1304 FORMAT (' Timerange = ',A8,' at ',A1,2I3,F5.1,' to ',A1,2I3,F5.1)
 1305 FORMAT (I6,' VLA archive records, approximately ',I10,' vis.')
      END
      SUBROUTINE PRTXXX (IRET)
C-----------------------------------------------------------------------
C   PRTXXX prints information about files of unknown type.  Basically
C   all it can do is report physical record lengths and counts.
C   Output:
C      IRET   I   Error code: 0 => okay,
C                   > 0 I/O error
C                   < 0 requested to quit by user
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER BTEMP*8, CTEMP*8, IMTYPE*2, IMNAME*12, IMCLAS*6
      INTEGER   NBYTES, IBYTES, LBYTES, IREC, LREC, LUN, FIND, I, IERR,
     *   IEREOF, JER
      INCLUDE 'PRTTP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IEREOF /4/
C-----------------------------------------------------------------------
C                                       Init printer
      CALL PRTPLI (1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Init parms
      NBYTES = 29184
      NBYTES = (NBYTES * NBITWD) / 8
      NBYTES = MIN (32767, NBYTES)
      LBYTES = 0
      LREC = 0
      LUN = FDVEC(1)
      FIND = FDVEC(40)
C                                       loop forever almost
      DO 100 IREC = 1,100000000
         MSGTXT = ' '
         CALL ZTPMIO ('READ', LUN, FIND, NBYTES, TAPBUF, 1, IRET)
         IF (IRET.EQ.0) CALL ZTPWAT (LUN, FIND, 1, IBYTES, IRET)
         IF (IRET.NE.0) IBYTES = -1
         CALL PRTPLI (4, JER)
         IERR = 0
         IF (IBYTES.NE.LBYTES) THEN
            IF (LREC.GT.0) THEN
               I = IREC - 1
               WRITE (PRTEXT,1000) LREC, I, LBYTES
               CALL PRTPLI (3, IERR)
               END IF
            LREC = IREC
            LBYTES = IBYTES
            END IF
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.IEREOF) THEN
                WRITE (PRTEXT,1010) IREC
                CALL PRTPLI (2, IERR)
                IRET = IERR
                GO TO 110
             ELSE
                WRITE (MSGTXT,1015) IRET, IREC
                CALL MSGWRT (7)
                CALL PRTPLI (4, IERR)
                END IF
             END IF
         IF (IRET.EQ.0) IRET = IERR
         IF (IRET.EQ.0) IRET = JER
         IF (IRET.NE.0) GO TO 999
 100     CONTINUE
C                                       file output only
 110  IF (IPTLEV.LE.-3) THEN
         IMNAME = 'UNKNOWN TYPE'
         IMCLAS = 'FORMAT'
         IMTYPE = '??'
         BTEMP = 'EOF AT'
         WRITE (CTEMP, 1110) IREC
         WRITE (PRTEXT,1300) TAPNAM, NFILES, IMTYPE, IMNAME, IMCLAS,
     *      LBYTES, BTEMP, CTEMP
         CALL ZTXIO ('WRIT', PLUN, PIND, PRTEXT(:64), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1301) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('   Record',I9,' through',I9,' length',I6,' bytes')
 1010 FORMAT ('   End-of-file at record',I9)
 1015 FORMAT ('PRTXXX: TAPE ERROR CODE',I2,' AT RECORD',I9)
 1110 FORMAT (I8)
 1300 FORMAT (A12,I6,1X,A2,1X,A12,1X,A6,I5,1X,A8,1X,A8)
 1301 FORMAT ('ZTXIO RETURNS ERROR',I5)
      END
      SUBROUTINE FLDKIO (OP, FDVEC, BUFF, IBIND, IERR)
C-----------------------------------------------------------------------
C   FLDKIO is for reading disk files for FILLM
C   NOTE: FLDKIO WORKS IN REAL (8-BIT) BYTES, NOT THE AIPS HALF-INTEGER
C   "BYTES".
C   Usage notes:
C     1) Zero fill FDVEC before filling in relevant values.
C     2) Opening the file.  If FLDKIO determines that the file is not
C        open it will do so.  Once the file is open the file descriptor
C        vector FDVEC must be used in each call.
C     3) Initialization.  FLDKIO initializes the I/O using the values in
C        FDVEC when it opens the file.
C        If OP='OPRD' the file is opened but I/O is not initialized;
C        this allows positioning tapes before the actual I/O starts.
C     5) Closing the file.  The file may be closed with a call with
C        opcode 'CLOS'.
C   31 <= LUN <= 30 + NTAPED will generate an error.
C   Disks: any other LUN, for VLA blocking (n * 2048)
C          FLDKIO requires that FDVEC(2) = 2048, FDVEC(31) = 0 on input
C          FLDKIO sets FDVEC(6) = 1
C          The desired file name must be in FDVEC(7-30) packed string
C          WITHOUT THE FILE NUMBER which will be appended
C   Inputs:
C      OP     C*4     Operation code: 'READ','CLOS', 'OPRD'
C   Input/Output:
C      FDVEC  I(50)   File descriptor vector.
C                     1 = LUN to use, set before first call.
C                         129-NTAPED to 128 => tape => error
C                     2 = Logical record length in bytes (8-bit)
C                     3 = Buffer size in 8-bit bytes I
C                     4 =
C                     5 =
C                     6 =
C                  7-30 = File name for disk files (24 char. packed)
C                    31 =
C                    32 =
C                    33 = Desired file number
C                 34-39   Reserved for future use
C                The following are used by FLDKIO:
C                    40 = FTAB pointer
C                    41 =
C                    42 = LBYTES - number of bytes read
C                    43 = Number of 2048's left to read
C                 44-50 = reserved for future use
C      BUFF   R(*)   Buffer for I/O must be large enough for the largest
C                    transfer rounded up to the next larger number of
C                    disk blocks.
C   Outputs:
C      IBIND  I      The location in BUFF of the start of the next
C                    record. Note: IBIND points to the address in the
C                    I   array irregardless of the actual data type.
C      IERR   I      Error return: 0 => ok
C                             2 => input error
C                             3 => i/o error on initialize
C                             4 => end of file
C                             5 => beginning of medium
C                             6 => end of medium
C                             7 => Buffer too small
C                             8 => error opening file.
C                            10 => data record shorter than 1 logical
C   Usage notes: For map i/o the first 16 words in each FTAB entry
C   contain a user table to handle double buffer i/o,  the rest
C   contain system-dependent I/O tables.
C   FTAB user table   entries, with offsets from the FIND pointer are:
C      FTAB + 0 =>  LUN using this entry
C             1 =>
C             2 =>  Number of 8-bit bytes in a logical record
C             3 =>  Number of disk logical records in each transfer (1)
C             4 =>
C             5 =>  Block offset on disk file for next operation I
C             6 =>
C           7-8 =>
C             9 =>  I/O opcode 0=read, 1=write
C            10 =>  1 => tape, 2 => disk
C            11 =>
C            12 =>
C            13 =>  1 => I/O active, else inactive (not initialized).
C            14 =>  number bytes last read/write to buffer 1
C            15 =>  number bytes last read/write to buffer 2
C   To suppress messages about logical and physical record lengths
C   being inconsistent, set MSGSUP to 1000 or greater.
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), BUFF(*), IBIND, IERR
      CHARACTER OP*4
C
      CHARACTER PHNAME*48, STRNG*8
      LOGICAL   DISK, DOREAD
      INTEGER   JERR, IND, JBUF, MBYTE, I, J, NUMMSG, LBYTES, I4TEMP,
     *   BUFSZ4, RECNO, BUFPNT, NBYPWD, ITRIM, IFILE, FCBOFF, MCB(2),
     *   LRW
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C
      SAVE NUMMSG
C-----------------------------------------------------------------------
      IERR = 2
      IBIND = 1
C                                       Initial parameters
      IND = FDVEC(40)
      NBYPWD = NBITWD / 8
C                                       See if tape or disk.
C??                                     BETTER TEST HERE
      DISK = (FDVEC(1).LT.129-NTAPED) .OR. (FDVEC(1).GT.128)
      IF (DISK) THEN
         IF (FDVEC(2).NE.13*2048) GO TO 985
         IF (FDVEC(31).NE.0) GO TO 985
         FDVEC(6) = 1
      ELSE
         MSGTXT = 'FLDKIO: ONLY DOES FILLM DISK I/O'
         GO TO 990
         END IF
      BUFSZ4 = FDVEC(3)
C??                                     CHECK HERE ON DEVTAB VALUE
      IF (MOD (DEVTAB(FDVEC(1)),2).EQ.1) GO TO 999
C                                       Test OPcode
      DOREAD = (OP.EQ.'OPRD') .OR. (OP.EQ.'READ')
      IF ((OP.NE.'READ') .AND. (OP.NE.'CLOS') .AND. (OP.NE.'OPRD')) THEN
         WRITE (MSGTXT,1000) OP
         GO TO 990
         END IF
C                                       Check if CLOSE
      IERR = 0
      IF (OP.EQ.'CLOS') GO TO 900
C                                       Check if Open but I/O inactive
      IF (((IND.GT.0) .AND. (FDVEC(1).EQ.FTAB(IND))) .AND.
     *   (FTAB(IND+13).EQ.1)) GO TO 500
C                                       Open
      IF ((IND.LE.0) .OR. (FDVEC(1).NE.FTAB(IND))) THEN
         PHNAME = ' '
         CALL H2CHR (48, 1, FDVEC(7), PHNAME)
         I = ITRIM (PHNAME)
         IFILE = MAX (1, FDVEC(33))
         WRITE (STRNG,1005) IFILE
         CALL CHTRIM (STRNG, 6, STRNG, J)
         PHNAME(I+1:) = STRNG(:J)
         MSGTXT = 'Opening ' // PHNAME
         IF (MSGSUP.NE.32000) CALL MSGWRT (3)
         CALL ZTPOPN (FDVEC(1), IND, FDVEC(5), PHNAME, 'READ', JERR)
C                                       Save number of blocks in file
         IF (JERR.NE.0) THEN
            IERR = 8
            WRITE (MSGTXT,1010) JERR
            FDVEC(40) = 0
            GO TO 990
            END IF
         FDVEC(40) = IND
         END IF
C                                       Init previously inactive I/O
      FDVEC(42) = 0
      FDVEC(43) = 0
      NUMMSG = -2
C                                       Fill values in FTAB
C                                       No. logical rec. done
      FTAB(IND+12) = 0
C                                       LUN
      FTAB(IND) = FDVEC(1)
C                                       Number of bytes in a logical
C                                       record.
      FTAB(IND+2) = FDVEC(2)
C                                       Number of disk blocks per op.
      FTAB(IND+3) = 1
      FTAB(IND+13) = 0
C                                       Buffer size check
      FTAB(IND+4) = -1
C                                       Buffer too small
      IF (BUFSZ4.LT.13*2048) THEN
         IERR = 7
         I4TEMP = 13 * 2048
         WRITE (MSGTXT,1100) BUFSZ4, I4TEMP
         GO TO 990
         END IF
C                                       Next disk block
      RECNO = 1
      FTAB(IND+5) = RECNO
C                                       Opcode
      FTAB(IND+9) = 0
C                                       Medium type
      FTAB(IND+10) = 2
C                                       Set buffer pointer
      BUFPNT = 1
C                                       Check limited no. records
      FDVEC(41) = -1
C                                       Set I/O active flag
      FTAB(IND+13) = 1
C                                       Done if OP='OPxx'
      IF (OP(1:2).EQ.'OP') GO TO 999
C                                       READ
 500  JBUF = 1
      J = MIN (13, FDVEC(43))
      IF (J.LE.0) J = 13
      MBYTE = 2048 * J
      FTAB(IND+14) = MBYTE
      RECNO = FTAB(IND+5)
      FCBOFF = IND + MOFF
      CALL ZDKMID ('READ', RECNO, FTAB(FCBOFF), BUFF, MBYTE, IERR)
      IF (IERR.EQ.4) GO TO 998
      IF (IERR.NE.0) GO TO 980
C                                       Update block counter
      FTAB(IND+5) = RECNO + J
      FDVEC(43) = FDVEC(43) - J
C                                       Set pointers etc.
      IBIND = 1
      JBUF = 1
      CALL ZTPWAT (FTAB(IND), IND, JBUF, LBYTES, IERR)
      IF (IERR.EQ.4) GO TO 998
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1500) IERR, OP
         GO TO 990
         END IF
C                                       Check bytes read
      FDVEC(42) = LBYTES
      IF (LBYTES.NE.MBYTE) THEN
         I = LBYTES / 2048
         IF (I*2048.NE.LBYTES) GO TO 970
         CALL ZI16IL (2, 1, BUFF, MCB)
         CALL ZI32IL (1, 2, BUFF, LRW)
         LRW = 2 * LRW + 4 * MCB(2)
         LRW = (LRW - 1) / 2048 + 1
         IF ((MCB(1).NE.1) .OR. (LRW.GT.I)) GO TO 970
         IERR = 10
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                       Close
 900  IERR = 8
      FDVEC(42) = 0
      IF ((IND.LE.0) .OR. (FTAB(IND).NE.FDVEC(1))) GO TO 999
      IERR = 0
C                                       Turn off things
      FTAB(IND+4) = 0
      FTAB(IND+12) = 0
      FTAB(IND+11) = 0
      FDVEC(40) = 0
C                                       Close file
      CALL ZTPCLS (FTAB(IND), IND, JERR)
      IF (IERR.EQ.0) IERR = JERR
      IF (IERR.NE.0) GO TO 980
      GO TO 998
C                                       I/O error
 970  WRITE (MSGTXT,1970) LBYTES
      IERR = 4
      GO TO 990
 980  WRITE (MSGTXT,1980) IERR, OP
      GO TO 990
C                                       disk IO error
 985  WRITE (MSGTXT,1985) FDVEC(2), FDVEC(31)
C                                       error messages
 990  CALL MSGWRT (6)
C                                       Mark I/O inactive
 998  FTAB(IND+13) = 0
C                                       Don't wait after error
      FTAB(IND+4) = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLDKIO: OPCODE ',A4,' INVALID')
 1005 FORMAT (I6)
 1010 FORMAT ('FLDKIO: ERROR ',I3,' OPENING FILE')
 1100 FORMAT ('FLDKIO: BUFFER SIZE=',I6,' TOO SMALL, NEED',I6)
 1500 FORMAT ('FLDKIO: ERROR ',I3,2X,A4,'ING FILE')
 1970 FORMAT ('FLDKIO: LBYTES=',I8,' BUT IERR = 0')
 1980 FORMAT ('FLDKIO: ERROR',I3,' DOING I/O WITH OPCODE = ',A4)
 1985 FORMAT ('FLDKIO: INPUT BYTES, LENGTH TYPE=',I7,I3,
     *     ' SHOULD BE  2048, 0')
      END
      SUBROUTINE TPARSE (ICARD, NUMKEY, KEYWRD, KEYVAL, KEYCHR, KEYTYP,
     *   FITBLK, IERR)
C-----------------------------------------------------------------------
C   Parses a table FITS header line.
C   Inputs:
C      ICARD    I        The card number (1-36) in block to interpret
C      FITBLK   C*2880   A block of FITS header data.
C   In/Out:
C      NUMKEY   I        on first input the max. number of keywords,
C                        output the number of arbitrary keyword/value
C                        pairs found.
C      KEYWRD   C(*)*8   Keywords
C      KEYVAL   D(*)     List of arbitrary numeric/logical values:
C                           KEYVAL(n) => Value in D.
C                           logicals coded as -1.0=>F, 1.0=>T.
C      KEYCHR   C(*)*8   List of character keyword values values.
C      KEYTYP   I(*)     Keyword type codes.
C   Outputs:
C      IERR     I        error code 0=ok. 1=error.  -1 too many columns
C   Commons:
C      /EXTHDR/    Extension file values.
C      /THDR/      Header values for a tables extension file.
C-----------------------------------------------------------------------
      CHARACTER KEYWRD(*)*8, KEYCHR(*)*8, FITBLK*(*)
      INTEGER   ICARD, NUMKEY, KEYTYP(*), IERR
      DOUBLE PRECISION KEYVAL(*)
C
      INTEGER   NN
      PARAMETER (NN=35)
      CHARACTER SYMBOL*8, STR*80, TWORD(NN)*8, CARD*80, CVER*1
      DOUBLE PRECISION X
      INTEGER   TLIMIT(NN), N, TABNO, NBYT, MAXKEY, NPNT, NCHAR,
     *   KTYPE, COUNT, JERR, NRCX, ISL, JTRIM, IAX, KBP, NPNTS
      LOGICAL   END, WARN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DBHD.INC'
      SAVE MAXKEY
C                                       Special keywords
      DATA TWORD /   'TFIELDS ', 'EXTNAME ', 'EXTVER  ', 'EXTLEVEL',
     *   'TTYPE   ', 'TBCOL   ', 'TFORM   ', 'TUNIT   ', 'TSCAL   ',
     *   'TZERO   ', 'TNULL   ', 'AUTHOR  ', 'REFERENC', 'NMATRIX ',
     *   'DATE-OBS', 'TELESCOP', 'OBSERVER', 'MAXIS   ', 'CTYPE   ',
     *   'CDELT   ', 'CRPIX   ', 'CRVAL   ', 'TMATX   ', 'TDIM    ',
     *   'xCTYP   ', 'xCRVL   ', 'xCDLT   ', 'xCRPX   ', 'xCROT   ',
     *   'BUNIT   ', 'EPOCH   ', 'EQUINOX ', 'OBSRA   ', 'OBSDEC  ',
     *   'OBJECT  '/
      DATA TLIMIT / 4*0, 7*99, 6*0, 6*99, 99, -99, -99, -99, -99, -99,
     *   6*0/
C-----------------------------------------------------------------------
      IF (NUMKEY.LT.0) THEN
         MAXKEY = -NUMKEY
         NUMKEY = 0
         END IF
C                                       parse the card
      CALL GTWCRD (ICARD, NN, TLIMIT, TWORD, FITBLK, NPNT, CARD,
     *   SYMBOL, TABNO, N, IAX, CVER, END, JERR)
      IF (END) GO TO 990
      NPNTS = NPNT
C                                       Unrecognized Keyword =
      IF (JERR.EQ.1) THEN
C                                       Check keyword limit
         IF (NUMKEY.LT.MAXKEY) THEN
            CALL GETKEY (SYMBOL, CARD, NPNT, NUMKEY, KEYWRD, KEYVAL,
     *         KEYCHR, KEYTYP, JERR)
C                                       Invalid code
            IF ((KEYTYP(NUMKEY).LE.0) .OR. (KEYTYP(NUMKEY).GT.6)) THEN
               MSGTXT = 'TPARSE: KEYWORD ' // SYMBOL //
     *            ' OF INDETERMINATE DATA TYPE'
               CALL MSGWRT (6)
               NUMKEY = NUMKEY - 1
               END IF
C                                       Blew keyword limit
         ELSE
            IF (WARN) THEN
               WRITE (MSGTXT,1000) MAXKEY
               CALL MSGWRT (6)
               WARN = .FALSE.
               END IF
            END IF
C                                       Not normal KEYWORD=value card.
      ELSE IF ((JERR.NE.0) .OR. (CVER.NE.' ')) THEN
C                                       Process selected SINGLDSH
C                                       keywords.
         IF (CARD(1:8).EQ.'SINGLDSH') THEN
            CALL SDTCRD (ICARD, FITBLK, MAXKEY, NUMKEY, KEYWRD,
     *         KEYVAL, KEYCHR, KEYTYP, WARN, NRCX)
C                                       Table type 'SD'
            ITYPE = 'SD'
            END IF
C                                       Decode special table keyword.
C                                       Number of fields.
      ELSE IF (TABNO.EQ.1) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         ITNCOL = X + .5
         IF (ITNCOL.GT.127) THEN
            WRITE (MSGTXT,1010) ITNCOL
            CALL MSGWRT (7)
            MSGTXT = 'WILL KEEP ONLY THE FIRST 127 COLUMNS'
            CALL MSGWRT (7)
            ITNCOL = 127
            END IF
C                                       Extension name.
      ELSE IF (TABNO.EQ.2) THEN
         NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         EXTNAM = STR(1:NCHAR)
C                                       If standard AIPS table type
C                                       then EXTNAM is of the form
C                                       'AIPS XX' where XX is the
C                                       table type.
         IF ((EXTNAM(1:5).NE.'AIPS ').AND.(EXTNAM(1:4).NE.'VLA ')) THEN
            ITYPE = 'UK'
         ELSE
            ITYPE = STR(6:7)
            END IF
C                                       Extension version.
      ELSE IF (TABNO.EQ.3) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         EXTVER = X + .01
C                                       Extension level.
      ELSE IF (TABNO.EQ.4) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         EXTLEV = X + .01
C                                       Field type.
      ELSE IF (TABNO.EQ.5) THEN
         NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         TTYPE(N) = STR(1:NCHAR)
C                                       Starting column.
      ELSE IF (TABNO.EQ.6) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         TBCOL(N) = X + .5
C                                       Field format.
      ELSE IF (TABNO.EQ.7) THEN
         NBYT = 8
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         TFORM(N) = STR(1:NCHAR)
C                                       Decode format.
C                                       3-D table
         CALL TABF3D (TFORM(N), COUNT, KTYPE, JERR)
         TFCODE(N) = KTYPE + 10 * COUNT
C                                       Field units.
      ELSE IF (TABNO.EQ.8) THEN
         NBYT = 8
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         TUNIT(N) = STR(1:NCHAR)
C                                       Field scale.
      ELSE IF (TABNO.EQ.9) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         TSCAL(N) = X
C                                       Field offset.
      ELSE IF (TABNO.EQ.10) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         TZERO(N) = X
C                                       Field null value.
      ELSE IF (TABNO.EQ.11) THEN
         NBYT = 8
         CALL GETSTR (CARD, 24, NBYT, NPNT, STR, NCHAR)
         TNULL(N) = STR(1:NCHAR)
C                                       AUTHOR or REFERENC (Ignore)
      ELSE IF ((TABNO.EQ.12) .OR. (TABNO.EQ.13)) THEN
C                                       # of matrices (VLBA)
      ELSE IF (TABNO.EQ.14) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         NMATRX = X + .01
C                                       Date of observation (VLBA)
      ELSE IF (TABNO.EQ.15) THEN
         NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         CALL DATFST ('F2L', STR)
         NCHAR = JTRIM (STR)
         DATOBS = STR(:NCHAR)
C                                       Telescope (VLBA)
      ELSE IF (TABNO.EQ.16) THEN
         NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         TELESC = STR(1:NCHAR)
C                                       Observer (VLBA)
      ELSE IF (TABNO.EQ.17) THEN
         NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         OBSRVR = STR(1:NCHAR)
C                                       # axes in data matrix (VLBA)
      ELSE IF (TABNO.EQ.18) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         IF (N.EQ.0) THEN
            MAXIS = X + .01
         ELSE
            MAXISI(N) = X + .01
            END IF
C                                       Axis type. (VLBA)
      ELSE IF (TABNO.EQ.19) THEN
         NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         CTYPE(N) = STR(1:NCHAR)
C                                       Axis increment (VLBA)
      ELSE IF (TABNO.EQ.20) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         CDELT(N) = X
C                                       Axis ref. pixel (VLBA)
      ELSE IF (TABNO.EQ.21) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         CRPIX(N) = X
C                                       Value at ref pixel (VLBA)
      ELSE IF (TABNO.EQ.22) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         CRVAL(N) = X
C                                       Column which is a data matrix
      ELSE IF (TABNO.EQ.23) THEN
         CALL GETLG (CARD, 80, NPNT, ISL)
         IF (ISL.EQ.0) TMATX(N) = .FALSE.
         IF (ISL.EQ.1) TMATX(N) = .TRUE.
C                                       TDIMnn card - get axes
      ELSE IF (TABNO.EQ.24) THEN
         IF (TTYPE(N)(:8).NE.'VISIBILI') THEN
            WRITE (MSGTXT,1240) N, TTYPE(N)
            CALL MSGWRT (6)
            END IF
         TMATX(N) = .TRUE.
         NBYT = 80
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
C                                       parse out dimensions
         N = 0
         KBP = 2
 40      IF (STR(KBP:KBP).NE.')') THEN
            N = N + 1
            CALL GETINT (STR, NCHAR, KBP, MAXISI(N))
            IF (MAXISI(N).GT.0) GO TO 40
            N = N - 1
            END IF
         MAXIS = N
C                                       coord type
      ELSE IF (TABNO.EQ.25) THEN
         IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
         NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         CTYPE(IAX) = STR(1:NCHAR)
C                                       coord ref value
      ELSE IF (TABNO.EQ.26) THEN
         IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         CRVAL(IAX) = X
C                                       coord increment
      ELSE IF (TABNO.EQ.27) THEN
         IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         CDELT(IAX) = X
C                                       coord ref pix
      ELSE IF (TABNO.EQ.28) THEN
         IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         CRPIX(IAX) = X
C                                       coord rotation
      ELSE IF (TABNO.EQ.29) THEN
         IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         CROTA(IAX) = X
C                                       BUNIT
      ELSE IF (TABNO.EQ.30) THEN
         NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         BUNIT = STR(1:NCHAR)
C                                       EPOCH/EQUINOX
      ELSE IF ((TABNO.EQ.31) .OR. (TABNO.EQ.32)) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
C                                       special parse for EQUINOX
         IF (X.EQ.DBLANK) THEN
            NPNT = NPNTS
            CALL GETSTR (CARD, 80, 68, NPNT, STR, NCHAR)
            IF (INDEX(STR,'1950').GT.0) THEN
               X = 1950.0D0
            ELSE IF (INDEX(STR,'2000').GT.0) THEN
               X = 2000.0D0
               END IF
            END IF
         IF (X.EQ.DBLANK) GO TO 790
         CEPOCH = X + 0.01
C                                       observed RA
      ELSE IF (TABNO.EQ.33) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         OBSRA = X
C                                       observed Declination
      ELSE IF (TABNO.EQ.34) THEN
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 790
         OBSDEC = X
C                                       OBJECT
      ELSE IF (TABNO.EQ.35) THEN
         NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         OBJECT = STR(1:NCHAR)
         END IF
      GO TO 990
C                                       axis coords not right
 780  IF (.NOT.TMATX(N)) THEN
         WRITE (MSGTXT,1780) N, SYMBOL
      ELSE
         WRITE (MSGTXT,1781) IAX, MAXIS
         END IF
      CALL MSGWRT (7)
      GO TO 990
C                                       BAD VALUE
 790  MSGTXT = 'TPARSE: NUMBER OUT OF RANGE ON ' // SYMBOL
      CALL MSGWRT (6)
C                                       Set common number of keywords
 990  ITANKY = NUMKEY
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TPARSE: BLEW NO. OF KEYWORD LIMIT OF ',I5,' IGNORE REST')
 1010 FORMAT ('TPARSE:',I5,' COLUMNS EXCEEDS AIPS LIMIT OF 127')
 1240 FORMAT ('TPARSE: COLUMN',I3,' TYPE ',A8,' NOT VISIBILI')
 1780 FORMAT ('TPARSE: COLUMN',I3,' NOT DIMENSIONS FOR COORD ',A)
 1781 FORMAT ('TPARSE: COORDINATE AXIS',I4,' LARGER THAN MAX',I3)
      END
