LOCAL INCLUDE 'PATGN.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQOUT, DISKO, NEWCNO, CATBLK(256), JBUFSZ, ICODE,
     *   SCRTCH(512)
      CHARACTER NAMOUT*12, CLAOUT*6, OPCODE*4, ARRAY*8
      HOLLERITH XNAMOU(3), XCLAOU(2), XOPCOD, XOPTYP
      REAL      XMSIZE(2), CELLS(2), XSEQO, XDISKO, CPARM(10),
     *   PBPARM(7), BUFF2(MABFSS)
      COMMON /INPARM/ XMSIZE, CELLS, XNAMOU, XCLAOU, XSEQO, XDISKO,
     *   XOPCOD, XOPTYP, CPARM, PBPARM
      COMMON /PARMS/ SEQOUT, DISKO, NEWCNO, JBUFSZ, ICODE
      COMMON /CHRPRM/ NAMOUT, CLAOUT, OPCODE, ARRAY
      COMMON /BUFRS/ BUFF2, SCRTCH
      COMMON /MAPHDR/ CATBLK
LOCAL END
      PROGRAM PATGN
C-----------------------------------------------------------------------
C! Generates an image containing a specified pattern.
C# Map Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000-2004, 2015-2016
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
C   PATGN creates a user specified pattern.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      IMSIZE         XMSIZE        Image size in pixels.
C      CELLSIZE       CELLS         Pixel size in first two dim.
C      OUTNAME        NAMOUT        Name of the output image
C      OUTCLASS       CLAOUT        Class of the output image.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      OPCODE         OPCODE        User specified opcode.
C      CPARM(10)      CPARM         User specified array.
C      PBPARM(7)      PBPARM        User specified beam pattern array.
C   Programmer Thad A. Polk, June 1984
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'PATGN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'PATGN '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL PATIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL PATSND (IRET)
C                                       Write output file
      IF (IRET.EQ.0) CALL PATHIS
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE PATIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   PATIN gets input parameters for PATGN and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, OLDNAM*12, BLANK*6, DEFNAM*12
      HOLLERITH CATH(256)
      INTEGER   IRET, IERR, NPARM, IROUND
      REAL      CATR(256)
      LOGICAL   T
      INCLUDE 'PATGN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
      DATA BLANK /'      '/
      DATA T /.TRUE./
      DATA DEFNAM /'PATGN MAP   '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 30
      CALL GTPARM (PRGN, NPARM, RQUICK, XMSIZE, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQOUT = IROUND (XSEQO)
      DISKO = IROUND (XDISKO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XOPTYP, ARRAY)
C                                       Init CATBLK.
      CALL CATINI (CATBLK)
C                                       Create new file.
C                                       Put values in CATBLK.
      OLDNAM = DEFNAM
      CALL MAKOUT (OLDNAM, BLANK, 0, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Get CATBLK
      IRET = 4
      CALL PATHED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Set obs. date=current date.
      CATH(KHDOB) = CATH(KHDMP)
      CATH(KHDOB+1) = CATH(KHDMP+1)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PATIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE PATSND (IRET)
C-----------------------------------------------------------------------
C   PATSND accepts an image one row at a time.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IRET,
     *   LUNO, BOTEMP, NXO, NYO, WINO(4), INDO, LIM1, BOO, LIM2, LIM3,
     *   LIM4, LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7),
     *   LIMO, LIMIT, OBIND
      REAL      OUTMAX, OUTMIN, CATR(256)
      DOUBLE PRECISION    CATD(128)
      LOGICAL   T, F, BLNKD
      INCLUDE 'PATGN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA LUNO /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open map file for write
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = F
C                                       Setup for looping
      LIM1 = MAX (1, CATBLK(KINAX))
      LIM2 = MAX (1, CATBLK(KINAX+1))
      LIM3 = MAX (1, CATBLK(KINAX+2))
      LIM4 = MAX (1, CATBLK(KINAX+3))
      LIM5 = MAX (1, CATBLK(KINAX+4))
      LIM6 = MAX (1, CATBLK(KINAX+5))
      LIM7 = MAX (1, CATBLK(KINAX+6))
      IPOS(1) = 1
      LIMO = CATBLK(KINAX) - 1
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = I4
                  DO 300 I3 = 1,LIM3
      IPOS(3) = I3
C                                       Init output file.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOTEMP, IRET)
      BOO = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ, 1,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'WRIT', IRET
         GO TO 990
         END IF
      DO 250 I2 = 1,LIM2
         IPOS(2) = I2
C                                       Write.
         CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
         OBIND = OBIND - 1
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) 'WRIT', IRET
            GO TO 990
            END IF
C                                       Call MAKPAT
         OBIND = OBIND + 1
         CALL MAKPAT (IPOS, BUFF2(OBIND), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1180) IRET
            GO TO 990
            END IF
C                                       Check max, min, blanking.
         LIMIT = OBIND + LIMO
         DO 200 I1 = OBIND,LIMIT
            BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
            IF (BUFF2(I1).NE.FBLANK) THEN
               OUTMAX = MAX (OUTMAX, BUFF2(I1))
               OUTMIN = MIN (OUTMIN, BUFF2(I1))
               END IF
 200        CONTINUE
 250     CONTINUE
C                                       Dump plane to output.
C                                       Flush buffer.
      CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1120) 'FINI', IRET
         GO TO 990
         END IF
C                                       Update CATBLK.
      CATR(KRDMX) = OUTMAX
      CATR(KRDMN) = OUTMIN
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1260) IRET
         GO TO 990
         END IF
 300  CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       Final call to MAKPAT
      IPOS(1) = -1
      CALL MAKPAT (IPOS, BUFF2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('PATSND: ERROR',I3,' OPENING SCRATCH FILE')
 1100 FORMAT ('PATSND: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('PATSND: ',A4,' ERROR',I3)
 1180 FORMAT ('PATSND: MAKPAT ERROR',I3)
 1260 FORMAT ('PATSND: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE PATHIS
C-----------------------------------------------------------------------
C   PATHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, ATIME*8, ADATE*12
      INTEGER   LUN, IERR, TIME(3), DATE(3), I
      LOGICAL   T
      INCLUDE 'PATGN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, NEWCNO, CATBLK, SCRTCH, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       New history
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       IMSIZE
      WRITE (HILINE,2001) TSKNAM, XMSIZE
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       CELLSIZE
      WRITE (HILINE,2002) TSKNAM, CELLS
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       OPCODE
      WRITE (HILINE,2003) TSKNAM, OPCODE
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       CPARM
C                                       fix units
      IF (ICODE.EQ.1) THEN
         CPARM(4) = CPARM(4) * CELLS(1)
      ELSE IF ((ICODE.EQ.2) .OR. (ICODE.EQ.3)) THEN
         CPARM(5) = CPARM(5) * CELLS(1)
         END IF
      DO 15 I= 1,10
         WRITE (HILINE,2004) TSKNAM, I, CPARM(I)
         CALL HIADD (LUN, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
 15      CONTINUE
C                                       Primary beam
      IF ((OPCODE.EQ.'BEAM') .OR. (OPCODE.EQ.'POLY') .OR.
     *   (OPCODE.EQ.'INVB')) THEN
         DO 18 I= 1,7
            IF (OPCODE.EQ.'BEAM') THEN
               WRITE (HILINE,2005) TSKNAM, I, PBPARM(I)
            ELSE IF (OPCODE.EQ.'INVB') THEN
               WRITE (HILINE,2005) TSKNAM, I, PBPARM(I)
            ELSE
               WRITE (HILINE,2006) TSKNAM, I, PBPARM(I)
               END IF
            CALL HIADD (LUN, HILINE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 20
 18         CONTINUE
         END IF
C                                       AIPS release
      WRITE (HILINE,2007) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Close HI file
 20   CALL HICLOS (LUN, T, SCRTCH, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PATHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 2001 FORMAT (A6,'IMSIZE = ', 2F8.0)
 2002 FORMAT (A6,'CELLSIZE = ',2F10.5)
 2003 FORMAT (A6,'OPCODE = ',1H',A4,1H')
 2004 FORMAT (A6,'CPARM(',I2.2,') =',1PE13.5,' / model parameter')
 2005 FORMAT (A6,'PBPARM(',I1,') =',1PE13.5,'  / beam parameter')
 2006 FORMAT (A6,'PBPARM(',I1,') =',1PE13.5,'  / model parameter')
 2007 FORMAT (A6,'RELEASE = ''',A7,' ''')
      END
      SUBROUTINE PATHED (IRET)
C-----------------------------------------------------------------------
C   PATHED (1) Creates the catalog header block to represent the
C   output file.  The catalog block will be updated when the history
C   file is written.  (2) Setting default values of some of the input
C   parameters.
C   Input (common):
C      CATBLK(256)    I     Output catalog header, also CATR, CATD
C   Output:
C      CATBLK(256)    I     Modified output catalog header.
C      IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER CODES(7)*4, UNITS*8, ATYPES(7)*8
      HOLLERITH CATH(256)
      INTEGER   IRET, I, NAXIS, LROW, IROUND
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      INTEGER   NCODE, INDEX
      INCLUDE 'PATGN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE  (CATBLK, CATR, CATH, CATD)
      DATA NCODE /8/
      DATA CODES /'ZONE','GAUS','LRTZ','BEAM','RADI','POLY','INVB'/
C                                       Output units.
      DATA UNITS /'UNDEFINE'/
C                                       Number of axes and types.
C                                       (Set for two axes = Ra, Dec.)
      DATA NAXIS  /7/
      DATA ATYPES /'RA---SIN','DEC--SIN', 'STOKES  ', 'FREQ ', 3*' '/
C-----------------------------------------------------------------------
      IRET = 1
C                                       Set default OPCODE
      ICODE = 1
      DO 10 I = 1,NCODE
         ICODE = I
         IF (OPCODE.EQ.CODES(I)) GO TO 20
 10      CONTINUE
C                                       Default OPCODE is first.
      OPCODE = CODES(1)
      ICODE = 1
 20   IF (ARRAY.EQ.'JVLA') ARRAY = 'EVLA'
      IF ((ARRAY.NE.'VLA') .AND. (ARRAY.NE.'ATCA')) ARRAY = 'EVLA'
      CALL CHR2H (8, ARRAY, 1, CATH(KHTEL))
C                                       Set output units.
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C                                       Fill axis type from ATYPES
      DO 30 I = 1,NAXIS
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, ATYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C                                       Fill in values.
      IF (XMSIZE(1).LT.2.0) XMSIZE(1) = 256.
      IF (XMSIZE(2).LT.2.0) XMSIZE(2) = 256.
      CATBLK(KINAX) = IROUND (XMSIZE(1))
      CATBLK(KINAX+1) = IROUND (XMSIZE(2))
      CALL FILL (5, 1, CATBLK(KINAX+2))
      CATD(KDCRV+2) = 1.0D0
      CATD(KDCRV+3) = 1420405752.D0
C                                       Assume CELLSIZE in sec.
C                                       NOTE: Ra decreases with
C                                       grid number.
      IF (CELLS(1).LE.0.0) CELLS(1) = 1.0
      IF (CELLS(2).LE.0.0) CELLS(2) = 1.0
      CATR(KRCIC) = - CELLS(1) / 3600.
      CATR(KRCIC+1) = CELLS(2) / 3600.
      CALL RFILL (5, 1.0, CATR(KRCIC+2))
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
C                                       Miscellaneous items.
C                                       Other checks.
C                                       Set default for center.
      LROW = CATBLK(KINAX)
      IF ((CPARM(1).EQ.0.0) .AND. (CPARM(2).EQ.0.0)) THEN
         CPARM(1) = (CATBLK(KINAX) + 1) / 2
         CPARM(2) = (CATBLK(KINAX+1) + 2) / 2
         WRITE (MSGTXT,1035) CPARM(1), CPARM(2)
         CALL MSGWRT (4)
         END IF
C                                       Bad CPARM(1) value, die.
      IF ((CPARM(1).LT.0.0) .OR. (CPARM(1).GT.LROW)) THEN
         WRITE (MSGTXT,1040)
         GO TO 990
         END IF
C                                       Bad CPARM(2) value, die.
      IF ((CPARM(2).LT.0.) .OR. (CPARM(2).GT.CATBLK(KINAX+1))) THEN
         WRITE (MSGTXT,1045)
         GO TO 990
         END IF
C                                       ref pixels
      CATR(KRCRP) = CPARM(1)
      CATR(KRCRP+1) = CPARM(2)
C                                       Gaussian, Lorentzian
      IF ((ICODE.EQ.2) .OR. (ICODE.EQ.3)) THEN
C                                       Set default r.m.s.
         CPARM(5) = ABS (CPARM(5)) / CELLS(1)
         IF (CPARM(5).EQ.0.0) THEN
            CPARM(5) = 0.707
            WRITE (MSGTXT,1050) CPARM(5)
            CALL MSGWRT (4)
            END IF
C                                       Limits default
         IF (CPARM(4).LE.CPARM(3)) THEN
            CPARM(3) = 0.0
            CPARM(4) = 1.0
            WRITE (MSGTXT,1060)
            CALL MSGWRT (4)
            END IF
C                                       ZONE (default)
      ELSE IF (ICODE.EQ.1) THEN
         IF (CPARM(3).EQ.0.0) CPARM(3) = 1.0
C                                       Set default period.
         CPARM(4) = ABS(CPARM(4)) / CELLS(1)
         IF (CPARM(4).EQ.0.0) THEN
            CPARM(4) = LROW * 320.0/512.0
            WRITE (MSGTXT,1070) CPARM(4)
            CALL MSGWRT (4)
            END IF
C                                       Set default r.m.s.
         CPARM(5) = ABS(CPARM(5))
         IF (CPARM(5).GT.0.5) THEN
            CPARM(5) = 0.5
            WRITE (MSGTXT,1075) CPARM(5)
            CALL MSGWRT (4)
            END IF
C                                       BEAM, INVB
      ELSE IF ((ICODE.EQ.4) .OR. (ICODE.EQ.7)) THEN
         IF (CPARM(3).LE.0.) CPARM(3) = 1.420405752
         CATD(KDCRV+3) = CPARM(3) * 1.0D9
         IF (PBPARM(1).GT.0.33) PBPARM(1) = 0.0
         END IF
C                                       Finished.
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1035 FORMAT ('Function center defaults to ',F6.2,',',F6.2)
 1040 FORMAT ('X-CENTER COORDINATE OUTSIDE IMAGE BOUNDARIES')
 1045 FORMAT ('Y-CENTER COORDINATE OUTSIDE IMAGE BOUNDARIES')
 1050 FORMAT ('CPARM(5) defaults to ',F6.3,' pixels')
 1060 FORMAT ('CPARM(3,4) defaults to 0.0, 1.0')
 1070 FORMAT ('CPARM(4) defaults to ',F8.3,' pixels')
 1075 FORMAT ('CPARM(5) defaults to ',F6.3)
      END
      SUBROUTINE MAKPAT (IPOS, RESULT, IRET)
C-----------------------------------------------------------------------
C   MAKPAT allows a user to create an image one row at a time.  Output
C   values are reals which may  be blanked.
C   The history is written after the last call to MAKPAT.
C   If IRET .GT. 0 then the output file will be destroyed.
C   After all data has been processed a final call will be made to
C   MAKPAT with IPOS(1)=-1.  This is to allow for the completion of
C   pending operations, i.e. preparation of HIstory cards.
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in DATA
C   Values from commons:
C      ICODE     I    Opcode number from list in PATHED.
C      FBLANK    R    Value of blanked pixel.
C      CPARM(10) R    Input adverb array.
C      CATBLK    I    Output catalog header (also CATR, CATD)
C   Output:
C      RESULT(*) R    Output row.
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C     CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), LROW, I, IRET
      REAL      RESULT(*), CATR(256), RADSQR, HEIGHT, POWER, SCALE, V
      DOUBLE PRECISION CATD(128), RADIUS, LAMBDA, X, Y
      LOGICAL   OUTSID
      INCLUDE 'PATGN.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call
      IF (IPOS(1).LT.0) GO TO 900
         LAMBDA = VELITE / CATD(KDCRV+3)
C                                       Set length of a row
         LROW = CATBLK(KINAX)
         DO 100 I = 1,LROW
            RADSQR = (CPARM(1)-I)**2 + (CPARM(2)-IPOS(2))**2
C                                       Gaussian branch.
            IF (ICODE.EQ.2) THEN
               SCALE = CPARM(4) - CPARM(3)
               HEIGHT = 0.707 / CPARM(5)
               POWER = 0.0 - (HEIGHT**2 * RADSQR)
               RESULT(I) = SCALE * EXP(POWER) + CPARM(3)
C                                       Lorentzian branch.
            ELSE IF (ICODE.EQ.3) THEN
               SCALE = CPARM(4) - CPARM(3)
               RESULT(I) = SCALE * (1 / (1 + (RADSQR/CPARM(5)**2)))
     *            + CPARM(3)
C                                       Zone plate branch. (default)
            ELSE IF (ICODE.EQ.1) THEN
               SCALE = SQRT (RADSQR)
               IF (SCALE.LE.CPARM(4)/2.0) THEN
                  RESULT(I) = CPARM(3) * COS (RADSQR*6.2831853*CPARM(5)
     *               / CPARM(4))
               ELSE IF (SCALE.LE.1.5*CPARM(4)) THEN
                  RESULT(I) = CPARM(3) * COS (6.2831853 * CPARM(5) *
     *               (CPARM(4)/2.0-((SCALE-CPARM(4))**2)/CPARM(4)))
               ELSE
                  RESULT(I) = FBLANK
                  END IF
C                                       BEAM, ATCA
            ELSE IF ((ICODE.EQ.4) .OR. (ICODE.EQ.7)) THEN
               RADIUS = (CATR(KRCIC)*(CPARM(1)-I))**2 +
     *            (CATR(KRCIC+1)*(CPARM(2)-IPOS(2)))**2
               RADIUS = SQRT (RADIUS)
               CALL PBCALC (RADIUS, LAMBDA, ARRAY, PBPARM(2), RESULT(I),
     *            OUTSID)
               IF (RESULT(I).LT.PBPARM(1)) RESULT(I) = FBLANK
               IF ((PBPARM(1).EQ.0.0) .AND. (OUTSID)) RESULT(I) = FBLANK
               IF (ICODE.EQ.7) THEN
                  IF (RESULT(I).LE.0.0) THEN
                     RESULT(I) = FBLANK
                  ELSE IF (RESULT(I).NE.FBLANK) THEN
                     RESULT(I) = 1.0 / RESULT(I)
                     END IF
                  END IF
C                                       RADI
            ELSE IF (ICODE.EQ.5) THEN
               RADIUS = (CATR(KRCIC)*(CPARM(1)-I))**2 +
     *            (CATR(KRCIC+1)*(CPARM(2)-IPOS(2)))**2
               RADIUS = SQRT (RADIUS) * 3600.0
               V = CPARM(3) + RADIUS * (CPARM(4) + RADIUS *
     *            (CPARM(5) + RADIUS * (CPARM(6) + RADIUS * (CPARM(7) +
     *            RADIUS * (CPARM(8) + RADIUS * (CPARM(9) + RADIUS *
     *            CPARM(10)))))))
               RESULT(I) = V
C                                       POLY
            ELSE IF (ICODE.EQ.6) THEN
               X = CATR(KRCIC)*(I - CPARM(1)) * 3600.0
               Y = CATR(KRCIC+1)*(IPOS(2) - CPARM(1)) * 3600.0
               RESULT(I) =  CPARM(3) + X*CPARM(4) + Y*CPARM(5) +
     *            X*X*CPARM(6) + Y*Y*CPARM(7) + (X**3)*CPARM(8) +
     *            (Y**3)*CPARM(9) + (X**4)*CPARM(10) + (Y**4)*PBPARM(1)
     *            + X*Y*PBPARM(2) + X*X*Y*PBPARM(3) + X*Y*Y*PBPARM(4) +
     *            (X**3)*Y*PBPARM(5) + (Y**3)*X*PBPARM(6) +
     *            ((X*Y)**2)*PBPARM(7)
               END IF
 100        CONTINUE
         GO TO 999
C                                       Last call - do history etc.
 900  CONTINUE
C
 999  RETURN
      END
