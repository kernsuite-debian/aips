LOCAL INCLUDE 'CPYRT.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   ROWEXT
      PARAMETER (ROWEXT = 10)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), JBUFSZ
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOT(2), XVERSN(12)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, VERSON*48
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   BUFF1(MABFSS), BUFF2(MABFSS)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOT,
     *   XSEQO, XDISKO, XVERSN
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, VERSON
      COMMON /PARMS/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, BLC, TRC
      COMMON /BUFRS/ BUFF1, BUFF2
LOCAL END
      PROGRAM CPYRT
C-----------------------------------------------------------------------
C! Task to insert copyright notices and readme history
C# Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2005-2006, 2012, 2015
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
C   CPYRT inserts the readme.survey file as the images' history and
C   copies the image adding 10 rows with a centered copyright statement.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      INFILE         VERSON        Disk area or name for readme file
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'CPYRT.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'CPYRT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL CPYIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL CPYMA (IRET)
C                                       History
      IF (IRET.EQ.0) CALL CPYHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE CPYIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   CPYIN gets input parameters for CPYRT and creates an output file.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, MTYPE*2
      INTEGER   IERR, NPARM, IROUND
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'CPYRT.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 26
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOT, CLAOUT)
      CALL H2CHR (48, 1, XVERSN, VERSON)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL RFILL (7, 0.0, BLC)
      CALL RFILL (7, 0.0, TRC)
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get user modification to CATBLK
      IRET = 4
      CALL NEWHED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, BUFF1, IERR)
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
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CPYIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('CPYIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE CPYMA (IRET)
C-----------------------------------------------------------------------
C   CPYMA sends image one row at a time to the user supplied
C   routine and then writes the modified data.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO,
     *   WINO(4), BOI, BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, LIMO,
     *   LIMIT, IBIND, OBIND, INDI, INDO, LIM1
      REAL      OUTMAX, OUTMIN, OLD4(256)
      LOGICAL   T, F, BLNKD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'CPYRT.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOLD, OLD4)
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = F
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      CORN(7) = 1
      LIMO = CATBLK(KINAX) - 1
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4) = I4
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3) = I3
C                                       Init. files, first input.
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1099) IRET
            GO TO 990
            END IF
         BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
            END IF
C                                       Init output file.
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3), BOTEMP,
     *      IRET)
         BOO = BOTEMP + 1
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2,
     *      JBUFSZ, BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'WRIT', IRET
            GO TO 990
            END IF
         DO 220 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       Check for deferred output.
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT', IRET
               GO TO 990
               END IF
C                                       Call DIDDLE
            CALL DIDDLE (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
            IF (IRET.EQ.0) GO TO 190
               WRITE (MSGTXT,1180) IRET
               GO TO 990
C                                       Check max, min, blanking.
 190        LIMIT = OBIND + LIMO
            DO 200 I1 = OBIND,LIMIT
               BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
               IF (BUFF2(I1).EQ.FBLANK) GO TO 200
                  OUTMAX = MAX (OUTMAX, BUFF2(I1))
                  OUTMIN = MIN (OUTMIN, BUFF2(I1))
 200              CONTINUE
 220        CONTINUE
C                                       now put in the chars
         DO 260 I2 = 1,ROWEXT
            IPOS(1) = -I2
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT', IRET
               GO TO 990
               END IF
C                                       Call DIDDLE
            CALL DIDDLE (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
               END IF
C                                       Check max, min, blanking.
            LIMIT = OBIND + LIMO
            DO 250 I1 = OBIND,LIMIT
               BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
               IF (BUFF2(I1).EQ.FBLANK) GO TO 250
                  OUTMAX = MAX (OUTMAX, BUFF2(I1))
                  OUTMIN = MIN (OUTMIN, BUFF2(I1))
 250              CONTINUE
 260        CONTINUE
C                                       Flush buffer.
         CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) 'FINI', IRET
            GO TO 990
            END IF
C                                       Update CATBLK.
         CATR(KRDMX) = OUTMAX
         CATR(KRDMN) = OUTMIN
         CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1260) IRET
            GO TO 990
            END IF
 300     CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       Close input map.
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CPYMA: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('CPYMA: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('CPYMA: COMOFF ERROR',I3)
 1100 FORMAT ('CPYMA: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('CPYMA: ',A,' ERROR',I3)
 1180 FORMAT ('CPYMA: DIDDLE ERROR',I3)
 1260 FORMAT ('CPYMA: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE CPYHIS
C-----------------------------------------------------------------------
C   CPYHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LINE*80, CTIME*8, CDATE*12, NOTTYP*2, TFILE*80
      INTEGER   TIME(3), DATE(3), LUN1, LUN2, IERR, I, IND1, JTRIM, J,
     *   NONOT, LUN3, JT
      LOGICAL   SURVEY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'CPYRT.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2, LUN3 /3, 28, 29/
      DATA NONOT, NOTTYP /0, ' '/
C-----------------------------------------------------------------------
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
C                                       Write History.
      CALL HIINIT (3)
C                                       Create new HI
      CALL HICREA (LUN2, DISKO, NEWCNO, CATBLK, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Write time and date on new file
      CALL ZDATE (DATE)
      IF (DATE(1).LT.50) DATE(1) = DATE(1) + 2000
      IF (DATE(1).LT.100) DATE(1) = DATE(1) + 1900
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (LINE,1000) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       text file name
      TFILE = VERSON
      SURVEY = .FALSE.
      IF (TFILE.EQ.' ') THEN
         TFILE = 'SURVEY'
         SURVEY = .TRUE.
         END IF
      I = JTRIM (TFILE)
      J = INDEX (TFILE, ':')
      IF (J.EQ.I) I = I - 1
      IF ((J.LE.0) .OR. (J.GT.I)) THEN
         TFILE(I+1:) = ':README.SURVEY'
         SURVEY = .TRUE.
         END IF
C                                       Copyright the image
      LINE = ' '
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (SURVEY) THEN
         WRITE (LINE,1001) DATE(1)
      ELSE
         WRITE (LINE,1002) DATE(1)
         END IF
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 900
      LINE = ' '
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       open readme file
      CALL ZTXOPN ('READ', LUN1, IND1, TFILE, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, LINE(:40)
         CALL MSGWRT (8)
         GO TO 900
         END IF
 20   CALL ZTXIO ('READ', LUN1, IND1, LINE, IERR)
      IF (IERR.EQ.0) THEN
         JT = JTRIM (LINE)
         CALL HIADD (LUN2, LINE, BUFF2, IERR)
         IF (IERR.EQ.0) GO TO 20
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (7)
      ELSE IF (IERR.NE.2) THEN
         WRITE (MSGTXT,1025) IERR
         CALL MSGWRT (7)
         END IF
      CALL ZTXCLS (LUN1, IND1, IERR)
C                                       Close HI file
 900  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN3, LUN2, DISKIN, DISKO,
     *   OLDCNO, NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE EXTENSION FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1001 FORMAT ('NRAO VLA Sky Survey image: COPYRIGHT ',I4,' AUI/NRAO')
 1002 FORMAT ('Image copyright ',I4,' Associated Universities, Inc.')
 1010 FORMAT ('ERROR',I4,' OPENING FILE ',A)
 1020 FORMAT ('ERROR',I4,' WRITING HI FILE')
 1025 FORMAT ('ERROR',I4,' READING TEXT FILE')
      END
      SUBROUTINE NEWHED (IRET)
C-----------------------------------------------------------------------
C   NEWHED is a
C    Input:
C       CATBLK    I(256)  Output catalog header, also CATR, CATD
C       CATOLD    I(256)  Input catalog header,
C    Output:
C       CATBLK    I(256)  Modified output catalog header.
C       IRET      I       Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'CPYRT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Copy/update axis values
      CATBLK(KINAX+1) = CATBLK(KINAX+1) + ROWEXT
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE DIDDLE (IPOS, DATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   DIDLE returns the input when IPOS > 0.
C   Inputs:
C      IPOS   I(7)    BLC (input image) of first value in DATA
C                     IPOS(1) = -m => on row m of the copyright
C      DATA   R(*)    Input row, magic value blanked.
C   Values from commons:
C      FBLANK  R      Value of blanked pixel.
C      CATBLK  I      Output catalog header (also CATR, CATD)
C      CATOLD  I      Input catalog header
C   Output:
C      RESULT  R(*)   Output row.
C      IRET    I      Return code   0 => OK
C                                  >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      DATA(*), RESULT(*)
C
      INTEGER   LROW, I, DATE(3)
      CHARACTER STR*24
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'CPYRT.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      LROW = CATBLK(KINAX)
C                                       copy input to output.
      IF (IPOS(1).GE.1) THEN
         CALL RCOPY (LROW, DATA, RESULT)
C                                       generate characters
      ELSE
         CALL ZDATE (DATE)
         IF (DATE(1).LT.50) DATE(1) = DATE(1) + 2000
         IF (DATE(1).LT.100) DATE(1) = DATE(1) + 1900
         WRITE (STR,1000) DATE(1)
         CALL LCHROW (-IPOS(1), LROW, STR, RESULT)
         DO 10 I = 1,LROW
            IF (RESULT(I).GT.0.0) THEN
               RESULT(I) = CATR(KRDMX)
            ELSE
               RESULT(I) = CATR(KRDMN)
               END IF
 10         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COPYRIGHT ',I4,' AUI/NRAO')
      END
      SUBROUTINE LCHROW (IR, NR, STRING, RESULT)
C-----------------------------------------------------------------------
C   Inputs:
C      IR       I      Row # in characters this pass
C      NR       I      Number of pixels in RESULT
C      STRING   C*(*)  Character string
C   Output
C      RESULT   R(*)   Output row
C-----------------------------------------------------------------------
      INTEGER   IR, NR
      CHARACTER STRING*(*)
      REAL      RESULT(*)
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   LENGTH, ICX, ICY, IBACK, ISPX1, ISPY1, BITIDX,
     *   IC, IT, MASK, ZAND, TABLE(5,97), TAB2(5,17), TAB3(5,16),
     *   TAB4(5,16), TAB5(5,16), TAB6(5,16), TAB7(5,16), JT, LT, KT,
     *   BITS(15), I, J, II, JJ, COUNT, LX,
     *   ISPX2, ISPY2, CSIZ(2), JTRIM, SCRTCH(MABFSS)
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE  (TABLE(1, 1), TAB2),  (TABLE(1,18), TAB3),
     *             (TABLE(1,34), TAB4),  (TABLE(1,50), TAB5),
     *             (TABLE(1,66), TAB6),  (TABLE(1,82), TAB7)
C                                        control chars all ?
C                                        blank !"#$%&'()*+,-./
      DATA TAB2/  32,  64,  69,  72,  48,
     *             0,   0,   0,   0,   0,
     *             0,   0, 121,   0,   0,
     *             0, 112,   0, 112,   0,
     *            20,  62,  20,  62,  20,
     *            18,  42, 127,  42,  36,
     *             2,  36,   8,  18,  32,
     *            54,  73,  85,  34,   5,
     *             0,   0, 112,   0,   0,
     *             0,  28,  34,  65,   0,
     *             0,  65,  34,  28,   0,
     *            20,   8,  62,   8,  20,
     *             8,   8,  62,   8,   8,
     *             0,   1,   6,   0,   0,
     *             0,   8,   8,   8,   0,
     *             0,   0,   1,   0,   0,
     *             2,   4,   8,  16,  32/
C                                        0123456789:;<=>?
      DATA TAB3/  62,  69,  73,  81,  62,
     *             0,  33, 127,   1,   0,
     *            35,  69,  73,  73,  49,
     *            66,  65,  73,  89, 102,
     *            12,  20,  36, 127,   4,
     *           114,  81,  81,  81,  78,
     *            30,  41,  73,  73,  70,
     *            64,  71,  72,  80,  96,
     *            54,  73,  73,  73,  54,
     *            49,  73,  73,  74,  60,
     *             0,   0,  18,   0,   0,
     *             0,   1,  22,   0,   0,
     *             8,  20,  34,  65,   0,
     *            20,  20,  20,  20,   0,
     *            65,  34,  20,   8,   0,
     *            32,  64,  69,  72,  48/
C
C                                       ABCDEFGHIJKLMNO
      DATA TAB4/  18,  37,  37,  37,  30,
     *            31,  36,  68,  36,  31,
     *           127,  73,  73,  73,  34,
     *            62,  65,  65,  65,  34,
     *            65, 127,  65,  65,  62,
     *           127,  73,  73,  73,  65,
     *           127,  72,  72,  64,  64,
     *            62,  65,  65,  69,  39,
     *           127,   8,   8,   8, 127,
     *             0,  65, 127,  65,   0,
     *             2,   1,   1,   1, 126,
     *           127,   8,  20,  34,  65,
     *           127,   1,   1,   1,   1,
     *           127,  32,  24,  32, 127,
     *           127,  16,   8,   4, 127,
     *            62,  65,  65,  65,  62/
C                                        PQRSTUVWXYZ[\]^_
      DATA TAB5/ 127,  72,  72,  72,  48,
     *            62,  65,  69,  66,  61,
     *           127,  72,  76,  74,  49,
     *            50,  73,  73,  73,  38,
     *            64,  64, 127,  64,  64,
     *           126,   1,   1,   1, 126,
     *           112,  12,   3,  12, 112,
     *           126,   1,  14,   1, 126,
     *            99,  20,   8,  20,  99,
     *            96,  16,  15,  16,  96,
     *            67,  69,  73,  81,  97,
     *             0,   0, 127,  65,   0,
     *            32,  16,   8,   4,   2,
     *             0,  65, 127,   0,   0,
     *            16,  32,  64,  32,  16,
     *             1,   1,   1,   1,   1/
C                                        `abcdefghijklmno
      DATA TAB6/   0,  64,  32,  16,  0,
     *            31,  36,  68,  36,  31,
     *           127,  73,  73,  73,  34,
     *            62,  65,  65,  65,  34,
     *            65, 127,  65,  65,  62,
     *           127,  73,  73,  73,  65,
     *           127,  72,  72,  64,  64,
     *            62,  65,  65,  69,  39,
     *           127,   8,   8,   8, 127,
     *            65,  65, 127,  65,  65,
     *             2,   1,   1,   1, 126,
     *           127,   8,  20,  34,  65,
     *           127,   1,   1,   1,   1,
     *           127,  32,  24,  32, 127,
     *           127,  16,   8,   4, 127,
     *            62,  65,  65,  65,  62/
C                                        pqrstuvwxyz{ }~?
      DATA TAB7/ 127,  72,  72,  72,  48,
     *            62,  65,  69,  66,  61,
     *           127,  72,  76,  74,  49,
     *            50,  73,  73,  73,  38,
     *            64,  64, 127,  64,  64,
     *           126,   1,   1,   1, 126,
     *           112,  12,   3,  12, 112,
     *           126,   1,  14,   1, 126,
     *            99,  20,   8,  20,  99,
     *            96,  16,  15,  16,  96,
     *            67,  69,  73,  81,  97,
     *             0,   8,  54,  65,   0,
     *             0,   0, 127,   0,   0,
     *             0,  65,  54,   8,   0,
     *             4,   8,   4,   2,   4,
     *            32,  64,  69,  72,  48/
C-----------------------------------------------------------------------
C                                       simple returns
      CALL RFILL (NR, 0.0, RESULT)
C                                        check input
      IF ((IR.LE.1) .OR. (IR.GE.9)) GO TO 999
      COUNT = JTRIM (STRING)
      LX = COUNT * 7
      IF (LX.GT.NR) GO TO 999
      IF (COUNT.LE.0) GO TO 999
      IBACK = 0
C                                       determine sizes
      CSIZ(1) = 7
      CSIZ(2) = 9
      ICX = 5
      ICY = 7
      ISPX1 = (CSIZ(1) - ICX) / 2
      ISPY1 = (CSIZ(2) - ICY) / 2
      ISPX2 = CSIZ(1) - ICX - ISPX1
      ISPY2 = CSIZ(2) - ICY - ISPY1
      IF ((ISPX1.LE.0) .OR. (ISPX2.LE.0) .OR. (ISPY1.LE.0) .OR.
     *   (ISPY2.LE.0)) THEN
         ISPX1 = MAX (1, ISPX1)
         ISPX2 = MAX (1, ISPX2)
         ISPY1 = MAX (1, ISPY1)
         ISPY2 = MAX (1, ISPY2)
         CSIZ(1) = ICX + ISPX1 + ISPX2
         CSIZ(2) = ICY + ISPY1 + ISPY2
         END IF
C                                       set up
      LENGTH = COUNT * CSIZ(1)
C                                       extract needed parts of table
      BITIDX = 1
      DO 20 I = 1,COUNT
C                                       get standard ASCII char
C                                       in highly machine independent
         JT = NBITWD / 8
         CALL ZCLC8 (1, STRING(I:I), JT, LT)
         CALL ZI32IL (1, 1, LT, KT)
         IT = NBITWD - (JT-1)*8
         CALL ZGTBIT (IT, KT, BITS)
         CALL ZPTBIT (8, IC, BITS(IT-7))
C                                        all CTRL characters to 1
         IC = MAX (1, IC-30)
         CALL COPY (ICX, TABLE(1,IC), SCRTCH(BITIDX))
         BITIDX = BITIDX + ICX
 20      CONTINUE
C                                        Character line
      I = IR - 1
      MASK = 2 ** (I-1)
      JJ = 1
      BITIDX = (NR - LENGTH) / 2 + 1
C                                        if mask bit on in char value
C                                        then set to -1 else to 0
      DO 40 J = 1,COUNT
         BITIDX = BITIDX + ISPX1
         DO 35 II = 1,ICX
            IC = ZAND (MASK, SCRTCH(JJ))
            IF (IC.NE.0) RESULT(BITIDX) = 1.0
            BITIDX = BITIDX + 1
            JJ = JJ + 1
 35         CONTINUE
         BITIDX = BITIDX + ISPX2
 40      CONTINUE
C
 999  RETURN
      END
