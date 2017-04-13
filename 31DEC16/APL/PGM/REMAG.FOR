LOCAL INCLUDE 'REMAG.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO, NBLANK,
     *   CATOLD(256), NUMHIS, JBUFSZ, ICODE
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOT(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, HISCRD(2)*64
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7), PIXVAL,
     *   BUFF1(MABFSS), BUFF2(MABFSS), OLD4(256)
      DOUBLE PRECISION OLD8(128)
      EQUIVALENCE (CATOLD, OLD4, OLD8)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOT,
     *   XSEQO, XDISKO, BLC, TRC, PIXVAL
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD
      COMMON /PARMS/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, ICODE, NUMHIS, NBLANK
      COMMON /BUFRS/ BUFF1, BUFF2
LOCAL END
      PROGRAM REMAG
C-----------------------------------------------------------------------
C! Replaces magic value blanking with another value.
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2005, 2008, 2015
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
C   REMAG: Task to REplace any MAGic blanked pixels with a number
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
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      PIXVAL         PIXVAL        value with which to replace blanks
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'REMAG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'REMAG '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL REMAIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL REMADO (IRET)
C                                       History
      IF (IRET.EQ.0) CALL REMAHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE REMAIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   REMAIN gets input parameters for REMAG and creates an output file.
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
      INCLUDE 'REMAG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 29
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
      NBLANK = 0
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOT, CLAOUT)
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
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get user modification to CATBLK
      IRET = 4
      CALL REMHED (IRET)
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
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REMAIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('REMAIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE REMADO (IRET)
C-----------------------------------------------------------------------
C   REMADO sends image one row at a time to the user supplied
C   routine and then writes the modified data.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO,
     *   WINO(4), BOI, BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, KOFF, LIMO,
     *   LIMIT, IBIND, OBIND, INDI, INDO, LIM1
      REAL      OUTMAX, OUTMIN
      LOGICAL   T, F, BLNKD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'REMAG.INC'
      INCLUDE 'INCS:DCAT.INC'
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
      KOFF = 0
      CORN(7) = 1
      LIMO = CATBLK(KINAX) - 1
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7+KOFF) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6+KOFF) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5+KOFF) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4+KOFF) = I4
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3+KOFF) = I3
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
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ,
     *      BOO, IRET)
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
C                                       Write.
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT', IRET
               GO TO 990
               END IF
C                                       Call REMAGI
            CALL REMAGI (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
               END IF
C                                       Check max, min, blanking.
            LIMIT = OBIND + LIMO
            DO 200 I1 = OBIND,LIMIT
               BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
               IF (BUFF2(I1).EQ.FBLANK) GO TO 200
                  OUTMAX = MAX (OUTMAX, BUFF2(I1))
                  OUTMIN = MIN (OUTMIN, BUFF2(I1))
 200              CONTINUE
 220        CONTINUE
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
C                                       Final call to functions
      IPOS(1) = -2
      CALL REMAGI (IPOS, BUFF1, BUFF2, IRET)
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
 1000 FORMAT ('REMADO: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('REMADO: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('REMADO: COMOFF ERROR',I3)
 1100 FORMAT ('REMADO: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('REMADO: ',A,' ERROR',I3)
 1180 FORMAT ('REMADO: REMAGI ERROR',I3)
 1260 FORMAT ('REMADO: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE REMAHI
C-----------------------------------------------------------------------
C   REMAHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LABEL*8, LINE*80, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'REMAG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /0, ' '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       BLC
      WRITE (MSGTXT,2000) TSKNAM, BLC
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TRC
      WRITE (MSGTXT,2001) TSKNAM, TRC
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1010) TSKNAM
         LINE(1:8) = LABEL(1:8)
         DO 50 I = 1,NUMHIS
            LINE(9:64) = HISCRD(I)(1:64)
            CALL HIADD (LUN2, LINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
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
 1000 FORMAT ('REMAHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
      END
      SUBROUTINE REMHED (IRET)
C-----------------------------------------------------------------------
C    Input:
C       CATBLK    I(256)  Output catalog header, also CATR, CATD
C       CATOLD    I(256)  Input catalog header, also OLD4, OLD8
C    Output:
C       CATBLK    I(256)  Modified output catalog header.
C       IRET      I       Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER FCHARS(3)*4, CTEMP*8
      INTEGER   INDXI, INC, INDEX, LIMIT, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'REMAG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Set axes in output CATBLK.
      LIMIT = CATOLD(KIDIM)
      INC = 2
C                                       Copy/update axis values
      DO 20 I = 1,LIMIT
         CATBLK(KINAX-1+I) = TRC(I) - BLC(I) + 1.01
         CATR(KRCRP-1+I) = OLD4(KRCRP-1+I) - BLC(I) + 1.0
         CATR(KRCIC-1+I) = CATR(KRCIC-1+I)
         CATD(KDCRV-1+I) = OLD8(KDCRV-1+I)
         INDXI = KHCTP + (I-1) * INC
         INDEX = KHCTP + (I-1) * INC
         CALL H2CHR (8, 1, CATH(INDEX), CTEMP)
         CALL CHR2H (8, CTEMP, 1, OLD4(INDXI))
         IF (CATBLK(KIALT).NE.0) THEN
            CALL H2CHR (4, 1, CATH(INDEX), CTEMP)
            IF ((CTEMP(1:4).EQ.FCHARS(1)) .OR.
     *          (CTEMP(1:4).EQ.FCHARS(2)) .OR.
     *          (CTEMP(1:4).EQ.FCHARS(3)))
     *      CATR(KRARP) = CATR(KRARP) - BLC(I) + 1.0
            END IF
 20      CONTINUE
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE REMAGI (IPOS, DATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   Substitutes PIXVAL for magic blanks
C   Inputs:
C      IPOS   I(7)    BLC (input image) of first value in DATA
C                     IPOS(1) = -1 => no input data this call.
C                     IPOS(2) = -2 => last call (no input data).
C      DATA   R(*)    Input row, magic value blanked.
C   Values from commons:
C      FBLANK  R      Value of blanked pixel.
C      PIXVAL  R      PIxel value to substitue
C      CATBLK  I      Output catalog header (also CATR, CATD)
C      CATOLD  I      Input catalog header (also OLD4, OLD8)
C   Output:
C      RESULT  R(*)   Output row.
C      IRET    I      Return code   0 => OK
C                                  >0 => error, terminate.
C   Output in COMMON:
C      NBLANK  I      Number pixels so far blanked
C      NUMHIS  I          # history entries (max. 10)
C      HISCRD  C(NUMHIS)  History records
C      CATBLK  I          Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      DATA(*), RESULT(*)
C
      INTEGER   LROW, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'REMAG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Data call
      IF (IPOS(1).GT.0) THEN
         LROW = CATBLK(KINAX)
         DO 10 I = 1,LROW
            IF (DATA(I).EQ.FBLANK) THEN
               NBLANK = NBLANK + 1
               RESULT(I) = PIXVAL
            ELSE
               RESULT(I) = DATA(I)
               END IF
 10         CONTINUE
C                                       History call
      ELSE
         NUMHIS = 2
         WRITE (HISCRD(1),1000) PIXVAL
         WRITE (HISCRD(2),1010) NBLANK
         WRITE (MSGTXT,1020) PIXVAL, NBLANK
         CALL MSGWRT (5)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PIXVAL =',1PE11.3,5X,'/ substituted value')
 1010 FORMAT ('NBLANK =',I12,4X,'/ number pixels changed')
 1020 FORMAT ('Substituted',1PE12.4,' for',I12,' blanked pixels')
      END
