LOCAL INCLUDE 'SHADW.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), NUMHIS, JBUFSZ, ICODE
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOT(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4,
     *   HISCRD(10)*64
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   CPARM(10), BUFF1(MABFSS), BUFF2(MABFSS)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOT,
     *   XSEQO, XDISKO, BLC, TRC, CPARM
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE, HISCRD
      COMMON /PARMS/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, ICODE, NUMHIS
      COMMON /BUFRS/ BUFF1, BUFF2
LOCAL END
      PROGRAM SHADW
C-----------------------------------------------------------------------
C! Generate a "shadowed" map from a standard AIPS map.
C# Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2008, 2010, 2015
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
C   SHADW generates a "shadowed" map from a standard AIPS map.
C   It was cloned from TAFFY.
C
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
C      CPARM(10)      CPARM         User specified array.
C                                    1: sun's azimuth (deg)
C                                    2: sun's elevation (deg)
C                                    3: surface height scale factor
C                                    4: relative shade brightness
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1990/Sep/20. Code last modified; 1990/Sep/20
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'SHADW.INC'
      INCLUDE 'INCS:DCAT.INC'
C
      DATA PRGM /'SHADW '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SHADIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL SENDMA (IRET)
C                                       History
      IF (IRET.EQ.0) CALL SHADHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SHADIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   SHADIN gets input parameters for SHADW and creates an output file.
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
      INCLUDE 'SHADW.INC'
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
C                                       Fixed PPM 1996.09.30: was 35
      NPARM = 38
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
 1000 FORMAT ('SHADIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('SHADIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE SENDMA (IRET)
C-----------------------------------------------------------------------
C   SENDMA sends image one row at a time to the user supplied
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
     *   LIMIT, IBIND, OBIND, INDI, INDO, LIM1, DIDRET, OUTCNT
      REAL      OUTMAX, OUTMIN
      LOGICAL   T, F, BLNKD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'SHADW.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.1) THEN
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
         IF (I3.LE.1) THEN
            CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3), BOTEMP,
     *         IRET)
            BOO = BOTEMP + 1
            CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2,
     *         JBUFSZ, BOO, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'WRIT', IRET
               GO TO 990
               END IF
            END IF
         DIDRET = 0
         OUTCNT = NYO
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
            IF (DIDRET.GE.0) THEN
               CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
               OBIND = OBIND - 1
               OUTCNT = OUTCNT - 1
               IF (OUTCNT.LT.0) THEN
                  WRITE (MSGTXT,1113)
                  GO TO 990
                  END IF
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1120) 'WRIT', IRET
                  GO TO 990
                  END IF
               END IF
C                                       Call DIDDLE
            IF (DIDRET.GE.0) OBIND = OBIND + 1
            CALL DIDDLE (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
            DIDRET = IRET
C                                       error
            IF (DIDRET.GT.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
C                                       Check max, min, blanking.
C                                       Check if output deferred.
            ELSE IF (DIDRET.EQ.0) THEN
               LIMIT = OBIND + LIMO
               DO 200 I1 = OBIND,LIMIT
                  IF (BUFF2(I1).NE.FBLANK) THEN
                     OUTMAX = MAX (OUTMAX, BUFF2(I1))
                     OUTMIN = MIN (OUTMIN, BUFF2(I1))
                  ELSE
                     BLNKD = .TRUE.
                     END IF
 200              CONTINUE
               END IF
 220        CONTINUE
C                                       Read out any remaining rows
C                                       from DIDDLE.
         DO 260 I2 = 1,OUTCNT
            IPOS(1) = -1
C                                       Check if write requested.
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
            OBIND = OBIND - 1
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT', IRET
               GO TO 990
               END IF
C                                       Call DIDDLE
            OBIND = OBIND + 1
            CALL DIDDLE (IPOS, BUFF1(IBIND), BUFF2(OBIND), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1180) IRET
               GO TO 990
               END IF
C                                       Check max, min, blanking.
            LIMIT = OBIND + LIMO
            DO 250 I1 = OBIND,LIMIT
               IF (BUFF2(I1).EQ.FBLANK) then
                  BLNKD = .TRUE.
               ELSE
                  OUTMAX = MAX (OUTMAX, BUFF2(I1))
                  OUTMIN = MIN (OUTMIN, BUFF2(I1))
                  END IF
 250           CONTINUE
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
         CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1,
     *      IRET)
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
      CALL DIDDLE (IPOS, BUFF1, BUFF2, IRET)
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
 1000 FORMAT ('SENDMA: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('SENDMA: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('SENDMA: COMOFF ERROR',I3)
 1100 FORMAT ('SENDMA: INIT-FOR-',A4,' ERROR',I3)
 1113 FORMAT ('SENDMA: OUTCNT.LT.0! Too many output rows returned.')
 1120 FORMAT ('SENDMA: ',A,' ERROR',I3)
 1180 FORMAT ('SENDMA: DIDDLE ERROR',I3)
 1260 FORMAT ('SENDMA: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE SHADHI
C-----------------------------------------------------------------------
C   SHADHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER LABEL*8, LINE*80, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'SHADW.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       copy some keywords
      CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
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
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SHADHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 2000 FORMAT (A6,' BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,' TRC =',6(F6.0,','),F6.0)
      END
      SUBROUTINE DIDDLE (IPOS, DATA, RESULT, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      IPOS   I(7)    BLC (input image) of first value in DATA
C                     IPOS(1) = -1 => no input data this call.
C                     IPOS(2) = -2 => last call (no input data).
C      DATA   R(*)    Input row, magic value blanked.
C   Values from commons:
C      ICODE   I      Opcode number from list in NEWHED.
C      FBLANK  R      Value of blanked pixel.
C      CPARM   R(10)  Input adverb array.
C      CATBLK  I      Output catalog header (also CATR, CATD)
C      CATOLD  I      Input catalog header
C   Output:
C      RESULT  R(*)   Output row.
C      IRET    I      Return code   0 => OK
C                                  >0 => error, terminate.
C   Output in COMMON:
C     NUMHIS  I          # history entries (max. 10)
C     HISCRD  C(NUMHIS)  History records
C     CATBLK  I          Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      DATA(*), RESULT(*)
C
      LOGICAL   DOIT
      INTEGER   NDX, NX, NY
      REAL      MAP(1048575)
      SAVE NX, NY, NDX, DOIT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'SHADW.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (IPOS(1).LE.-2) THEN
C        Last call (no input data).
      ELSE IF (IPOS(1).EQ.-1) THEN
C        Send back the results a row at a time.
         IF (DOIT) THEN
C           First time through, do the shadowing.
            CALL SHADOW (NX, NY, MAP)
            NDX = 1
            DOIT = .FALSE.
            END IF
C
         CALL RCOPY (NX, MAP(NDX), RESULT)
         NDX = NDX + NX
      ELSE
         IF (IPOS(2).EQ.1) THEN
C           First call for this plane, set length of a row.
            NX = CATBLK(KINAX)
            NY = CATBLK(KINAX+1)
            NDX  = 1
            DOIT = .TRUE.
            END IF
C                                       Accumulate rows of the input map
         CALL RCOPY (NX, DATA, MAP(NDX))
         NDX = NDX + NX
         IRET = -1
         END IF
C
      RETURN
      END
      SUBROUTINE SHADOW (NX, NY, MAP)
C-----------------------------------------------------------------------
C     SHADOW interprets the input map as a three dimensional surface
C     upon which the sun shines, casting shadows.
C
C     Given:
C          NX,NY       I     Map dimensions.
C
C     Given via common INPARM:
C          CPARM(1)    R     Input adverb array.
C                              1) sun's azimuth (deg)
C                              2) sun's elevation (deg)
C                              3) surface height scale factor
C                              4) relative shade brightness
C
C     Given via common MAPHDR:
C          CATR(256)   R     Input map header.
C
C     Given and returned:
C          MAP(NX,NY)  R     The input map.  On return contains the
C                            shadowed representation of the map.
C
C     Called:
C          SHADW: {ILLUM}
C
C     Algorithm:
C          MAP(X,Y) is dissected and loaded into an array BAND(U,V)
C          with rotation by an integral multiple of 90 degrees so that
C          that the sun shines from within 45 degrees of the +V
C          direction.
C
C     Notes:
C       1) Can handle an input map of up to MAXIMG pixels on a side.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1990/Sep/20. Code last modified; 1990/Sep/20
C
C-----------------------------------------------------------------------
      INCLUDE 'SHADW.INC'
      INTEGER  IQUAD, IV, IX, IY, NV, NX, NY
      REAL     BAND(-1:1,0:4097), D2R, DARK, DV, DW, SCL, MAP(NX,NY),
     *   SCLFCT, SHDW(MAXIMG), SOLAZ, SOLEL, SS(MAXIMG), US, VS, WS
C
      PARAMETER (D2R = 3.14159265358/180.0)
C
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Get input parameters
      SOLAZ  = CPARM(1)
      SOLEL  = CPARM(2)
      SCLFCT = CPARM(3)
      DARK   = CPARM(4)
      SOLAZ = MOD (SOLAZ, 360.0)
      IF (SOLAZ.LT.0.0)  SOLAZ  = SOLAZ + 360.0
      IF (SOLEL.LE.0.0)  SOLEL  = 45.0
      IF (SOLEL.GT.90.0) SOLEL  = 90.0
      IF (SCLFCT.EQ.0.0) SCLFCT = 100.0
      IF (DARK.EQ.0.0)   DARK   = 0.75
C                                       Replace blanks with zeroes and
C                                       scale the pixel heights
      SCL = SCLFCT/CATR(KRDMX)
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX
            IF (MAP(IX,IY).EQ.FBLANK) MAP(IX,IY) = 0.0
            MAP(IX,IY) = SCL*MAP(IX,IY)
 10         CONTINUE
 20      CONTINUE
C                                       Determine the solar quadrant
      IQUAD = INT((SOLAZ+45.0)/90.0)
      IQUAD = MOD(IQUAD,4) + 1
      IF (SOLAZ.EQ.45.0)  IQUAD = 1
      IF (SOLAZ.EQ.225.0) IQUAD = 3
      SOLAZ = SOLAZ - (IQUAD-1)*90.0
C                                       Number of pixels on the side
C                                       facing the sun
      IF (IQUAD.EQ.1) NV = NX
      IF (IQUAD.EQ.2) NV = NY
      IF (IQUAD.EQ.3) NV = NX
      IF (IQUAD.EQ.4) NV = NY
C                                       Components of the unit vector in
C                                       the direction of the sun
      US = COS(SOLAZ*D2R)*COS(SOLEL*D2R)
      VS = SIN(SOLAZ*D2R)*COS(SOLEL*D2R)
      WS = SIN(SOLEL*D2R)
C                                       Shadow decrement between image
C                                       rows (DU = -1.0)
      DV = TAN(SOLAZ*D2R)
      DW = TAN(SOLEL*D2R)/SCL
C                                       Initial shadow line
      DO 30 IV = 1, NV
         SHDW(IV) = -1.0E30
 30      CONTINUE
C                                       Do it for the particular
C                                       quadrant
      IF (IQUAD.EQ.1) THEN
C                                       Sunshine from the bottom quarter
         DO 150 IY = 1, NY
            IX = 0
            IF (IY.EQ.1) THEN
               DO 110 IV = 1, NV
                  IX = IX + 1
                  BAND(1,IV)  = 2.0*MAP(IX,1) - MAP(IX,2)
                  BAND(0,IV)  = MAP(IX,1)
                  BAND(-1,IV) = MAP(IX,2)
 110              CONTINUE
            ELSE IF (IY.EQ.NY) THEN
               DO 120 IV = 1, NV
                  IX = IX + 1
                  BAND(1,IV)  = BAND(0,IV)
                  BAND(0,IV)  = BAND(-1,IV)
                  BAND(-1,IV) = 2.0*MAP(IX,NY) - MAP(IX,NY-1)
 120              CONTINUE
            ELSE
               DO 130 IV = 1, NV
                  IX = IX + 1
                  BAND(1,IV)  = BAND(0,IV)
                  BAND(0,IV)  = BAND(-1,IV)
                  BAND(-1,IV) = MAP(IX,IY+1)
 130              CONTINUE
               END IF
C
            CALL ILLUM (DARK, US, VS, WS, DV, DW, NV, BAND, SHDW, SS)
C
            IX = 0
            DO 140 IV = 1, NV
               IX = IX + 1
               MAP(IX,IY) = SS(IV)
 140           CONTINUE
 150        CONTINUE
C
      ELSE IF (IQUAD.EQ.2) THEN
C                                       Sunshine from the right quarter
         DO 250 IX = NX, 1, -1
            IY = 0
            IF (IX.EQ.NX) THEN
               DO 210 IV = 1, NV
                  IY = IY + 1
                  BAND(1,IV)  = 2.0*MAP(NX,IY) - MAP(NX-1,NY)
                  BAND(0,IV)  = MAP(NX,IY)
                  BAND(-1,IV) = MAP(NX-1,IY)
 210              CONTINUE
            ELSE IF (IX.EQ.1) THEN
               DO 220 IV = 1, NV
                  IY = IY + 1
                  BAND(1,IV)  = BAND(0,IV)
                  BAND(0,IV)  = BAND(-1,IV)
                  BAND(-1,IV) = 2.0*MAP(1,IY) - MAP(2,IY)
 220              CONTINUE
            ELSE
               DO 230 IV = 1, NV
                  IY = IY + 1
                  BAND(1,IV)  = BAND(0,IV)
                  BAND(0,IV)  = BAND(-1,IV)
                  BAND(-1,IV) = MAP(IX-1,IY)
 230              CONTINUE
               END IF
C
            CALL ILLUM (DARK, US, VS, WS, DV, DW, NV, BAND, SHDW, SS)
C
            IY = 0
            DO 240 IV = 1, NV
               IY = IY + 1
               MAP(IX,IY) = SS(IV)
 240           CONTINUE
 250        CONTINUE
C
      ELSE IF (IQUAD.EQ.3) THEN
C                                       Sunshine from the top quarter
         DO 350 IY = NY, 1, -1
            IX = NX + 1
            IF (IY.EQ.NY) THEN
               DO 310 IV = 1, NV
                  IX = IX - 1
                  BAND(1,IV)  = 2.0*MAP(IX,NY) - MAP(IX,NY-1)
                  BAND(0,IV)  = MAP(IX,NY)
                  BAND(-1,IV) = MAP(IX,NY-1)
 310              CONTINUE
            ELSE IF (IY.EQ.1) THEN
               DO 320 IV = 1, NV
                  IX = IX - 1
                  BAND(1,IV)  = BAND(0,IV)
                  BAND(0,IV)  = BAND(-1,IV)
                  BAND(-1,IV) = 2.0*MAP(IX,1) - MAP(IX,2)
 320              CONTINUE
            ELSE
               DO 330 IV = 1, NV
                  IX = IX - 1
                  BAND(1,IV)  = BAND(0,IV)
                  BAND(0,IV)  = BAND(-1,IV)
                  BAND(-1,IV) = MAP(IX,IY-1)
 330              CONTINUE
               END IF
C
            CALL ILLUM (DARK, US, VS, WS, DV, DW, NV, BAND, SHDW, SS)
C
            IX = NX + 1
            DO 340 IV = 1, NV
               IX = IX - 1
               MAP(IX,IY) = SS(IV)
 340           CONTINUE
 350        CONTINUE
C
      ELSE IF (IQUAD.EQ.4) THEN
C                                       Sunshine from the left quarter
         DO 450 IX = 1, NX
            IY = NY + 1
            IF (IX.EQ.1) THEN
               DO 410 IV = 1, NV
                  IY = IY - 1
                  BAND(1,IV)  = 2.0*MAP(1,IY) - MAP(2,IY)
                  BAND(0,IV)  = MAP(1,IY)
                  BAND(-1,IV) = MAP(2,IY)
 410              CONTINUE
            ELSE IF (IX.EQ.NX) THEN
               DO 420 IV = 1, NV
                  IY = IY - 1
                  BAND(1,IV)  = BAND(0,IV)
                  BAND(0,IV)  = BAND(-1,IV)
                  BAND(-1,IV) = 2.0*MAP(NX,IY) - MAP(NX-1,IY)
 420              CONTINUE
            ELSE
               DO 430 IV = 1, NV
                  IY = IY - 1
                  BAND(1,IV)  = BAND(0,IV)
                  BAND(0,IV)  = BAND(-1,IV)
                  BAND(-1,IV) = MAP(IX+1,IY)
 430              CONTINUE
               END IF
C
            CALL ILLUM (DARK, US, VS, WS, DV, DW, NV, BAND, SHDW, SS)
C
            IY = NY + 1
            DO 440 IV = 1, NV
               IY = IY - 1
               MAP(IX,IY) = SS(IV)
 440           CONTINUE
 450        CONTINUE
         END IF
C
      RETURN
      END
      SUBROUTINE ILLUM (DARK, US, VS, WS, DV, DW, NV, BAND, SHDW, SS)
C-----------------------------------------------------------------------
C     ILLUM applies the illumination / shadowing algorithm.
C
C     Given:
C          DARK        R     Factor by which to reduce the brightness in
C                            shadowed areas.
C                              = 0.0 gives pitch black shadows
C                              = 1.0 gives no shadows
C          US,VS,WS    R     Components of the unit vector towards the
C                            sun in the (U,V,W) coordinate system.
C          DV,DW       R     Shadow falloff in V and W for DU = -1.0
C          NV          R     Number of pixels in the V-direction.
C
C     Given and returned:
C          BAND(,)     R     Input band of pixels dimensioned as
C                            BAND(-1:1,0:NV+1).  The locations at
C                            IV=0, and IV=NV+1 are for extrapolation
C                            and will be modified.
C          SHDW(NV)    R     Array containing the shadow height.
C
C     Returned:
C          SS(NV)      R     The surface brightness.
C
C     Called:
C          none
C
C     Algorithm:
C          Coordinates are expressed in terms of (U,V,W) and the sun is
C          assumed to lie within 45 degrees of the +V direction.
C          The dot product of the unit vector in the direction of the
C          sun and the unit vector normal to the surface is calculated
C          at each pixel.  The result is normalized to lie in the range
C          0 (black), to 1 (white), and then squared to simulate a
C          cosine reflectivity law.
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1990/Sep/20. Code last modified; 1990/Sep/20
C
C-----------------------------------------------------------------------
      INTEGER  IU, IV, NV
      REAL     BU, BV, DARK, DV, DW, NS, S, SHDW(NV), SS(NV), UN, US,
     :         BAND(-1:1,0:NV+1), VN, VS, WN, WS
C-----------------------------------------------------------------------
C                                       Extrapolate
      DO 10 IU = -1, 1
         BAND(IU,0)    = 2.0*BAND(IU,1)  - BAND(IU,2)
         BAND(IU,NV+1) = 2.0*BAND(IU,NV) - BAND(IU,NV-1)
 10      CONTINUE
C
      DO 20 IV = 1, NV
C                                       Compute derivatives in U and V
C                                       at the pixel, quadratic fit
         BU = (BAND(1,IV)   - BAND(-1,IV))/2.0
         BV = (BAND(0,IV+1) - BAND(0,IV-1))/2.0
C                                       Unit vector normal to the
C                                       surface
         S  = SQRT(BU*BU + BV*BV + 1.0)
         UN = -BU/S
         VN = -BV/S
         WN = 1.0/S
C                                       Compute the vector dot product
         NS = UN*US + VN*VS + WN*WS
C                                       Compute the light value
         SS(IV) = ((NS + 1.0)/2.0)**2
 20      CONTINUE
C
      DO 30 IV = 1, NV
         IF (BAND(0,IV).GE.SHDW(IV)) THEN
C                                       Sunlight
            SHDW(IV) = BAND(0,IV)
         ELSE
C                                       Shadow
            SS(IV) = DARK*SS(IV)
            END IF
 30      CONTINUE
C                                       Decrement the shadow line
      DO 40 IV = 1, NV
         SHDW(IV) = SHDW(IV) - DW
 40      CONTINUE
C                                       Shift the shadow line in V
      IF (DV.LT.0.0) THEN
         DO 50 IV = NV, 2, -1
            SHDW(IV) = (1.0 + DV)*SHDW(IV) - DV*SHDW(IV-1)
 50         CONTINUE
         SHDW(1) = 0.0
C
      ELSE IF (DV.GT.0.0) THEN
         DO 60 IV = 1, NV-1
            SHDW(IV) = (1.0 - DV)*SHDW(IV) + DV*SHDW(IV+1)
 60         CONTINUE
         SHDW(NV) = 0.0
         END IF
C
      RETURN
      END
