LOCAL INCLUDE 'IMMOD.INC'
C                                       Local include for IMMOD
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAXGAU
      PARAMETER (MAXGAU = 9999)
C
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XOPCOD,
     *   XINLST(12)
      REAL      XSEQIN, XDISKI,  XSEQO, XDISKO, BLC(7), TRC(7),
     *   XGAUSS, XMAX(4), XPOS(2,4), XWID(3,4), FLUX, FACTOR
      REAL      BUFF1(MABFSS), BUFF2(MABFSS), FMAX(MAXGAU),
     *   FPOS(2,MAXGAU), FWID(3,MAXGAU)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO, JBUFSZ,
     *   CATOLD(256), CATBLK(256), ICODE, NGAUSS
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4,
     *   INLIST*48
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, BLC, TRC, XOPCOD, XGAUSS, XMAX, XPOS, XWID,
     *   FLUX, FACTOR, XINLST
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE, INLIST
      COMMON /PARMS/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, ICODE
      COMMON /BUFRS/ BUFF1, BUFF2, FMAX, FPOS, FWID, NGAUSS
      COMMON /MAPHDR/ CATBLK
C                                       End IMMOD
LOCAL END
      PROGRAM IMMOD
C-----------------------------------------------------------------------
C! Adds a model and/or noise to an image
C# Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2005, 2008, 2010-2011, 2014-2015
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
C   IMMOD is an AIPS task to modify an image by a model
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
C      OPCODE         OPCODE        User specified opcode.
C      SHIFT(2)       SHIFT         X, Y centroid offset of model.
C      ZEROSP(5)      ZEROSP        Zero spacing flux I, Q, U, V.
C      BMAJ           BMAJ          Model major axis in arcsec. 0 => 1
C      BMIN           BMIN          Model minor axis in arcsec. 0 => 1
C      BPA            BPA           Model position angle in deg.
C      FLUX           FLUX          Noise level in Jy/Pix.
C      FACTOR         FACTOR        Multiplying factor for previous
C                                   data.
C      Whoever wrote this program was sensible not to put their name
C      in this place; NEBK, March, 1988
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'IMMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'IMMOD '/
C-----------------------------------------------------------------------
C                                       Get inputs, create output file
      CALL IMMIN (PRGM, IRET)
C                                       Apply model to image
      IF (IRET.EQ.0) CALL IMMOMA (IRET)
C                                       Add history
      IF (IRET.EQ.0) CALL IMMHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE IMMIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   IMMIN gets input parameters for IMMOD and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, MTYPE*2
      HOLLERITH CATH(256)
      INTEGER  IRET, IERR, NPARM, IROUND
      REAL      CATR(256)
      INCLUDE 'IMMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
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
      NPARM = 68
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
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (48, 1, XINLST, INLIST)
C                                       Create new file
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
C                                       get components
      CALL READIT (IERR)
      IF (IERR.NE.0) GO TO 999
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
      CALL IMMHED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      SEQOUT = CATBLK(KIIMS)
      IRET = 0
C                                       keywords copied mostly
      CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMMIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('IMMIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE IMMOMA (IRET)
C-----------------------------------------------------------------------
C   IMMOMA sends the image one row at a time to the user supplied
C   routine and then writes the modified data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IRET, IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO,
     *   WINO(4), BOI, BOO, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2,
     *   I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, LIMO, LIMIT,
     *   IBIND, OBIND, INDI, INDO, LIM1
      REAL      OUTMAX, OUTMIN, CATR(256), OLDR(256)
      DOUBLE PRECISION    OLDD(128), CATD(128)
      LOGICAL   T, F, BLNKD
      INCLUDE 'IMMOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD),   (CATOLD, OLDR, OLDD)
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
      OUTMAX = -1.0E30
      OUTMIN = 1.0E30
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
      IPOS(1) = WINI(1)
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
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ,
     *      BOO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'WRIT', IRET
            GO TO 990
            END IF
         DO 250 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.99
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
C                                       Apply model to current row
            CALL IMMODL (IPOS, BUFF1(IBIND), BUFF2(OBIND))
C                                       Check max, min, blanking.
            LIMIT = OBIND + LIMO
            DO 200 I1 = OBIND,LIMIT
               BLNKD = BLNKD .OR. (BUFF2(I1).EQ.FBLANK)
               IF (BUFF2(I1).EQ.FBLANK) GO TO 200
                  OUTMAX = MAX (OUTMAX, BUFF2(I1))
                  OUTMIN = MIN (OUTMIN, BUFF2(I1))
 200              CONTINUE
 250        CONTINUE
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
C                                       Close images
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMMOMA: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('IMMOMA: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('IMMOMA: COMOFF ERROR',I3)
 1100 FORMAT ('IMMOMA: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('IMMOMA: ',A4,' ERROR',I3)
 1260 FORMAT ('IMMOMA: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE IMMHED (IRET)
C-----------------------------------------------------------------------
C     Create the output image header
C     Output:
C         IRET   I       Error status, 0 => OK else abort
C-----------------------------------------------------------------------
      CHARACTER FCHARS(3)*4, CODES(6)*4, CHTMP*4
      HOLLERITH CATH(256), OLDH(256)
      INTEGER   IRET
      INTEGER   LIMIT, I
      REAL      CATR(256), OLDR(256)
      DOUBLE PRECISION    CATD(128), OLDD(128)
      INTEGER   NCODE, INDEX
      INCLUDE 'IMMOD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE  (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE  (CATOLD, OLDR, OLDD, OLDH)
      DATA FCHARS /'FREQ','VELO','FELO'/
      DATA NCODE /6/
      DATA CODES /'POIN','GAUS','DISK','RECT','SPHE','EXPD'/
C-----------------------------------------------------------------------
C                                       Set default OPCODE
      ICODE = 2
      DO 10 I = 1,NCODE
         IF (OPCODE.EQ.CODES(I)) ICODE = I
 10      CONTINUE
C                                       Default to point model if nec.
      OPCODE = CODES(ICODE)
      WRITE (MSGTXT,1010) OPCODE
      CALL MSGWRT (3)
C                                       Set axes in output CATBLK.
      LIMIT = CATOLD(KIDIM)
C                                       Copy/update axis values
      DO 80 I = 1,LIMIT
         CATBLK(KINAX+I-1) = TRC(I) - BLC(I) + 1.01
         CATR(KRCRP+I-1) = OLDR(KRCRP+I-1) - BLC(I) + 1.0
         CATR(KRCIC+I-1) = CATR(KRCIC+I-1)
         CATD(KDCRV+I-1) = OLDD(KDCRV+I-1)
         INDEX = KHCTP + (I-1) * 2
         CATH(INDEX) = OLDR(INDEX)
         CATH(INDEX+1) = OLDR(INDEX+1)
         IF (CATBLK(KIALT).EQ.0) GO TO 80
            CALL H2CHR (4, 1, CATH(INDEX), CHTMP)
            IF ((CHTMP.EQ.FCHARS(1)) .OR. (CHTMP.EQ.FCHARS(2)) .OR.
     *         (CHTMP.EQ.FCHARS(3))) CATR(KRARP) = CATR(KRARP) -
     *         BLC(I) + 1.0
 80      CONTINUE
C                                       Finished.
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Using OPCODE = ''',A,'''')
      END
      SUBROUTINE IMMODL (IPOS, DATA, RESULT)
C-----------------------------------------------------------------------
C   Apply model to one row of the image.
C   Inputs:
C      IPOS     R(7)   IPOS(2) is the index of the current row in the
C                      input image
C      DATA     R      The current row data
C   Output:
C      RESULT   R      The row after modification by the model
C-----------------------------------------------------------------------
      INTEGER   IPOS(7)
      REAL      DATA(*), RESULT(*)
C
      INCLUDE 'IMMOD.INC'
      REAL      XX, YY, CPHI(MAXGAU), SPHI(MAXGAU), R, ANOISE
      INTEGER   I1, I, ISET, IXBLC, IXTRC, II, NCOMP, J
      REAL      CATR(256), OLDR(256), MV, RSUM
      DOUBLE PRECISION    CATD(128), OLDD(128)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD), (CATOLD, OLDR, OLDD)
      SAVE ISET, NCOMP, IXBLC, IXTRC, CPHI, SPHI
      DATA ISET /0/
C-----------------------------------------------------------------------
C                                       Initialize constants
      IF (ISET.EQ.0) THEN
         ISET = 1
         NCOMP = NGAUSS
         DO 10 J = 1,NCOMP
            CPHI(J) = COS (FWID(3,J) * DG2RAD)
            SPHI(J) = SIN (FWID(3,J) * DG2RAD)
 10         CONTINUE
C                                       Convert x-window to integers
         IXBLC = BLC(1) + 0.01
         IXTRC = TRC(1) + 0.01
         IF (FLUX.GT.0.0) CALL RANDIN (I1)
         END IF
C                                       Loop over row pixels and apply
C                                       model
      DO 90 I = IXBLC, IXTRC
         II = I - IXBLC + 1
         RSUM = 0.0
         ANOISE = 0.0
         DO 80 J = 1,NCOMP
            IF (ICODE.NE.1) CALL RADPOS (I, IPOS(2), FPOS(1,J),
     *         FWID(1,J), CPHI(J), SPHI(J), XX, YY, R)
            MV = 0.0
C                                       Point
            IF (ICODE.EQ.1) THEN
               R = SQRT ((I-FPOS(1,J))**2 + (IPOS(2)-FPOS(2,J))**2)
               IF (R.LE.0.5) MV = 1.0
C                                       Gaussian; 2.772588722 = 4ln(2)
            ELSE IF (ICODE.EQ.2) THEN
               MV =  EXP (-2.772588722 * R * R)
C                                       Disk
            ELSE IF (ICODE.EQ.3) THEN
               IF (R.LE.0.5) MV = 1.0
C                                       Rectangle
            ELSE IF (ICODE.EQ.4) THEN
               IF ((ABS(XX).LE.0.5) .AND. (ABS(YY).LE.0.5)) MV = 1.0
C                                       Sphere
            ELSE IF (ICODE.EQ.5) THEN
               IF (R.LT.0.5) MV = SQRT (1.0 - 4.0 * R * R)
C                                       Exponential
            ELSE IF (ICODE.EQ.6) THEN
               MV = EXP (-1.386294361 * R)
               END IF
            RSUM = RSUM + MV * FMAX(J)
 80         CONTINUE
C                                       Add random noise?
         IF (FLUX.GT.0.0) CALL NOISE (ANOISE)
C
         RESULT(II) = FACTOR * DATA(II) + RSUM + ANOISE * FLUX
 90      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RADPOS (I, J, FP, FW, CPHI, SPHI, XX, YY, R)
C-----------------------------------------------------------------------
C   Work out distance of current pixel from model center and normalize
C   by the FWHM and correct for p.a. of model
C   Inputs:
C      I      I      X pixel
C      J      I      Y pixel
C      FP     R(2)   Component X,Y center pixels
C      FW     R(3)   Component Bmaj, Bmin, Bpa (pixels, pixel, deg)
C      CPHI   R      Cos (Bpa)
C      SPHI   R      Sin (Bpa)
C   Outputs:
C      XX     R      Normalized X position
C      YY     R      Normalized Y position
C      R      R      Normalized radius of current pixel from model
C                     center
C   Disks and rectangles extend only to R=0.5 or XX,YY=0.5
C-----------------------------------------------------------------------
      INTEGER   I, J
      REAL      FP(2), FW(3), CPHI, SPHI, XX, YY, R
C
      REAL      X, Y
C-----------------------------------------------------------------------
      X = I - FP(1)
      Y = J - FP(2)
      XX = (Y * CPHI - X * SPHI) / FW(1)
      YY = (X * CPHI + Y * SPHI) / FW(2)
      R = SQRT (XX**2 + YY**2)
C
      RETURN
      END
      SUBROUTINE NOISE (A)
C-----------------------------------------------------------------------
C   NOISE generates a random number approximately distributed in a
C   Gaussian manner about zero.  It does it by summing a uniformly-
C   distributed random number 12 times.
C   Output:
C      A   R       The current sample from the gaussian distribution
C-----------------------------------------------------------------------
      REAL      A, B
      INTEGER   J
C-----------------------------------------------------------------------
      A = -6.0
      DO 10 J = 1,12
         CALL RANDUM (B)
         A = A + B
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE IMMHIS
C-----------------------------------------------------------------------
C   IMMHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I, NCOMP, JTRIM
      INCLUDE 'IMMOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (3)
      IF (IERR.NE.0) GO TO 20
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (3)
      IF (IERR.NE.0) GO TO 20
C                                       OPCODE
      WRITE (HILINE,2002) TSKNAM, OPCODE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Components
      NCOMP = NGAUSS
      NCOMP = MAX (1, MIN (9, NCOMP))
      IF (NGAUSS.GT.NCOMP) THEN
         MSGTXT = 'ONLY FIRST 9 LISTED'
         CALL MSGWRT (2)
         END IF
      WRITE (HILINE,2006) TSKNAM, NGAUSS
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      IF (INLIST.NE.' ') THEN
         I = JTRIM (INLIST)
         WRITE (HILINE,2007) TSKNAM, INLIST(:I)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
      DO 10 I = 1,NCOMP
C                                       FMAX
         WRITE (HILINE,2003) TSKNAM, I, FMAX(I)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (3)
         IF (IERR.NE.0) GO TO 20
C                                       FPOS
         WRITE (HILINE,2004) TSKNAM, I, FPOS(1,I), FPOS(2,I)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (3)
         IF (IERR.NE.0) GO TO 20
C                                       BMAJ
         IF (ICODE.NE.1) THEN
            WRITE (HILINE,2005) TSKNAM, I, FWID(1,I), FWID(2,I),
     *         FWID(3,I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            MSGTXT = HILINE(7:)
            CALL MSGWRT (3)
            IF (IERR.NE.0) GO TO 20
            END IF
 10      CONTINUE
C                                       FLUX
      WRITE (HILINE,2008) TSKNAM, FLUX
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (3)
      IF (IERR.NE.0) GO TO 20
C                                       FACTOR
      WRITE (HILINE,2009) TSKNAM, FACTOR
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (3)
      IF (IERR.NE.0) GO TO 20
C                                       Close HI file
 20   CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
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
 1000 FORMAT ('IMMHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',7F6.0)
 2001 FORMAT (A6,'TRC =',7F6.0)
 2002 FORMAT (A6,'OPCODE = ',1H',A4,1H')
 2003 FORMAT (A6,'FMAX(',I1,')   =',1PE12.4,12X,'/ JY/BEAM')
 2004 FORMAT (A6,'FPOS(',I1,')   =',F8.2,',',F8.2,7X,'/ pixels')
 2005 FORMAT (A6,'FWIDTH(',I1,') =',F7.3,',',F7.3,',',F6.1,
     *   '  / Maj pix, Min pix, PA deg')
 2006 FORMAT (A6,'NGAUSS=',I6,'    total number components')
 2007 FORMAT (A6,'INLIST= ''',A,'''  / text file of components')
 2008 FORMAT (A6,'FLUX = ',1PE12.4,5X,'/ noise added')
 2009 FORMAT (A6,'FACTOR = ',1PE12.4,5X,'/ Applied to input data')
      END
      SUBROUTINE READIT (IRET)
C-----------------------------------------------------------------------
C   Prepares list of components for adverbs or text file
C   Output
C      IRET   I   Error code
C   rest in Common
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'IMMOD.INC'
      INTEGER   TLUN, TIND, LUNTMP, LLIM, LP, I, JTRIM, J
      REAL      BMAJ, BMIN, BPA, XINC, CATR(256)
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATR, CATBLK)
C-----------------------------------------------------------------------
C                                       width defaults
      XINC = ABS (CATR(KRCIC))
      BMAJ = 0.0
      IF (XINC.GT.0.0) THEN
         BMAJ = CATR(KRBMJ) / XINC
         BMIN = CATR(KRBMN) / XINC
         BPA = CATR(KRBPA)
         END IF
      IF ((BMAJ.LE.0.0) .OR. (BMIN.LE.0.0)) THEN
         BMAJ = 3.0
         BMIN = 3.0
         BPA = 0.0
         END IF
C                                       adverbs
      IF (INLIST.EQ.' ') THEN
         NGAUSS = XGAUSS + 0.5
         NGAUSS = MAX (1, NGAUSS)
         DO 10 I = 1,NGAUSS
            FMAX(I) = XMAX(I)
            FPOS(1,I) = XPOS(1,I)
            FPOS(2,I) = XPOS(2,I)
            FWID(1,I) = XWID(1,I)
            FWID(2,I) = XWID(2,I)
            FWID(3,I) = XWID(3,I)
            IF ((FWID(1,I).LE.0.0) .OR. (FWID(2,I).LE.0.0)) THEN
               FWID(1,I) = BMAJ
               FWID(2,I) = BMIN
               FWID(3,I) = BPA
               END IF
 10         CONTINUE
C                                       text file
      ELSE
         TLUN = LUNTMP (2)
C                                       open the text file
         CALL ZTXOPN ('READ', TLUN, TIND, INLIST, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN TEXT FILE'
            GO TO 980
            END IF
         NGAUSS = 0
 100     CALL ZTXIO ('READ', TLUN, TIND, LINE, IRET)
         IF ((IRET.EQ.0) .AND. (NGAUSS.LT.MAXGAU)) THEN
            LLIM = JTRIM (LINE)
C                                       blanks, comments
            IF (LLIM.LE.0) GO TO 100
            IF (LINE(1:1).EQ.'#') GO TO 100
C                                       parse
C                                       flux
            LP = 1
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 100
            IF (X.LE.0.0) GO TO 100
            NGAUSS = NGAUSS + 1
            FMAX(NGAUSS) = X
C                                       position
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) THEN
               NGAUSS = NGAUSS - 1
               GO TO 100
            ELSE
               FPOS(1,NGAUSS) = X
               END IF
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) THEN
               NGAUSS = NGAUSS - 1
               GO TO 100
            ELSE
               FPOS(2,NGAUSS) = X
               END IF
C                                       width
            CALL RFILL (3, 0.0, FWID(1,NGAUSS))
            DO 110 J = 1,3
               IF (LP.LE.LLIM) THEN
                  CALL GETNUM (LINE, LLIM, LP, X)
                  IF (X.EQ.DBLANK) THEN
                     NGAUSS = NGAUSS - 1
                     GO TO 100
                  ELSE
                     FWID(J,NGAUSS) = X
                     END IF
                  END IF
 110           CONTINUE
            GO TO 100
C                                       real error
         ELSE IF ((IRET.GT.0) .AND. (IRET.NE.2)) THEN
            WRITE (MSGTXT,1000) IRET, 'READING TEXT FILE'
            GO TO 980
C                                       EOF
         ELSE
            CALL ZTXCLS (TLUN, TIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'CLOSING TEXT FILE'
               GO TO 980
               END IF
            END IF
         END IF
C                                       check defaults
      DO 120 I = 1,NGAUSS
         IF ((FWID(1,I).LE.0.0) .OR. (FWID(2,I).LE.0.0)) THEN
            FWID(1,I) = BMAJ
            FWID(2,I) = BMIN
            FWID(3,I) = BPA
            END IF
 120     CONTINUE
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('READIT ERROR',I4,' ON ',A)
      END
