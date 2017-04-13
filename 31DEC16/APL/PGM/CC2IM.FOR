LOCAL INCLUDE 'CC2IM.INC'
C                                       Local include for CC2IM
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, CCVER, NEWCNO, OLDCNO,
     *   CATOLD(256), JBUFSZ, CCBUFF(512)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), OLDH(256)
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, XCCVER, CELL(2), DOCENT,
     *   IMSIZE(2), BUFF1(MABFSS), BUFF2(MABFSS), OLDR(256)
      DOUBLE PRECISION OLDD(128)
      EQUIVALENCE (CATOLD, OLDH, OLDR, OLDD)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, XCCVER, CELL, DOCENT, IMSIZE
      COMMON /PARMS/  CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, CCVER, JBUFSZ
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, CCBUFF
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
LOCAL END
      PROGRAM CC2IM
C-----------------------------------------------------------------------
C! converts CC file to image
C# MAP Math
C-----------------------------------------------------------------------
C;  Copyright (C) 2012
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
C   CC2IM converts a CC file to an image
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
C      INVERS         CCVER         CC version number
C      CELLSIZE       CELL(2)       Cell size in asec on input
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NEED
      REAL      IMAG(2)
      LONGINT   PIMAG
      INCLUDE 'CC2IM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'CC2IM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL CC2IMI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Dynamic memory make output image
      NEED = CATBLK(KINAX) * CATBLK(KINAX+1) / 1024 + 2
      CALL ZMEMRY ('GET ', PRGM, NEED, IMAG, PIMAG, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET NEEDED DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       fill it in and write it
      CALL CC2IMD (CATBLK(KINAX), CATBLK(KINAX+1), IMAG(1+PIMAG), IRET)
      IF (IRET.EQ.0) CALL CC2IMH
C                                       Close down files, etc.
 990  CALL DIE (IRET, CCBUFF)
C
 999  STOP
      END
      SUBROUTINE CC2IMI (PRGN, IRET)
C-----------------------------------------------------------------------
C   CC2IMI gets input parameters for CC2IM and creates an output file.
C   Inputs:
C      PRGN   C*6    Program name
C   Output:
C      IRET   I      Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER STAT*4, BLANK*6, MTYPE*2
      INTEGER   IERR, NPARM, IROUND
      INCLUDE 'CC2IM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK /'      '/
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
      NPARM = 20
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, CCBUFF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, CCBUFF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
      CCVER = IROUND (XCCVER)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, CCBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', CCBUFF, IERR)
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
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Get user modification to CATBLK
      IRET = 4
      CALL CC2IMS (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, CCBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
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
 1000 FORMAT ('CC2IMI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('CC2IMI: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE CC2IMS (IRET)
C-----------------------------------------------------------------------
C   CC2IMS reads the CC file and determines the size and coordinates
C   of the output image.
C   Common input/output
C      CATBLK   I(256)   Output catalog header, also CATR, CATD
C      CATOLD   I(256)   Input catalog header, also OLDR, OLDD
C   Output:
C      IRET     I        Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUN, NCOL, NCOMP, ICCRNO, IROW, IROUND, CCKOLS(MAXCCC),
     *   CCNUMV(MAXCCC), TYPE
      REAL      XMAX, XMIN, YMAX, YMIN, SMAX, X, Y, FLUX, PARMS(3), DX,
     *   DY
      INCLUDE 'CC2IM.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       open CC file
      LUN = 43
      CALL CCMINI ('READ', CCBUFF, DISKIN, OLDCNO, CCVER, CATOLD, LUN,
     *   ICCRNO, CCKOLS, CCNUMV, NCOL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING CC FILE'
         GO TO 990
         END IF
C                                       init
      NCOMP = CCBUFF(5)
      XMAX = -1.E10
      XMIN = -XMAX
      YMAX = XMAX
      YMIN = -YMAX
      SMAX = 0.0
      DO 20 IROW = 1,NCOMP
         CALL TABCCM ('READ', CCBUFF, ICCRNO, CCKOLS, CCNUMV, NCOL, X,
     *      Y, FLUX, TYPE, PARMS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING CC FILE'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
            XMIN = MIN (XMIN, X)
            XMAX = MAX (XMAX, X)
            YMIN = MIN (YMIN, Y)
            YMAX = MAX (YMAX, Y)
            IF (TYPE.GT.0) SMAX = MAX (SMAX, PARMS(1))
            END IF
 20      CONTINUE
      CALL TABCCM ('CLOS', CCBUFF, ICCRNO, CCKOLS, CCNUMV, NCOL, X,
     *   Y, FLUX, TYPE, PARMS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING CC FILE'
         GO TO 990
         END IF
C                                       cell size
      CELL(1) = CELL(1) / 3600.0
      CELL(2) = CELL(2) / 3600.0
      IF (CELL(1).EQ.0.0) CELL(1) = ABS (OLDR(KRCIC))
      IF (CELL(2).EQ.0.0) CELL(2) = ABS (OLDR(KRCIC+1))
      IF (CELL(1).EQ.0.0) CELL(1) = 1.0 / 3600.0
      IF (CELL(2).EQ.0.0) CELL(2) = 1.0 / 3600.0
      XMIN = (XMIN - 4*SMAX) / CELL(1)
      XMAX = (XMAX + 4*SMAX) / CELL(1)
      IF (OLDR(KRCIC).LT.0.0) THEN
         X = XMIN
         XMIN = -XMAX
         XMAX = -X
         END IF
      YMIN = (YMIN - 4*SMAX) / CELL(2)
      YMAX = (YMAX + 4*SMAX) / CELL(2)
      IF (DOCENT.GT.0.0) THEN
         X = MAX (ABS(XMAX), ABS(XMIN))
         XMAX = X
         XMIN = -X
         Y = MAX (ABS(YMAX), ABS(YMIN))
         YMAX = Y
         YMIN = -Y
         END IF
      CATBLK(KINAX) = XMAX - XMIN + 8.5
      DX = MAX (0.0, IMSIZE(1) - CATBLK(KINAX))
      CATBLK(KINAX) = CATBLK(KINAX) + DX + 0.1
      CATR(KRCIC) = -CELL(1)
      CATR(KRCRP) = IROUND (-XMIN + 4.0 + DX/2.0)
      CATBLK(KINAX+1) = YMAX - YMIN + 8.5
      DY = MAX (0.0, IMSIZE(2) - CATBLK(KINAX+1))
      CATBLK(KINAX+1) = CATBLK(KINAX+1) + DY + 0.1
      CATR(KRCIC+1) = CELL(2)
      CATR(KRCRP+1) = IROUND (-YMIN + 5.0 + DY/2.0)
      IF (DOCENT.GT.0.0) THEN
         CATBLK(KINAX) = ((CATBLK(KINAX)+1)/2) * 2
         CATR(KRCRP) = (CATBLK(KINAX)+1) / 2
         CATBLK(KINAX+1) = ((CATBLK(KINAX+1)+1)/2) * 2
         CATR(KRCRP+1) = CATBLK(KINAX+1) / 2 + 1
         END IF
      CALL FILL (5, 1, CATBLK(KINAX+2))
      CALL CHR2H (8, 'JY/PIXEL', 1, CATH(KHBUN))
      CATBLK(KITYP) = 2
      CATBLK(KINIT) = NCOMP
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CC2IMS: ERROR',I4,' ',A)
      END
      SUBROUTINE CC2IMD (NX, NY, IMAG, IRET)
C-----------------------------------------------------------------------
C   CC2IMD fills in the imagr from the Clean components
C   and then writes it to the image file
C   Input:
C      NX     I      X dimension
C      NY     I      Y dimension
C   Output:
C      IMAG   R(*)   Image of CC
C      IRET   I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      IMAG (NX,*)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER IFILE*48, KEYW*8
      LOGICAL   T
      INTEGER   LUNO, INDO, LUN, NCOL, NCOMP, ICCRNO, CCKOLS(MAXCCC),
     *   CCNUMV(MAXCCC), IX, IY, IX1, IX2, IY1, IY2, TYPE, IROW, I,
     *   WINO(4), IBLKOF, OBIND, LOCS, KEYTYP, NCNV
      REAL      X, Y, FLUX, PARMS(3), FMAX, FMIN, SCALES(2), OFFS(2),
     *   FSUM, GSUM, TSUM, SR, CR, AA, BB, CC, AA0, BB0, CC0, D
      INCLUDE 'CC2IM.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNO /17/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       init for CC models
      SCALES(1) = 1.0 / CATR(KRCIC)
      SCALES(2) = 1.0 / CATR(KRCIC+1)
      OFFS(1) = CATR(KRCRP)
      OFFS(2) = CATR(KRCRP+1)
      I = NX * NY
      CALL RFILL (I, 0.0, IMAG)
C                                       open CC
C                                       open CC file
      LUN = 43
      CALL CCMINI ('READ', CCBUFF, DISKIN, OLDCNO, CCVER, CATOLD, LUN,
     *   ICCRNO, CCKOLS, CCNUMV, NCOL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING CC FILE'
         GO TO 990
         END IF
      NCOMP = CCBUFF(5)
      FSUM = 0.0
      TSUM = 0.0
      AA0 = CATR(KRCIC)*CATR(KRCIC) * 4.0 * LOG (2.0)
      BB0 = CATR(KRCIC+1)*CATR(KRCIC+1) * 4.0 * LOG (2.0)
      CC0 = ABS(CATR(KRCIC)*CATR(KRCIC+1)) * 8.0 * LOG (2.0)
      DO 90 IROW = 1,NCOMP
         CALL TABCCM ('READ', CCBUFF, ICCRNO, CCKOLS, CCNUMV, NCOL, X,
     *      Y, FLUX, TYPE, PARMS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING CC FILE'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
            TSUM = TSUM + FLUX
            X = X * SCALES(1) + OFFS(1)
            Y = Y * SCALES(2) + OFFS(2)
            IX = X + 0.5
            IY = Y + 0.5
            IF ((IX.GT.1) .AND. (IX.LT.NX) .AND. (IY.GT.1) .AND.
     *         (IY.LT.NY)) THEN
               FSUM = FSUM + FLUX
               IF ((TYPE.EQ.0) .OR. ((TYPE.GT.0) .AND.
     *            (PARMS(1)*PARMS(2).EQ.0.0))) THEN
                  IX = X
                  IY = Y
                  X = X - IX
                  Y = Y - IY
                  IMAG(IX,IY) = IMAG(IX,IY) + (1.-X)*(1.-Y) * FLUX
                  IMAG(IX+1,IY) = IMAG(IX+1,IY) + X*(1.-Y) * FLUX
                  IMAG(IX,IY+1) = IMAG(IX,IY+1) + (1.-X)*Y * FLUX
                  IMAG(IX+1,IY+1) = IMAG(IX+1,IY+1) + X*Y * FLUX
               ELSE IF (TYPE.EQ.1) THEN
                  SR = SIN (PARMS(3)*DG2RAD)
                  CR = COS (PARMS(3)*DG2RAD)
                  AA = ((SR/PARMS(1))**2 + (CR/PARMS(2))**2) * AA0
                  BB = ((CR/PARMS(1))**2 + (SR/PARMS(2))**2) * BB0
                  CC = ((1.0/pARMS(2))**2 - (1.0/PARMS(1))**2) * SR * CR
     *               * CC0
                  NCNV = 4.0 * PARMS(1) / MIN (ABS(CATR(KRCIC)),
     *               ABS (CATR(KRCIC+1))) + 0.75
                  NCNV = NCNV / 2
                  IX1 = X + 1.0 - NCNV
                  IX1 = MAX (1, MIN (NX, IX1))
                  IX2 = X + NCNV
                  IX2 = MAX (1, MIN (NX, IX2))
                  IY1 = Y - NCNV
                  IY1 = MAX (1, MIN (NY, IY1))
                  IY2 = Y + NCNV
                  IY2 = MAX (1, MIN (NY, IY2))
                  SR = 0.0
                  DO 20 IY = IY1,IY2
                     DO 10 IX = IX1,IX2
                        D = (IX-X)*(IX-X)*AA + (IY-Y)*(IY-Y)*BB +
     *                     (IX-X)*(IY-Y)*CC
                        SR = SR + EXP (-D)
 10                     CONTINUE
 20                  CONTINUE
                  SR = FLUX / SR
                  DO 40 IY = IY1,IY2
                     DO 30 IX = IX1,IX2
                        D = (IX-X)*(IX-X)*AA + (IY-Y)*(IY-Y)*BB +
     *                     (IX-X)*(IY-Y)*CC
                        IMAG(IX,IY) = IMAG(IX,IY) + SR * EXP (-D)
 30                     CONTINUE
 40                  CONTINUE
                  END IF
               END IF
            END IF
 90      CONTINUE
      CALL TABCCM ('CLOS', CCBUFF, ICCRNO, CCKOLS, CCNUMV, NCOL, X,
     *   Y, FLUX, TYPE, PARMS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING CC FILE'
         CALL MSGWRT (7)
         END IF
C                                       find max/min
      GSUM = 0.0
      FMAX = -1.E10
      FMIN = -FMAX
      DO 190 IY = 1,NY
         DO 180 IX = 1,NX
            FMIN = MIN (FMIN, IMAG(IX,IY))
            FMAX = MAX (FMAX, IMAG(IX,IY))
            GSUM = GSUM + ABS (IMAG(IX,IY))
 180        CONTINUE
 190     CONTINUE
      IF ((FMAX.LE.FMIN) .OR. (GSUM.EQ.0.0)) THEN
         IRET = 8
         MSGTXT = 'NO USEFUL CLEAN COMPONENTS FOUND'
         GO TO 990
         END IF
      WRITE (MSGTXT,1190) FSUM
      CALL MSGWRT (5)
      IF (TSUM.NE.FSUM) THEN
         WRITE (MSGTXT,1191) TSUM
         CALL MSGWRT (5)
         END IF
C                                       Setup for I/O
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT IMAGE'
         GO TO 990
         END IF
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NX
      WINO(4) = NY
      IBLKOF = 1
      CALL MINIT ('WRIT', LUNO, INDO, NX, NY, WINO, BUFF2, JBUFSZ,
     *   IBLKOF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT I/O'
         GO TO 990
         END IF
C                                       Loop
      DO 220 IY = 1,NY
         CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT IMAGE'
            GO TO 990
            END IF
         CALL RCOPY (NX, IMAG(1,IY), BUFF2(OBIND))
 220     CONTINUE
      CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH WRITING OUTPUT'
         GO TO 990
         END IF
C                                       Update CATBLK.
      CATR(KRDMX) = FMAX
      CATR(KRDMN) = FMIN
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', CCBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'UPDATING THE OUTPUT HEADER'
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       totsl flux
      KEYW = 'CCFLUX'
      LOCS = 1
      KEYTYP = 2
      CALL CATKEY ('WRIT', DISKO, NEWCNO, KEYW, 1, LOCS, FSUM, KEYTYP,
     *   CCBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'UPDATING CCFLUX KEYWORD IN HEADER'
         IRET = 0
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CC2IMD: ERROR',I3,' ON ',A)
 1190 FORMAT ('Found',1PE12.4,' Jy in components')
 1191 FORMAT ('Total CC flux is different',1PE12.4)
      END
      SUBROUTINE CC2IMH
C-----------------------------------------------------------------------
C   CC2IMH copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR
      LOGICAL   T
      INCLUDE 'CC2IM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       don't copy keywords
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   CCBUFF(257), CCBUFF, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, CCBUFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   CCBUFF, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       INVERS
      WRITE (HILINE,2000) TSKNAM, CCVER
      CALL HIADD (LUN2, HILINE, CCBUFF, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       CELLSIZE
      CELL(1) = CELL(1) * 3600.
      CELL(2) = CELL(2) * 3600.
      WRITE (HILINE,2001) TSKNAM, CELL
      CALL HIADD (LUN2, HILINE, CCBUFF, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, CCBUFF, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, CCBUFF(257), CCBUFF, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', CCBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CC2IMH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'INVERS =',I5,'     / CC table version')
 2001 FORMAT (A6,'CELLSIZ =',F7.2,',',F7.2)
      END
