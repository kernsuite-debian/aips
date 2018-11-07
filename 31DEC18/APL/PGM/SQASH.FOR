LOCAL INCLUDE 'SQASH.INC'
      INCLUDE 'INCS:PMAD.INC'
      HOLLERITH XNAMIN(3), XCLSIN(2), XNMOUT(3), XCLOUT(2), XINTXT(12)
      REAL      XSEQI, XDISKI, XSEQO, XDISKO, XBLC(7), XTRC(7), XBDROP,
     *   DPARM(10)
      COMMON /INPARM/ XNAMIN, XCLSIN, XSEQI, XDISKI, XNMOUT, XCLOUT,
     *   XSEQO, XDISKO, XBLC, XTRC, XBDROP, DPARM, XINTXT
      CHARACTER NAMIN*12, CLSIN*6, NAMOUT*12, CLSOUT*6, INTEXT*48
      COMMON /CHPARM/ NAMIN, CLSIN, NAMOUT, CLSOUT, INTEXT
      INTEGER   CATIN(256), SCRTCH(256), DISKIN, DISKOU, CNOIN, CNOUT,
     *   SEQIN, SEQOU, LUNIN, INDIN, LUNOU, INDOU, IBDROP, IFUNC
      LOGICAL   SUM(7)
      COMMON /OTPARM/ CATIN, SCRTCH, DISKIN, DISKOU, CNOIN, CNOUT,
     *   SEQIN, SEQOU, LUNIN, INDIN, LUNOU, INDOU, IBDROP, SUM, IFUNC
      REAL      BUFF1(MABFSS), BUFF2(MABFSS)
      COMMON /BUPARM/ BUFF1, BUFF2
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
LOCAL END
      PROGRAM SQASH
C-----------------------------------------------------------------------
C! Task to sum or average planes along axes, writing to new file
C# Map-util Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2008-2009, 2011-2012, 2015, 2017
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
C   SQASH will copy a subarray of a cataloged file to a new file
C   summing or averaging the planes along one or more axes.
C   Inputs:  (from AIPS)
C      INNAME     R(3)   the entry name for the source file.  If
C                 blank the first match consistent with the other
C                 parameters is used.
C      INCLASS    R(2)   the class of the source file.  If blank
C                 the first match consistent with the other
C                 parameters is used.
C      INSEQ      R   the sequence number of the source file.  If
C                 zero the first match consistent with the other
C                 parameters is used.
C      INDISK     R   the disk volume number of the source file.
C                 If zero all disks are searched and the first
C                 match found is used.
C      OUTNAME    R(3)   the name of the new subimage file.  If
C                 blank the value in INNAME will be used.
C      OUTCLASS   R(2)   the class of the new subimage file.  If
C                 blank the value in INCLASS will be used.
C      OUTSEQ     R   the sequence number for the new subimage
C                 file.  If zero the first sequence number that
C                 will produce a unique file specification is
C                 used.
C      OUTDISK    R   the disk volume number for the new subimage
C                 file.  If zero the new file will be created
C                 on the same disk as the old file.
C      BLC        R(7)   the coordinate in the source file to
C                 become the bottom left hand coordinate (1,1)
C                 of the subimage.  BLC(1) is the X value and
C                 BLC(2) is the Y value.  The first coordinate
C                 IN the source map is (1,1). Any BLC(I) equal
C                 to zero defaults to 1.
C      TRC        R(7)   the coordinate in the source file to
C                 become the top right hand corner of the
C                 subimage.  The conventions used for BLC hold.
C      BDROP      The axis number over which the planes are summed or
C                 averaged.  > 0 just that axis, < 0 all axes >=
C                 -BDROP.
C      DPARM(10)  (1) = 0 average, = 1 sum, = 2 maximum
C                 (2) > 0 ignore blanked pixels, <= 0 enforce them.
C-----------------------------------------------------------------------
      INTEGER   IRET, NWORDS, NX, NY, I, J
      LONGINT   PWT, PWORK, PASUM, PACNT, PASQS
      REAL      WT(2), WORK(2), ASUM(2), ASQS(2)
      INTEGER   ACNT(2)
      CHARACTER PRGNAM*6
      INCLUDE 'SQASH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGNAM /'SQASH'/
C-----------------------------------------------------------------------
C                                       get inputs, create output
      CALL SQASHI (PRGNAM, NX, NY, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get dynamic memory for weights
      PWORK = 0
      PWT = 0
      IF (IFUNC.LE.2) THEN
         NWORDS = 1
         DO 10 I = 3,7
            J = XTRC(I) - XBLC(I) + 1.5
            NWORDS = NWORDS * J
 10         CONTINUE
         I = NWORDS
         NWORDS = (NWORDS - 1) / 1024 + 3
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, WT, PWT, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'CANNOT GET DYNAMIC MEMORY NEEDED'
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                       text file
         IF ((DPARM(3).GT.0.0) .AND. (INTEXT.NE.' ')) THEN
            CALL SQASHT (I, WT(1+PWT), IRET)
            IF (IRET.NE.0) GO TO 990
C                                       read each plane and compute
         ELSE
            NWORDS = (NX * NY - 1) / 1024 + 3
            CALL ZMEMRY ('GET ', TSKNAM, NWORDS, WORK, PWORK, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'CANNOT GET DYNAMIC MEMORY NEEDED'
               CALL MSGWRT (8)
               GO TO 990
               END IF
            CALL SQASHR (WT(1+PWT), NX, WORK(1+PWORK), IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
         END IF
C                                       do computation
C     IF (IBDROP.EQ.3) THEN
      NWORDS = (NX * NY - 1) / 1024 + 3
      IF (PWORK.EQ.0) THEN
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, WORK, PWORK, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'CANNOT GET DYNAMIC MEMORY NEEDED'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         END IF
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, ASUM, PASUM, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET DYNAMIC MEMORY NEEDED'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, ASQS, PASQS, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET DYNAMIC MEMORY NEEDED'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, ACNT, PACNT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET DYNAMIC MEMORY NEEDED'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      IF (IBDROP.GT.2) THEN
         CALL SQASH3 (WT(1+PWT), NX, WORK(1+PWORK), ASUM(1+PASUM),
     *      ASQS(1+PASQS), ACNT(1+PACNT), IRET)
      ELSE
         CALL SQASH2 (WT(1+PWT), NX, WORK(1+PWORK), ASUM(1+PASUM),
     *      ASQS(1+PASQS), ACNT(1+PACNT), IRET)
         END IF
C     ELSE
c         CALL SQASHD (WT(1+PWT), IRET)
c         END IF
      IF (IRET.NE.0) GO TO 990
C                                       history
      CALL SQASHH
C
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE SQASHI (PRGNAM, NX, NY, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      PRGNAM   C*6   Program name
C   Outputs:
C      NX       I     Number X pixels in output
C      NY       I     Number Y pixels in output
C      IRET     I     error code
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   NX, NY, IRET
C
      INTEGER   NPARMS, IERR, IUSER, INODIM, IROUND, I, JTRIM
      CHARACTER MTYPE*2, CFUNC(7)*16
      INCLUDE 'SQASH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA CFUNC /'Weighted mean', 'Weighted rms', 'Mean', 'Rms', 'Sum',
     *   'Maximum', 'Minimum'/
C-----------------------------------------------------------------------
C                                       Initialize the I/O parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      LUNIN = 17
      LUNOU = 18
C                                       Get input values from AIPS.
      NPARMS = 51
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAMIN, SCRTCH, IERR)
      IRET = IERR
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
C
 10   IF (RQUICK) CALL RELPOP (IRET, CATBLK, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (12, 1, XNMOUT, NAMOUT)
      CALL H2CHR (6, 1, XCLOUT, CLSOUT)
      CALL H2CHR (48, 1, XINTXT, INTEXT)
      SEQIN = IROUND (XSEQI)
      SEQOU = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKOU = IROUND (XDISKO)
C                                       Open source file.
      MTYPE = 'MA'
      IUSER = NLUSER
      CALL MAPOPN ('READ', DISKIN, NAMIN, CLSIN, SEQIN, MTYPE, IUSER,
     *   LUNIN, INDIN, CNOIN, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
      CALL COPY (256, CATBLK, CATIN)
C                                       Set default values BLC, TRC.
      INODIM = CATBLK(KIDIM)
      CALL WINDOW (INODIM, CATBLK(KINAX), XBLC, XTRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       check BDROP
      IBDROP = IROUND (XBDROP)
      IBDROP = ABS(IBDROP)
      IF ((IBDROP.GT.INODIM) .OR. (IBDROP.LT.2)) THEN
         WRITE (MSGTXT,1015) IBDROP, INODIM
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       function
      IFUNC = IROUND (DPARM(1)) + 1
      IF ((IFUNC.LT.1) .OR. (IFUNC.GT.7)) IFUNC = 1
      I = JTRIM (CFUNC(IFUNC))
      IF (XBDROP.LT.-0.49) THEN
         WRITE (MSGTXT,1025) CFUNC(IFUNC)(:I), IBDROP, INODIM
      ELSE
         WRITE (MSGTXT,1026) CFUNC(IFUNC)(:I), IBDROP
         END IF
      CALL MSGWRT (3)
C                                       output header
C                                       Build new file cat name.
      CALL MAKOUT (NAMIN, CLSIN, SEQIN, '      ', NAMOUT, CLSOUT,
     *   SEQOU)
      CALL SUBHDR (XBLC, XTRC, 1.0, 1.0)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLSOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOU
C                                       counters
      NX = CATIN(KINAX)
      NY = CATIN(KINAX+1)
      DO 20 I = 1,7
         SUM(I) = .FALSE.
         IF (I.EQ.IBDROP) SUM(I) = .TRUE.
         IF ((I.GT.IBDROP) .AND. (XBDROP.LT.-0.49)) SUM(I) = .TRUE.
 20      CONTINUE
      DO 25 I = 1,7
         IF (SUM(I)) CATBLK(KINAX+I-1) = 1
         IF (CATBLK(KINAX+I-1).LE.0) CATBLK(KINAX+I-1) = 1
 25      CONTINUE
C                                       Create new cataloged file.
      CALL MCREAT (DISKOU, CNOUT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKOU
      FCNO(NCFILE) = CNOUT
      FRW(NCFILE) = 2
      SEQOU = CATBLK(KIIMS)
C                                       copy most keywords
      CALL KEYPCP (DISKIN, CNOIN, DISKOU, CNOUT, 0, ' ', IERR)
C                                       Open new file.
      MTYPE = 'MA'
      CALL MAPOPN ('INIT', DISKOU, NAMOUT, CLSOUT, SEQOU, MTYPE, IUSER,
     *   LUNOU, INDOU, CNOUT, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. ERR=',I5)
 1015 FORMAT ('BAD VALUE OF BDROP =',I2,' NOT IN RANGE 2 -',I2)
 1025 FORMAT('*** determines ',A,' along axes ',I1,' thru ',I1,' ***')
 1026 FORMAT('*** determines ',A,' along axis ',I1,' ***')
      END
      SUBROUTINE SQASHH
C-----------------------------------------------------------------------
C   writes HI and tables
C-----------------------------------------------------------------------
C
      INTEGER   IERR, IC(7), I, INODIM, HLUN1, HLUN2, JTRIM
      CHARACTER HILINE*72, CFUNC(7)*16, NOTTYP(1)*2
      INCLUDE 'SQASH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA HLUN1, HLUN2 /27, 28/
      DATA CFUNC /'Weighted mean', 'Weighted rms', 'Mean', 'Rms', 'Sum',
     *   'Maximum', 'Minimum'/
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
      CALL HIINIT (3)
C                                       Create and copy history file.
      CALL HISCOP (HLUN1, HLUN2, DISKIN, DISKOU, CNOIN, CNOUT, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.3) GO TO 110
      CALL HENCO1 (TSKNAM, NAMIN, CLSIN, SEQIN, DISKIN, HLUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLSOUT, SEQOU, DISKOU, HLUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       corners
      INODIM = CATIN(KIDIM)
      DO 10 I = 1,INODIM
         IC(I) = XBLC(I) + 0.5
 10      CONTINUE
      WRITE (HILINE,1010) TSKNAM, 'BLC', (IC(I), I = 1,INODIM)
      I = JTRIM (HILINE)
      IF (HILINE(I:I).EQ.',') HILINE(I:I) = ' '
      CALL HIADD (HLUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      DO 15 I = 1,INODIM
         IC(I) = XTRC(I) + 0.5
 15      CONTINUE
      WRITE (HILINE,1010) TSKNAM, 'TRC', (IC(I), I = 1,INODIM)
      I = JTRIM (HILINE)
      IF (HILINE(I:I).EQ.',') HILINE(I:I) = ' '
      CALL HIADD (HLUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       function
      WRITE (HILINE,1015) TSKNAM, IFUNC, CFUNC(IFUNC)
      CALL HIADD (HLUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (IFUNC.LE.2) THEN
         IF (DPARM(3).GT.0.0) THEN
            I = JTRIM (INTEXT)
            WRITE (HILINE,1016) TSKNAM, INTEXT(:I)
         ELSE
            WRITE (HILINE,1017) TSKNAM
            END IF
         CALL HIADD (HLUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (XBDROP.LE.-0.49) THEN
         WRITE (HILINE,1020) TSKNAM, IBDROP, IBDROP, INODIM
      ELSE
         WRITE (HILINE,1021) TSKNAM, IBDROP, IBDROP
         END IF
      CALL HIADD (HLUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       update header
 100  CALL HICLOS (HLUN2, .TRUE., BUFF2, IERR)
C                                       copy tables, not CC
 110  CALL ALLTAB (1, NOTTYP, HLUN1, HLUN2, DISKIN, DISKOU, CNOIN,
     *   CNOUT, CATBLK, BUFF1, BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A,A,' =',2(I6,','),4(I5,','),I5)
 1015 FORMAT (A,'DPARM(1) =',I2,'    / compute ',A)
 1016 FORMAT (A,'INTEXT =''',A,'''  / weight file')
 1017 FORMAT (A,'  / weight from data')
 1020 FORMAT (A,'BDROP = -',I1,'  / summed over axes',I2,' thru',I2)
 1021 FORMAT (A,'BDROP = ',I2,'  / summed over axis',I2)
      END
      SUBROUTINE SQASHT (N, WT, IRET)
C-----------------------------------------------------------------------
C   SQASHT reads the text file of weights
C   Inputs:
C      N      I      Number wts expected
C   Outputs:
C      WT     R(*)   Weight array
C      IRET   I      Error code
C-----------------------------------------------------------------------
      INTEGER   N, IRET
      REAL      WT(*)
C
      INTEGER   I, KBP, LUN, LUNTMP, J, JTRIM, K, FIND
      DOUBLE PRECISION X
      CHARACTER LINE*132
      INCLUDE 'SQASH.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       open text file
      LUN = LUNTMP (2)
      CALL ZTXOPN ('READ', LUN, FIND, INTEXT, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE TEXT FILE'
         GO TO 990
         END IF
C                                       read loop
      I = 0
      K = 0
 20   K = K + 1
      CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
      IF (IRET.EQ.0) THEN
         J = JTRIM (LINE)
         CALL CHLTOU (J, LINE)
         IF ((J.LE.0) .OR. (LINE(:1).EQ.'#') .OR. ((LINE(:1).GE.'A')
     *      .AND. (LINE(:1).LE.'Z'))) GO TO 20
         KBP = 1
         CALL GETNUM (LINE, J, KBP, X)
         IF (X.EQ.DBLANK) THEN
            WRITE (MSGTXT,1020) K
            CALL MSGWRT (7)
         ELSE
            I = I + 1
            IF (I.LE.N) WT(I) = X
            END IF
         GO TO 20
      ELSE IF (IRET.NE.2) THEN
         WRITE (MSGTXT,1000) IRET, 'READING TEXT FILE'
         GO TO 990
         END IF
      IRET = 0
      K = K - 1
      CALL ZTXCLS (LUN, FIND, KBP)
      IF (I.EQ.N) THEN
         WRITE (MSGTXT,1100) I, K
         CALL MSGWRT (3)
      ELSE
         WRITE (MSGTXT,1101) I, N, K
         CALL MSGWRT (7)
         IF (I.LT.N) IRET = 10
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SQASHT: ERROR',I4,' ON ',A)
 1020 FORMAT ('SQASHT: TEXT FILE NUMBER ERROR LINE',I5)
 1100 FORMAT ('SQASHT: read',I6,' weights in',I6,' text file lines')
 1101 FORMAT ('SQASHT: READ',I6,' WEIGHTS, EXPECTED',I6,' IN',I6,
     *   ' LINES')
      END
      SUBROUTINE SQASHR (WT, NX, WORK, IRET)
C-----------------------------------------------------------------------
C   SQASHR reads the image a plane at a time and fits a robust rms to
C   each plane returning WT = 1 / (rms**2).
C   Inputs:
C      NX     I      Number X points per row
C   Outputs:
C      WT     R(*)   Weights for each plane
C      WORK   R(*)   work area for each plane
C      IRET   I      error code
C-----------------------------------------------------------------------
      INTEGER   NX, IRET
      REAL      WT(*), WORK(NX,*)
C
      INTEGER   IROUND, IX3L, IX3U, IX4L, IX4U, IX5L, IX5U, IX6L, IX6U,
     *   IX7L, IX7U, I3, I4, I5, I6, I7, IDEPTH(5), IWIN(4), N, NY, NBY,
     *   IX, IY, IBIND, IPASS, IBLKOF
      REAL      RSC(9)
      DOUBLE PRECISION SS, SQ, SN, RSP, RSM, RM, RS, T
      INCLUDE 'SQASH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA RSC /6.0, 5.0, 4.0, 3.5, 3.0, 2.8, 2.6, 3.2, 3.2/
C-----------------------------------------------------------------------
      NBY = 2 * MABFSS
      IX3L = IROUND (XBLC(3))
      IX3U = IROUND (XTRC(3))
      IX4L = IROUND (XBLC(4))
      IX4U = IROUND (XTRC(4))
      IX5L = IROUND (XBLC(5))
      IX5U = IROUND (XTRC(5))
      IX6L = IROUND (XBLC(6))
      IX6U = IROUND (XTRC(6))
      IX7L = IROUND (XBLC(7))
      IX7U = IROUND (XTRC(7))
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = CATIN(KINAX)
      IWIN(4) = CATIN(KINAX+1)
      NY = CATIN(KINAX+1)
      N = 0
      DO 200 I7 = IX7L,IX7U
         DO 190 I6 = IX6L,IX6U
            DO 180 I5 = IX5L,IX5U
               DO 170 I4 = IX4L,IX4U
                  DO 160 I3 = IX3L,IX3U
                     IDEPTH(1) = I3
                     IDEPTH(2) = I4
                     IDEPTH(3) = I5
                     IDEPTH(4) = I6
                     IDEPTH(5) = I7
                     CALL COMOFF (CATIN(KIDIM), CATIN(KINAX), IDEPTH,
     *                  IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'COMOFF CALL'
                        GO TO 990
                        END IF
                     IBLKOF = IBLKOF + 1
C                                       init I/O this plane
                     CALL MINIT ('READ', LUNIN, INDIN, NX, NY, IWIN,
     *                  BUFF1, NBY, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT I/O TO INPUT'
                        GO TO 990
                        END IF
C                                       fill plane
                     DO 20 IY = 1,NY
                        CALL MDISK ('READ', LUNIN, INDIN, BUFF1, IBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'READING INPUT IMAGE'
                           GO TO 990
                           END IF
                        CALL RCOPY (NX, BUFF1(IBIND), WORK(1,IY))
 20                     CONTINUE
C                                       find rms
                     RSP = 1.E10
                     RSM = -1.E10
                     RM = 0.0D0
                     RS = 1.D6
                     DO 50 IPASS = 1,9
                        SS = 0.0D0
                        SQ = 0.0D0
                        SN = 0.0D0
                        DO 40 IY = 1,NY
                           DO 30 IX = 1,NX
                              IF (WORK(IX,IY).NE.FBLANK) THEN
                                 T = WORK(IX,IY)
                                 IF ((T.GE.RSM) .AND. (T.LE.RSP)) THEN
                                    SS = SS + T
                                    SQ = SQ + T * T
                                    SN = SN + 1.0D0
                                    END IF
                                 END IF
 30                           CONTINUE
 40                        CONTINUE
                        IF (SN.LE.0.0D0) THEN
                           RSP = RSP + 3.0D0 * RS
                           RSM = RSM - 3.0D0 * RS
                        ELSE
                           RM = SS / SN
                           RS = SQ / SN - RM * RM
                           RS = SQRT (MAX (0.0D0, RS))
                           IF (RS.EQ.0.0D0) RS = RM/3.0D0
                           RSP = RM + RSC(IPASS) * RS
                           RSM = RM - RSC(IPASS) * RS
                           END IF
 50                     CONTINUE
C                                       squirrel away a weight
                     N = N + 1
                     IF (RS.GT.0.0) THEN
                        WT(N) = 1.0D0 / (RS * RS)
                     ELSE
                        WT(N) = 0.0
                        END IF
                     WRITE (MSGTXT,1050) WT(N), I3, I4, I5, I6, I7
                     CALL MSGWRT (3)
 160                 CONTINUE
 170              CONTINUE
 180           CONTINUE
 190        CONTINUE
 200     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SQASHR: ERROR',I4,' ON ',A)
 1050 FORMAT ('SQASHR weight',F14.3,' plane',I7,I5,3I4)
      END
      SUBROUTINE PLANE (IDEPTH, XBLC, XTRC, JPL)
C-----------------------------------------------------------------------
C   PLANE finds the plane number currently
C   Inputs:
C      IDEPTH   I(5)   Current input file depth
C      XBLC     R(7)   bottom left corner of input
C      XTRC     R(7)   top right corner of input
C   Output:
C      JPL      I      Plane number
C-----------------------------------------------------------------------
      INTEGER   IDEPTH(5), JPL
      REAL      XBLC(7), XTRC(7)
C
      INTEGER   IBLC, NPIX, NTOT(6), I
C-----------------------------------------------------------------------
      NTOT(1) = 1
      JPL = 1
      DO 10 I = 1,5
         IBLC = XBLC(I+2) + 0.1
         NPIX = XTRC(I+2) + 1.1 - IBLC
         NTOT(I+1) = NTOT(I) * NPIX
         JPL = JPL + (IDEPTH(I)-IBLC) * NTOT(I)
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SQASH3 (WT, NX, WORK, ASUM, ASQS, ACNT, IRET)
C-----------------------------------------------------------------------
C   SQASHR does the squash operation
C   Inputs:
C      WT     R(*)   Weights for each plane
C      NX     I      Row size
C   Outputs:
C      WORK   R(*)   work buffer
C      ASUM   R(*)   work buffer
C      ICNT   I(*)   work buffer
C      IRET   I      error code
C-----------------------------------------------------------------------
      INTEGER   NX, ACNT(NX,*), IRET
      REAL      WT(*), WORK(NX,*), ASQS(NX,*), ASUM(NX,*)
C
      INCLUDE 'SQASH.INC'
      INTEGER   IROUND, IX3L, IX3U, IX4L, IX4U, IX5L, IX5U, IX6L, IX6U,
     *   IX7L, IX7U, I3, I4, I5, I6, I7, IDEPTH(5), IWIN(4), NY, NBY,
     *   IX, IY, IBIND, IBLKOF, K3, K4, K5, K6, K7, K3U, K4U, K5U, K6U,
     *   K7U, OWIN(4), OBIND, NCNTMX, JPL, INY, INX
      REAL      RMAX, RMIN, W, T
      LOGICAL   WASBLK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      NBY = 2 * MABFSS
      IX3L = IROUND (XBLC(3))
      IX3U = IROUND (XTRC(3))
      IX4L = IROUND (XBLC(4))
      IX4U = IROUND (XTRC(4))
      IX5L = IROUND (XBLC(5))
      IX5U = IROUND (XTRC(5))
      IX6L = IROUND (XBLC(6))
      IX6U = IROUND (XTRC(6))
      IX7L = IROUND (XBLC(7))
      IX7U = IROUND (XTRC(7))
      K3U = CATBLK(KINAX+2)
      K4U = CATBLK(KINAX+3)
      K5U = CATBLK(KINAX+4)
      K6U = CATBLK(KINAX+5)
      K7U = CATBLK(KINAX+6)
      NY = CATIN(KINAX+1)
      IWIN(1) = IROUND (XBLC(1))
      IWIN(2) = IROUND (XBLC(2))
      IWIN(3) = IROUND (XTRC(1))
      IWIN(4) = IROUND (XTRC(2))
      INX = IWIN(3) - IWIN(1) + 1
      INY = IWIN(4) - IWIN(2) + 1
      OWIN(1) = 1
      OWIN(2) = 1
      OWIN(3) = INX
      OWIN(4) = INY
      RMAX = -1.E10
      RMIN = -RMAX
      WASBLK = .FALSE.
      DO 200 K7 = 1,K7U
         DO 190 K6 = 1,K6U
            DO 180 K5 = 1,K5U
               DO 170 K4 = 1,K4U
                  DO 160 K3 = 1,K3U
                     IDEPTH(1) = K3
                     IDEPTH(2) = K4
                     IDEPTH(3) = K5
                     IDEPTH(4) = K6
                     IDEPTH(5) = K7
                     CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH,
     *                  IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'COMOFF FOR WRITE'
                        GO TO 990
                        END IF
                     IBLKOF = IBLKOF + 1
C
                     CALL MINIT ('WRIT', LUNOU, INDOU, INX, INY, OWIN,
     *                  BUFF2, NBY, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT WRITE I/O'
                        GO TO 990
                        END IF
C                                       Which planes contribute
                     IF (.NOT.SUM(7)) THEN
                        IX7L = K7 + XBLC(7) - 0.999
                        IX7U = IX7L
                        END IF
                     IF (.NOT.SUM(6)) THEN
                        IX6L = K6 + XBLC(6) - 0.999
                        IX6U = IX6L
                        END IF
                     IF (.NOT.SUM(5)) THEN
                        IX5L = K5 + XBLC(5) - 0.999
                        IX5U = IX5L
                        END IF
                     IF (.NOT.SUM(4)) THEN
                        IX4L = K4 + XBLC(4) - 0.999
                        IX4U = IX4L
                        END IF
                     IF (.NOT.SUM(3)) THEN
                        IX3L = K3 + XBLC(3) - 0.999
                        IX3U = IX3L
                        END IF
C                                       init the buffer
                     IY = INX * INY
                     IF (IFUNC.EQ.7) THEN
                        CALL RFILL (IY, 1.E10, WORK)
                     ELSE IF (IFUNC.EQ.6) THEN
                        CALL RFILL (IY, -1.E10, WORK)
                     ELSE
                        CALL RFILL (IY, 0.0, WORK)
                        CALL RFILL (IY, 0.0, ASUM)
                        CALL RFILL (IY, 0.0, ASQS)
                        END IF
                     CALL FILL (IY, 0, ACNT)
                     NCNTMX = 0
                     DO 130 I7 = IX7L,IX7U
                        DO 120 I6 = IX6L,IX6U
                           DO 110 I5 = IX5L,IX5U
                              DO 100 I4 = IX4L,IX4U
                                 DO 90 I3 = IX3L,IX3U
                                    IDEPTH(1) = I3
                                    IDEPTH(2) = I4
                                    IDEPTH(3) = I5
                                    IDEPTH(4) = I6
                                    IDEPTH(5) = I7
                                    CALL PLANE (IDEPTH, XBLC, XTRC, JPL)
                                    CALL COMOFF (CATIN(KIDIM),
     *                                 CATIN(KINAX), IDEPTH, IBLKOF,
     *                                 IRET)
                                    IF (IRET.NE.0) THEN
                                       WRITE (MSGTXT,1000) IRET,
     *                                    'COMOFF CALL'
                                       GO TO 990
                                       END IF
                                    IBLKOF = IBLKOF + 1
                                    CALL MINIT ('READ', LUNIN, INDIN,
     *                                 NX, NY, IWIN, BUFF1, NBY, IBLKOF,
     *                                 IRET)
                                    IF (IRET.NE.0) THEN
                                       WRITE (MSGTXT,1000) IRET,
     *                                    'INIT I/O TO INPUT'
                                       GO TO 990
                                       END IF
                                    NCNTMX = NCNTMX + 1
                                    DO 80 IY = 1,INY
                                       CALL MDISK ('READ', LUNIN, INDIN,
     *                                    BUFF1, IBIND, IRET)
                                       IF (IRET.NE.0) THEN
                                          WRITE (MSGTXT,1000) IRET,
     *                                       'READING INPUT IMAGE'
                                          GO TO 990
                                          END IF
                                       W = 1.0
                                       IF (IFUNC.LE.2) W = WT(JPL)
                                       DO 70 IX = 1,INX
                                          T = BUFF1(IX+IBIND-1)
                                          IF (T.NE.FBLANK) THEN
                                             IF (IFUNC.EQ.7) THEN
                                                WORK(IX,IY) = MIN (T,
     *                                             WORK(IX,IY))
                                             ELSE IF (IFUNC.EQ.6) THEN
                                                WORK(IX,IY) = MAX (T,
     *                                             WORK(IX,IY))
                                             ELSE
                                                WORK(IX,IY) = W * T
     *                                             + WORK(IX,IY)
                                                ASQS(IX,IY) = W * T * T
     *                                             + ASQS(IX,IY)
                                                END IF
                                             ASUM(IX,IY) = ASUM(IX,IY)+W
                                             ACNT(IX,IY) = ACNT(IX,IY)+1
                                             END IF
 70                                       CONTINUE
 80                                    CONTINUE
 90                                 CONTINUE
 100                             CONTINUE
 110                          CONTINUE
 120                       CONTINUE
 130                    CONTINUE
C                                       test and average
                     DO 150 IY = 1,INY
                        CALL MDISK ('WRIT', LUNOU, INDOU, BUFF2, OBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET, 'WRITE A ROW'
                           GO TO 990
                           END IF
                        DO 140 IX = 1,INX
                           T = WORK(IX,IY)
                           IF ((ACNT(IX,IY).LE.0) .OR.
     *                        (ASUM(IX,IY).LE.0.0)) THEN
                              T = FBLANK
                           ELSE IF ((DPARM(2).GT.0.0) .AND.
     *                        (ACNT(IX,IY).LT.NCNTMX)) THEN
                              T = FBLANK
                           ELSE
                              IF (IFUNC.LE.4) T = T / ASUM(IX,IY)
                              IF ((IFUNC.EQ.2) .OR. (IFUNC.EQ.4)) THEN
                                 T = ASQS(IX,IY) / ASUM(IX,IY) - T * T
                                 T = SQRT (MAX (0.0, T))
                                 END IF
                              RMAX = MAX (RMAX, T)
                              RMIN = MIN (RMIN, T)
                              END IF
                           BUFF2(OBIND+IX-1) = T
                           IF (T.EQ.FBLANK) WASBLK = .TRUE.
 140                       CONTINUE
 150                    CONTINUE
C                                       write last buffer
                     CALL MDISK ('FINI', LUNOU, INDOU, BUFF2, OBIND,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'WRITE A ROW'
                        GO TO 990
                        END IF
 160                 CONTINUE
 170              CONTINUE
 180           CONTINUE
 190        CONTINUE
 200     CONTINUE
C                                       anything?
      IF (RMAX.LT.RMIN) THEN
         MSGTXT = 'NO VALID PIXELS FOUND'
         IRET = 10
         GO TO 990
         END IF
      CATR(KRDMN) = RMIN
      CATR(KRDMX) = RMAX
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SQASH3: ERROR',I4,' ON ',A)
      END
      SUBROUTINE SQASH2 (WT, NX, WORK, ASUM, ASQS, ACNT, IRET)
C-----------------------------------------------------------------------
C   SQASHR does the squash operation when axis 2 is included
C   Inputs:
C      WT     R(*)   Weights for each plane
C      NX     I      Row size
C   Outputs:
C      WORK   R(*)   work buffer
C      ASUM   R(*)   work buffer
C      ICNT   I(*)   work buffer
C      IRET   I      error code
C-----------------------------------------------------------------------
      INTEGER   NX, ACNT(NX,*), IRET
      REAL      WT(*), WORK(NX,*), ASQS(NX,*), ASUM(NX,*)
C
      INCLUDE 'SQASH.INC'
      INTEGER   IROUND, IX3L, IX3U, IX4L, IX4U, IX5L, IX5U, IX6L, IX6U,
     *   IX7L, IX7U, I3, I4, I5, I6, I7, IDEPTH(5), IWIN(4), NY, NBY,
     *   IX, IY, IBIND, IBLKOF, K3, K4, K5, K6, K7, K3U, K4U, K5U, K6U,
     *   K7U, OWIN(4), OBIND, NCNTMX, JPL, INY, INX
      REAL      RMAX, RMIN, W, T
      LOGICAL   WASBLK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      NBY = 2 * MABFSS
      IX3L = IROUND (XBLC(3))
      IX3U = IROUND (XTRC(3))
      IX4L = IROUND (XBLC(4))
      IX4U = IROUND (XTRC(4))
      IX5L = IROUND (XBLC(5))
      IX5U = IROUND (XTRC(5))
      IX6L = IROUND (XBLC(6))
      IX6U = IROUND (XTRC(6))
      IX7L = IROUND (XBLC(7))
      IX7U = IROUND (XTRC(7))
      K3U = CATBLK(KINAX+2)
      K4U = CATBLK(KINAX+3)
      K5U = CATBLK(KINAX+4)
      K6U = CATBLK(KINAX+5)
      K7U = CATBLK(KINAX+6)
      NY = CATIN(KINAX+1)
      IWIN(1) = IROUND (XBLC(1))
      IWIN(2) = IROUND (XBLC(2))
      IWIN(3) = IROUND (XTRC(1))
      IWIN(4) = IROUND (XTRC(2))
      INX = IWIN(3) - IWIN(1) + 1
      INY = IWIN(4) - IWIN(2) + 1
      OWIN(1) = 1
      OWIN(2) = 1
      OWIN(3) = INX
      OWIN(4) = INY
      RMAX = -1.E10
      RMIN = -RMAX
      WASBLK = .FALSE.
      DO 200 K7 = 1,K7U
         DO 190 K6 = 1,K6U
            DO 180 K5 = 1,K5U
               DO 170 K4 = 1,K4U
                  DO 160 K3 = 1,K3U
                     IDEPTH(1) = K3
                     IDEPTH(2) = K4
                     IDEPTH(3) = K5
                     IDEPTH(4) = K6
                     IDEPTH(5) = K7
                     CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH,
     *                  IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'COMOFF FOR WRITE'
                        GO TO 990
                        END IF
                     IBLKOF = IBLKOF + 1
C
                     CALL MINIT ('WRIT', LUNOU, INDOU, INX, INY, OWIN,
     *                  BUFF2, NBY, IBLKOF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT WRITE I/O'
                        GO TO 990
                        END IF
C                                       Which planes contribute
                     IF (.NOT.SUM(7)) THEN
                        IX7L = K7 + XBLC(7) - 0.999
                        IX7U = IX7L
                        END IF
                     IF (.NOT.SUM(6)) THEN
                        IX6L = K6 + XBLC(6) - 0.999
                        IX6U = IX6L
                        END IF
                     IF (.NOT.SUM(5)) THEN
                        IX5L = K5 + XBLC(5) - 0.999
                        IX5U = IX5L
                        END IF
                     IF (.NOT.SUM(4)) THEN
                        IX4L = K4 + XBLC(4) - 0.999
                        IX4U = IX4L
                        END IF
                     IF (.NOT.SUM(3)) THEN
                        IX3L = K3 + XBLC(3) - 0.999
                        IX3U = IX3L
                        END IF
C                                       init the buffer
                     IY = INX * INY
                     IF (IFUNC.EQ.7) THEN
                        CALL RFILL (IY, 1.E10, WORK)
                     ELSE IF (IFUNC.EQ.6) THEN
                        CALL RFILL (IY, -1.E10, WORK)
                     ELSE
                        CALL RFILL (IY, 0.0, WORK)
                        CALL RFILL (IY, 0.0, ASUM)
                        CALL RFILL (IY, 0.0, ASQS)
                        END IF
                     CALL FILL (IY, 0, ACNT)
                     NCNTMX = 0
                     DO 130 I7 = IX7L,IX7U
                        DO 120 I6 = IX6L,IX6U
                           DO 110 I5 = IX5L,IX5U
                              DO 100 I4 = IX4L,IX4U
                                 DO 90 I3 = IX3L,IX3U
                                    IDEPTH(1) = I3
                                    IDEPTH(2) = I4
                                    IDEPTH(3) = I5
                                    IDEPTH(4) = I6
                                    IDEPTH(5) = I7
                                    CALL PLANE (IDEPTH, XBLC, XTRC, JPL)
                                    CALL COMOFF (CATIN(KIDIM),
     *                                 CATIN(KINAX), IDEPTH, IBLKOF,
     *                                 IRET)
                                    IF (IRET.NE.0) THEN
                                       WRITE (MSGTXT,1000) IRET,
     *                                    'COMOFF CALL'
                                       GO TO 990
                                       END IF
                                    IBLKOF = IBLKOF + 1
                                    CALL MINIT ('READ', LUNIN, INDIN,
     *                                 NX, INY, IWIN, BUFF1, NBY,
     *                                 IBLKOF, IRET)
                                    IF (IRET.NE.0) THEN
                                       WRITE (MSGTXT,1000) IRET,
     *                                    'INIT I/O TO INPUT'
                                       GO TO 990
                                       END IF
                                    NCNTMX = NCNTMX + 1
                                    DO 80 IY = 1,INY
                                       CALL MDISK ('READ', LUNIN, INDIN,
     *                                    BUFF1, IBIND, IRET)
                                       IF (IRET.NE.0) THEN
                                          WRITE (MSGTXT,1000) IRET,
     *                                       'READING INPUT IMAGE'
                                          GO TO 990
                                          END IF
                                       W = 1.0
                                       IF (IFUNC.LE.2) W = WT(JPL)
                                       DO 70 IX = 1,INX
                                          T = BUFF1(IX+IBIND-1)
                                          IF (T.NE.FBLANK) THEN
                                             IF (IFUNC.EQ.7) THEN
                                                WORK(IX,1) = MIN (T,
     *                                             WORK(IX,1))
                                             ELSE IF (IFUNC.EQ.6) THEN
                                                WORK(IX,1) = MAX (T,
     *                                             WORK(IX,1))
                                             ELSE
                                                WORK(IX,1) = W * T
     *                                             + WORK(IX,1)
                                                ASQS(IX,1) = W * T * T
     *                                             + ASQS(IX,1)
                                                END IF
                                             ASUM(IX,1) = ASUM(IX,1)+W
                                             ACNT(IX,1) = ACNT(IX,1)+1
                                             END IF
 70                                       CONTINUE
 80                                    CONTINUE
 90                                 CONTINUE
 100                             CONTINUE
 110                          CONTINUE
 120                       CONTINUE
 130                    CONTINUE
C                                       test and average
                     DO 150 IY = 1,INY
                        CALL MDISK ('WRIT', LUNOU, INDOU, BUFF2, OBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET, 'WRITE A ROW'
                           GO TO 990
                           END IF
                        DO 140 IX = 1,INX
                           T = WORK(IX,IY)
                           IF ((ACNT(IX,IY).LE.0) .OR.
     *                        (ASUM(IX,IY).LE.0.0)) THEN
                              T = FBLANK
                           ELSE IF ((DPARM(2).GT.0.0) .AND.
     *                        (ACNT(IX,IY).LT.NCNTMX)) THEN
                              T = FBLANK
                           ELSE
                              IF (IFUNC.LE.4) T = T / ASUM(IX,IY)
                              IF ((IFUNC.EQ.2) .OR. (IFUNC.EQ.4)) THEN
                                 T = ASQS(IX,IY) / ASUM(IX,IY) - T * T
                                 T = SQRT (MAX (0.0, T))
                                 END IF
                              RMAX = MAX (RMAX, T)
                              RMIN = MIN (RMIN, T)
                              END IF
                           BUFF2(OBIND+IX-1) = T
                           IF (T.EQ.FBLANK) WASBLK = .TRUE.
 140                       CONTINUE
 150                    CONTINUE
C                                       write last buffer
                     CALL MDISK ('FINI', LUNOU, INDOU, BUFF2, OBIND,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'WRITE A ROW'
                        GO TO 990
                        END IF
 160                 CONTINUE
 170              CONTINUE
 180           CONTINUE
 190        CONTINUE
 200     CONTINUE
C                                       anything?
      IF (RMAX.LT.RMIN) THEN
         MSGTXT = 'NO VALID PIXELS FOUND'
         IRET = 10
         GO TO 990
         END IF
      CATR(KRDMN) = RMIN
      CATR(KRDMX) = RMAX
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SQASH2: ERROR',I4,' ON ',A)
      END
