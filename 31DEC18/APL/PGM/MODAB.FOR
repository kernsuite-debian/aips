LOCAL INCLUDE 'MODAB.INC'
C                                       Local include for MODAB
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAXGAU
      PARAMETER (MAXGAU = 99)
C
      HOLLERITH XNAMOU(3), XINLST(12), XOPTYP
      REAL      XSEQO, XDISKO, FLUX, COORD(6), XIMSIZ(2), CELLS(2),
     *   APARM(10)
      REAL      BUFF(MABFSL), PTAU(MAXGAU), PCHAN(MAXGAU), PWID(MAXGAU),
     *   PSEP(MAXGAU)
      INTEGER   SEQO(2), DISKO(2), NPOL, NEWCNO(2), JBUFSZ,
     *   CATNEW(256,2), NGAUSS, SCRTCH(256)
      CHARACTER NAMOUT*12, CLASO(2)*6, INLIST*48, OPTYPE*4
      LOGICAL   DONEW, IVIN
      COMMON /INPARM/ XNAMOU, XSEQO, XDISKO, FLUX, XOPTYP, XINLST,
     *   COORD, XIMSIZ, CELLS, APARM
      COMMON /CHRCOM/ NAMOUT, CLASO, INLIST, OPTYPE
      COMMON /PARMS/ CATNEW, SEQO, DISKO, NPOL, NEWCNO, JBUFSZ, DONEW,
     *   NGAUSS, IVIN
      COMMON /BUFRS/ PTAU, PCHAN, PWID, PSEP, BUFF, SCRTCH
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       End MODAB
LOCAL END
      PROGRAM MODAB
C-----------------------------------------------------------------------
C! makes image with I/V absorption line(s)
C# Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2017
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
C   MODAB is an AIPS task to create an image with a model - specifically
C   a spectral model with Zeeman absorption options
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTSEQ         SEQO          Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      FLUX           FLUX          Noise level in Jy/Pix.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NX, NY, NEED
      LONGINT   PIMAGE
      REAL      IMAGE(2)
      INCLUDE 'MODAB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'MODAB '/
C-----------------------------------------------------------------------
C                                       Get inputs, create output file
      CALL MODABI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get memory for 3 planes
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
      NEED = (NX * NY - 1) / 1024 + 4
      NEED = 3 * NEED
      CALL ZMEMRY ('GET ', TSKNAM, NEED, IMAGE, PIMAGE, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'FAILED TO GET NEEDED DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Apply model to image
      CALL MODABD (NX, NY, IMAGE(1+PIMAGE), IRET)
C                                       Add history
      IF (IRET.EQ.0) CALL MODABH
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE MODABI (PRGN, IRET)
C-----------------------------------------------------------------------
C   MODABI gets input parameters for MODAB and creates an output file.
C   Inputs:
C      PRGN    C*6       Program name
C   Output:
C      IRET    I         Error code: 0 => ok
C                           4 => user routine detected error.
C                           5 => catalog troubles
C                           8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER MTYPE*2
      INTEGER   IERR, NPARM, IROUND, I, IST
      INCLUDE 'MODAB.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSL
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 39
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMOU, SCRTCH, IERR)
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
      NPOL = 1
C                                       Crunch input parameters.
      SEQO(1) = IROUND (XSEQO)
      SEQO(2) = IROUND (XSEQO)
      DISKO(1) = IROUND (XDISKO)
      DISKO(2) = IROUND (XDISKO)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CLASO(1) = 'IMODEL'
      CLASO(2) = 'VMODEL'
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (48, 1, XINLST, INLIST)
      IF (INLIST.EQ.' ') THEN
         MSGTXT = 'AN INLIST MUST BE SPECIFIED'
         GO TO 990
         END IF
      IF (NAMOUT.EQ.' ') NAMOUT = 'Absorption'
C                                       get components, err msg if error
      CALL READIT (IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGTXT,1005) NGAUSS
      CALL MSGWRT (3)
      IF (NGAUSS.LE.0) THEN
         IRET = 10
         GO TO 999
         END IF
      DONEW = .TRUE.
      MTYPE = 'MA'
C                                       Make a new header
      IVIN = .FALSE.
      CALL NEWHDR
C                                       create outputs
      DO 40 I = 1,2
C                                       Copy old CATBLK to new.
         CALL COPY (256, CATNEW(1,I), CATBLK)
C                                       Put new values in CATBLK.
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLASO(I), KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQO(I)
C                                       Create output file.
         NEWCNO(I) = 1
         IRET = 4
         CALL MCREAT (DISKO(I), NEWCNO(I), SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR, I
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO(I)
         FCNO(NCFILE) = NEWCNO(I)
         FRW(NCFILE) = 2
         SEQO(I) = CATBLK(KIIMS)
C                                       set Stokes value
         CALL AXEFND (8, 'STOKES  ', CATBLK(KIDIM), CATH(KHCTP), IST,
     *      IERR)
         CATBLK(KDCRV+IST) = 1.0D0 + 3.0D0 * (I-1)
         CATR(KRCIC+IST) = 1.0
         CATR(KRCRP+IST) = 1.0
         CALL COPY (256, CATBLK, CATNEW(1,I))
 40      CONTINUE
C                                       init random number generator
      IF (FLUX.GT.0.0) CALL RANDIN (I)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODABI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1005 FORMAT ('Will use',I5,' model components')
 1030 FORMAT ('MODABI: ERROR',I3,' CREATING OUTPUT FILE',I2)
      END
      SUBROUTINE NEWHDR
C-----------------------------------------------------------------------
C   NEWHDR makes up image headers from scratch
C-----------------------------------------------------------------------
C
      INTEGER   DATE(3), I
      CHARACTER STRNG*8
      INCLUDE 'MODAB.INC'
C-----------------------------------------------------------------------
C                                       blank slate
      CALL CATINI (CATBLK)
      CALL CHR2H (8, 'IVmodab', 1, CATH(KHOBJ))
      CALL CHR2H (8, 'JY/BEAM ', 1, CATH(KHBUN))
      CALL ZDATE (DATE)
      WRITE (STRNG,1000) DATE
      CALL CHR2H (8, STRNG, 1, CATH(KHDMP))
      CALL CHR2H (8, STRNG, 1, CATH(KHDOB))
      CATR(KREPO) = 2000.0
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIDIM) = 4
C                                       RA
      I = XIMSIZ(1) + 0.5
      IF (I.LE.0) I = 512
      CATBLK(KINAX) = I
      CATD(KDCRV) = (COORD(1)*15.D0 + COORD(2)/4.D0 + COORD(3)/240.D0)
      CATR(KRCRP) = (I + 1) / 2
      IF (CELLS(1).EQ.0.0) CELLS(1) = 1.
      CATR(KRCIC) = -ABS(CELLS(1)) / 3600.0
      CALL CHR2H (8, 'RA---SIN', 1, CATH(KHCTP))
C                                       DEC
      I = XIMSIZ(2) + 0.5
      IF (I.LE.0) I = 512
      CATBLK(KINAX+1) = I
      CATD(KDCRV+1) = (COORD(4) + COORD(5)/60.D0 + COORD(6)/3600.D0)
      CATR(KRCRP+1) = (I + 2) / 2
      IF (CELLS(1).EQ.0.0) CELLS(1) = 1.
      CATR(KRCIC+1) = ABS(CELLS(1)) / 3600.0
      CALL CHR2H (8, 'DEC--SIN', 1, CATH(KHCTP+2))
C                                       FREQ
      I = APARM(3) + 0.5
      IF (I.LE.0) I = 512
      CATBLK(KINAX+2) = I
      CATD(KDCRV+2) = APARM(1) * 1.D9
      CATR(KRCRP+2) = 1.0
      IF (APARM(2).EQ.0.0) APARM(2) = 0.001
      CATR(KRCIC+2) = APARM(2) * 1.E9
      CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP+4))
C                                       STOKES
      CATBLK(KINAX+3) = 1
      CATD(KDCRV+3) = 1.0D0
      CATR(KRCRP+3) = 1.0
      CATR(KRCIC+3) = 1.0
      CALL CHR2H (8, 'STOKES  ', 1, CATH(KHCTP+6))
C                                       clean beam fake
      CATR(KRBMJ) = 3.0 * ABS (CATR(KRCIC))
      CATR(KRBMN) = 3.0 * ABS (CATR(KRCIC))
C                                       to output
      CALL COPY (256, CATBLK, CATNEW(1,1))
      CATD(KDCRV+3) = 4.0D0
      CALL COPY (256, CATBLK, CATNEW(1,2))
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4,2I2.2)
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
      INCLUDE 'MODAB.INC'
      INTEGER   TLUN, TIND, LUNTMP, LLIM, LP, JTRIM
      CHARACTER LINE*132
      DOUBLE PRECISION X
C-----------------------------------------------------------------------
C                                       read text file
      TLUN = LUNTMP (2)
C                                       open the text file
      CALL ZTXOPN ('READ', TLUN, TIND, INLIST, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN TEXT FILE'
         GO TO 999
         END IF
      NGAUSS = 0
 100  CALL ZTXIO ('READ', TLUN, TIND, LINE, IRET)
      IF ((IRET.EQ.0) .AND. (NGAUSS.LT.MAXGAU)) THEN
         LLIM = JTRIM (LINE)
C                                       blanks, comments
         IF (LLIM.LE.0) GO TO 100
         IF (LINE(1:1).EQ.'#') GO TO 100
C                                       parse
C                                       peak optical depth
         LP = 1
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            GO TO 100
         ELSE
            NGAUSS = NGAUSS + 1
            PTAU(NGAUSS) = X
            END IF
C                                       channel
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            PCHAN(NGAUSS) = X
            END IF
C                                       width
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            PWID(NGAUSS) = X
            END IF
C                                      R-L separation
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) THEN
            NGAUSS = NGAUSS - 1
            GO TO 100
         ELSE
            PSEP(NGAUSS) = X
            END IF
         GO TO 100
C                                       real error
      ELSE IF ((IRET.GT.0) .AND. (IRET.NE.2)) THEN
         WRITE (MSGTXT,1000) IRET, 'READING TEXT FILE'
         GO TO 999
C                                       EOF
      ELSE
         CALL ZTXCLS (TLUN, TIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSING TEXT FILE'
            GO TO 999
            END IF
         END IF
C                                       MSGWRT left to caller
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('READIT ERROR',I4,' ON ',A)
      END
      SUBROUTINE MODABD (NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   MODABD read in the input images one plane at a time, adds the model
C   appropriate to that plane, and then writes out the plane
C   Input:
C      NX      I      Number X pixels
C      NY      I      Nu,ber Y pixels
C   Output:
C      IMAGE   R(*)   Adequate memory for 2 planes
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      IMAGE(NX,NY,3)
C
      CHARACTER IFILE*48
      INTEGER   LUNO(2), NYI, NXI, WINI(4), NXO, NYO, WINO(4), BOO, IX,
     *   LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I3, I4, I5, I6, I7, IY,
     *   IPOS(7), CORN(7), BOTEMP, LIMO, OBIND, INDO(2), LIM1, FRAX, J
      REAL      OUTMAX(2), OUTMIN(2)
      LOGICAL   T, F, BLNKD(2)
      INCLUDE 'MODAB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNO /18,19/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL MAKONT (NX, NY, IMAGE(1,1,3), APARM)
C                                       loop over polarization
      CALL COPY (256, CATNEW(1,1), CATBLK)
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), FRAX, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT FIND FREQ AXIS'
         GO TO 990
         END IF
      DO 10 J = 1,2
         CALL ZPHFIL ('MA', DISKO(J), NEWCNO(J), 1, IFILE, IRET)
         CALL ZOPEN (LUNO(J), INDO(J), DISKO(J), IFILE, T, T, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUPUT', J
            GO TO 990
            END IF
10       CONTINUE
C                                       Setup for I/O
      J = 1
      NXI = CATNEW(KINAX,J)
      NYI = CATNEW(KINAX+1,J)
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINI(1) = 1
      WINI(2) = 1
      WINI(3) = NXO
      WINI(4) = NYO
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      OUTMAX(1) = -1.0E30
      OUTMIN(1) = 1.0E30
      OUTMAX(2) = -1.0E30
      OUTMIN(2) = 1.0E30
      BLNKD(1) = F
      BLNKD(2) = F
C                                       Setup for looping
      LIM1 = NXO
      LIM2 = NYO
      LIM3 = CATNEW(KINAX+2,1)
      LIM4 = 1
      LIM5 = 1
      LIM6 = 1
      LIM7 = 1
      CORN(7) = 1
      LIMO = CATBLK(KINAX) - 1
C                                       Loop
      IPOS(1) = WINI(1)
      DO 90 I7 = 1,LIM7
         IPOS(7) = I7
         CORN(7) = I7
         DO 85 I6 = 1,LIM6
            IPOS(6) = I6
            CORN(6) = I6
            DO 80 I5 = 1,LIM5
               IPOS(5) = I5
               CORN(5) = I5
               DO 75 I4 = 1,LIM4
                  IPOS(4) = I4
                  CORN(4) = I4
                  DO 70 I3 = 1,LIM3
                     IPOS(3) = I3
                     CORN(3) = I3
C                                       Init plane
                     IY = 2 * NX * NY
                     CALL RFILL (IY, 0.0, IMAGE)
                     CALL THEMOD (I3, NX, NY, IMAGE)
                     DO 50 J = 1,2
                        DO 35 IY = 1,NYO
                           DO 30 IX = 1,NXO
                              IF (IMAGE(IX,IY,J).EQ.FBLANK) THEN
                                 BLNKD(J)  = .TRUE.
                              ELSE
                                 OUTMAX(J) = MAX (OUTMAX(J),
     *                              IMAGE(IX,IY,J))
                                 OUTMIN(J) = MIN (OUTMIN(J),
     *                              IMAGE(IX,IY,J))
                                 END IF
 30                           CONTINUE
 35                        CONTINUE
C                                       Init output file.
                        CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX),
     *                     CORN(3), BOTEMP, IRET)
                        BOO = BOTEMP + 1
                        CALL MINIT ('WRIT', LUNO(J), INDO(J), NXO, NYO,
     *                     WINO, BUFF, JBUFSZ, BOO, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT', J
                           GO TO 990
                           END IF
                        DO 40 IY = 1,LIM2
                           IPOS(2) = IY
C                                       Write.
                           CALL MDISK ('WRIT', LUNO(J), INDO(J), BUFF,
     *                        OBIND, IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT',
     *                           J
                              GO TO 990
                              END IF
                           CALL RCOPY (NX, IMAGE(1,IY,J), BUFF(OBIND))
 40                        CONTINUE
C                                       Flush buffer.
                        CALL MDISK ('FINI', LUNO(J), INDO(J), BUFF,
     *                     OBIND, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET, 'FINISH OUTPUT', J
                           GO TO 990
                           END IF
 50                     CONTINUE
 70                  CONTINUE
 75               CONTINUE
 80            CONTINUE
 85         CONTINUE
 90      CONTINUE
C                                       Mark blanking in CATBLK.
      DO 100 J = 1,2
         CALL COPY (256, CATNEW(1,J), CATBLK)
         CATR(KRBLK) = 0.0
         IF (BLNKD(J)) CATR(KRBLK) = FBLANK
         CATR(KRDMX) = OUTMAX(J)
         CATR(KRDMN) = OUTMIN(J)
         CALL COPY (256, CATBLK, CATNEW(1,J))
         CALL CATIO ('UPDT', DISKO(J), NEWCNO(J), CATBLK, 'REST',
     *      SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'UPDATING HEADER OF', J
            GO TO 990
            END IF
C                                       Close images
         CALL ZCLOSE (LUNO(J), INDO(J), IRET)
 100     CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODABD: ERROR',I3,' ON ',A,' FILE',I2)
      END
      SUBROUTINE MAKONT (NX, NY, IMAGE, APARM)
C-----------------------------------------------------------------------
C   Constructs the base continuum image
C   Inputs:
C      NX      I       X pixels
C      NY      I       Y pixels
C   In/out:
C      APARM   R(10)   Model parameters - defaults filled in
C   Output
C      IMAGE   R(*)    Image
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      REAL      IMAGE(NX,NY), APARM(10)
C
      INTEGER   IX, IY
      REAL      CPHI, SPHI, X, Y, XX, YY, R
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IX = NX * NY
      CALL RFILL (IX, 0.0, IMAGE)
      CPHI = COS (APARM(9) * DG2RAD)
      SPHI = SIN (APARM(9) * DG2RAD)
      IF (APARM(8).LT.0.5) APARM(8) = 3
      IF (APARM(7).LT.0.5) APARM(7) = 3
      IF ((APARM(6).LE.1.0) .OR. (APARM(6).GE.NY)) APARM(6) = NY/2 + 1
      IF ((APARM(5).LE.1.0) .OR. (APARM(5).GE.NX)) APARM(5) = NX/2
      IF (APARM(4).LE.0.0) APARM(4) = 1.0

      DO 20 IY = 1,NY
         DO 10 IX = 1,NX
            X = IX - APARM(5)
            Y = IY - APARM(6)
            XX = (Y * CPHI - X * SPHI) / APARM(7)
            YY = (X * CPHI + Y * SPHI) / APARM(8)
            R = XX**2 + YY**2
            IF (R.LT.4.) IMAGE(IX,IY) = APARM(4) * EXP (-2.772588722*R)
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE THEMOD (J, NX, NY, IMAGE)
C-----------------------------------------------------------------------
C   Apply model to one plane of the image
C   Inputs:
C      J        I      spectral channel
C      NX       I      Number X pixels
C      NY       I      Number Y pixels
C   In/out
C      IMAGE    R(NX,*)   image
C-----------------------------------------------------------------------
      INTEGER   J, NX, NY
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'MODAB.INC'
      REAL      ANOISE
      INTEGER   IX, IY, K
      DOUBLE PRECISION HALFAC, R, RTAU, LTAU, RSUM, LSUM
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA HALFAC /2.77258872D0/
C-----------------------------------------------------------------------
      RTAU = 0.0D0
      LTAU = 0.0D0
      DO 20 K = 1,NGAUSS
         R = (J - PCHAN(K) + PSEP(K) / 2.0) / PWID(K)
         R = HALFAC * R * R
         IF (R.LT.15.0) RTAU = RTAU + PTAU(K) * EXP (-R)
         R = (J - PCHAN(K) - PSEP(K) / 2.0) / PWID(K)
         R = HALFAC * R * R
         IF (R.LT.15.0) LTAU = LTAU + PTAU(K) * EXP (-R)
 20      CONTINUE
      IF (OPTYPE.NE.'EMIT') THEN
         RTAU = EXP (-RTAU)
         LTAU = EXP (-LTAU)
         END IF
C                                       Loop over image and apply model
C                                       Point
      DO 130 IY = 1,NY
         DO 120 IX = 1,NX
            RSUM = IMAGE(IX,IY,3) * RTAU
            LSUM = IMAGE(IX,IY,3) * LTAU
C                                       Add random noise?
            IF (FLUX.GT.0.0) THEN
               CALL NOISE (ANOISE)
               RSUM = RSUM + ANOISE * FLUX
               CALL NOISE (ANOISE)
               LSUM = LSUM + ANOISE * FLUX
               END IF
C                                       convert to I/V
            IMAGE(IX,IY,1) = (RSUM + LSUM) / 2.0
            IMAGE(IX,IY,2) = (RSUM - LSUM) / 2.0 +
     *         APARM(10) * IMAGE(IX,IY,1)
 120        CONTINUE
 130     CONTINUE
C
 999  RETURN
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
      SUBROUTINE MODABH
C-----------------------------------------------------------------------
C   MODABH copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72, NOTTYP*2, CODES(6)*4
      INTEGER   LUN1, LUN2, IERR, I, NCOMP, J, TIME(3), DATE(3)
      INCLUDE 'MODAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NOTTYP /'CC'/
      DATA CODES /'POIN','GAUS','DISK','RECT','SPHE','EXPD'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
      DO 100 J = 1,2
         CALL COPY (256, CATNEW(1,J), CATBLK)
C                                       need new HI file
C                                       Create/open hist. file.
         CALL HICREA (LUN2, DISKO(J), NEWCNO(J), CATBLK, BUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CREATING HI FILE', J
            CALL MSGWRT (6)
            GO TO 20
            END IF
C                                       Get current date/time.
         CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
         WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       outname
         CALL HENCOO (TSKNAM, NAMOUT, CLASO(J), SEQO(J), DISKO(J), LUN2,
     *      BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       Components
         NCOMP = NGAUSS
         NCOMP = MAX (1, MIN (9, NCOMP))
         IF (NGAUSS.GT.NCOMP) THEN
            MSGTXT = 'ONLY FIRST 9 LISTED IN HI'
            IF (J.EQ.1) CALL MSGWRT (2)
            END IF
         WRITE (HILINE,2003) TSKNAM, NGAUSS
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20

         DO 10 I = 1,NCOMP
C                                       PTAU
            WRITE (HILINE,2004) TSKNAM, I, PTAU(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       channel
            WRITE (HILINE,2005) TSKNAM, I, PCHAN(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       width
            WRITE (HILINE,2006) TSKNAM, I, PWID(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
C                                       separation
            WRITE (HILINE,2007) TSKNAM, I, PSEP(I)
            CALL HIADD (LUN2, HILINE, BUFF, IERR)
            IF (IERR.NE.0) GO TO 20
 10         CONTINUE
C                                       continuum
         WRITE (HILINE,2011) TSKNAM, APARM(4)
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2012) TSKNAM, APARM(5), APARM(6)
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2013) TSKNAM, APARM(7), APARM(8), APARM(9)
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2015) TSKNAM, APARM(10)
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       FLUX
         WRITE (HILINE,2020) TSKNAM, FLUX
         CALL HIADD (LUN2, HILINE, BUFF, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       Close HI file
 20      CALL HICLOS (LUN2, .TRUE., BUFF, IERR)
C                                        Update CATBLK.
         CALL CATIO ('UPDT', DISKO(J), NEWCNO(J), CATBLK, 'REST',
     *      BUFF, IERR)
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODABH: ERROR',I3,' ON ',A,' FILE',I2)
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 2003 FORMAT (A6,'NGAUSS=',I6,'    total number components')
 2004 FORMAT (A6,'PTAU(',I4,')  =',1PE12.4,12X,'/ optDepth')
 2005 FORMAT (A6,'PCHAN(',I4,')  =',F8.2,7X,'/ channels')
 2006 FORMAT (A6,'PWIDTH(',I4,') =',F7.3,8X,'/ channels')
 2007 FORMAT (A6,'PSEP(',I4,') =',F9.3,8X,'/ channels')
 2011 FORMAT (A6,'CFLUX =',1pE12.4,5X,'/ Continuum peak Jy/beam')
 2012 FORMAT (A6,'CPOS =',F7.1,',',F7.1,3X,'/ position pixels')
 2013 FORMAT (A6,'CBEAM =',F5.1,',',F5.1,',',F7.1,2X,
     *   '/ Bmaj, Bmin pixels, Bpa deg')
 2015 FORMAT (A6,'LEAKAGE=',F7.4,4X,'/ I leakage gain into V')
 2020 FORMAT (A6,'FLUX = ',1PE12.4,5X,'/ noise added')
      END
